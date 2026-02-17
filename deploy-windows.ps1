# ==============================================================================
# EcoNeTool - Windows Deployment Script for laguna.ku.lt
# ==============================================================================
# This PowerShell script deploys EcoNeTool from Windows to the Shiny Server
#
# Usage:
#   .\deploy-windows.ps1 [-DryRun] [-NoBackup] [-Force] [-Verbose]
#
# Prerequisites:
#   - Windows 10/11 with OpenSSH client (built-in)
#   - SSH key configured for razinka@laguna.ku.lt
#   - Or password authentication enabled
#
# Setup SSH key (recommended):
#   ssh-keygen -t ed25519
#   ssh-copy-id razinka@laguna.ku.lt
#
# ==============================================================================

param(
    [switch]$DryRun,
    [switch]$NoBackup,
    [switch]$Force,
    [switch]$RestartServer,
    [switch]$UseSCP,        # Skip tar, use SCP directly (more reliable)
    [switch]$NoSudo,        # Deploy to home dir staging area (no sudo required)
    [switch]$SkipData,      # Skip data/ directory (faster deployment)
    [string]$User = "razinka",
    [string]$Server = "laguna.ku.lt",
    [int]$Port = 22
)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

$ErrorActionPreference = "Stop"

# Server paths
$SHINY_SERVER_ROOT = "/srv/shiny-server"
$APP_NAME = "EcoNeTool"
$STAGING_PATH = "/home/$User/EcoNeTool_staging"

# Use staging path if NoSudo mode
if ($NoSudo) {
    $APP_DEPLOY_PATH = $STAGING_PATH
    $BACKUP_DIR = "/home/$User/backups"
} else {
    $APP_DEPLOY_PATH = "$SHINY_SERVER_ROOT/$APP_NAME"
    $BACKUP_DIR = "$SHINY_SERVER_ROOT/backups"
}

# Local paths
$PROJECT_ROOT = Split-Path -Parent $MyInvocation.MyCommand.Path
$LOG_DIR = Join-Path $PROJECT_ROOT "deployment_logs"
$TIMESTAMP = Get-Date -Format "yyyyMMdd_HHmmss"
$LOG_FILE = Join-Path $LOG_DIR "deploy_$TIMESTAMP.log"

# SSH connection string
$SSH_TARGET = "$User@$Server"
# Minimal SSH options - rely on user's ~/.ssh/config for other settings
$SSH_OPTS = @()
if ($Port -ne 22) {
    $SSH_OPTS += @("-p", $Port)
}

# Files and directories to deploy
$DEPLOY_ITEMS = @(
    "app.R",
    "run_app.R",
    "VERSION",
    "R/",
    "www/",
    "examples/",
    "metawebs/",
    "data/",
    "config/"
)

# Patterns to exclude
$EXCLUDE_PATTERNS = @(
    "*.Rproj",
    ".Rproj.user",
    "*.Rhistory",
    ".RData",
    ".git",
    ".gitignore",
    ".gitattributes",
    ".DS_Store",
    ".claude",
    "*backup*",
    "*test*.R",
    "deploy*.ps1",
    "deploy*.sh",
    "deploy*.bat",
    "deployment/",
    "Script.R",
    "create_example_datasets.R",
    "*.ewemdb",
    "*.eweaccdb",
    "*.accdb",
    "*.xml",
    "*.doc",
    "*.zip",
    "cache/",
    "output/",
    "docs/",
    "tests/",
    "*.md",
    "*.log",
    "archive/",
    "data_conversion/"
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

function Write-Log {
    param([string]$Message, [string]$Level = "INFO")
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"

    # Ensure log directory exists
    if (-not (Test-Path $LOG_DIR)) {
        New-Item -ItemType Directory -Path $LOG_DIR -Force | Out-Null
    }

    Add-Content -Path $LOG_FILE -Value $logMessage -ErrorAction SilentlyContinue

    switch ($Level) {
        "ERROR"   { Write-Host "X $Message" -ForegroundColor Red }
        "WARNING" { Write-Host "! $Message" -ForegroundColor Yellow }
        "SUCCESS" { Write-Host "v $Message" -ForegroundColor Green }
        default   { Write-Host "  $Message" -ForegroundColor Cyan }
    }
}

function Test-SSHConnection {
    Write-Log "Testing SSH connection to $SSH_TARGET..."

    try {
        # Use a simple job-based approach with timeout
        Write-Host "  DEBUG: Testing with: ssh -o BatchMode=yes -o ConnectTimeout=10 $SSH_TARGET echo CONNECTION_OK" -ForegroundColor Gray

        $job = Start-Job -ScriptBlock {
            param($target)
            & ssh -o BatchMode=yes -o ConnectTimeout=10 $target "echo CONNECTION_OK" 2>&1
        } -ArgumentList $SSH_TARGET

        Write-Host "  DEBUG: Waiting for SSH job (max 20 seconds)..." -ForegroundColor Gray

        $completed = Wait-Job $job -Timeout 20

        if ($null -eq $completed) {
            Write-Host "  DEBUG: SSH job timed out, stopping..." -ForegroundColor Yellow
            Stop-Job $job
            Remove-Job $job -Force
            Write-Log "SSH connection timed out" "ERROR"
            return $false
        }

        $result = Receive-Job $job
        $state = $job.State
        Remove-Job $job -Force

        Write-Host "  DEBUG: Job state: $state" -ForegroundColor Gray
        Write-Host "  DEBUG: Result: $result" -ForegroundColor Gray

        if ($result -match "CONNECTION_OK") {
            Write-Log "SSH connection successful" "SUCCESS"
            return $true
        } else {
            Write-Log "SSH connection failed: $result" "ERROR"
            return $false
        }
    } catch {
        Write-Log "SSH connection error: $_" "ERROR"
        Write-Host "  DEBUG: Exception: $_" -ForegroundColor Red
        return $false
    }
}

function Invoke-RemoteCommand {
    param([string]$Command, [switch]$IgnoreError)

    if ($DryRun) {
        Write-Log "[DRY-RUN] Would execute: $Command" "INFO"
        return ""
    }

    # Use job-based approach to avoid hanging
    $job = Start-Job -ScriptBlock {
        param($target, $cmd)
        & ssh -o BatchMode=yes -o ConnectTimeout=30 $target $cmd 2>&1
    } -ArgumentList $SSH_TARGET, $Command

    $completed = Wait-Job $job -Timeout 60

    if ($null -eq $completed) {
        Stop-Job $job
        Remove-Job $job -Force
        if (-not $IgnoreError) {
            throw "Remote command timed out: $Command"
        }
        return ""
    }

    $result = Receive-Job $job
    Remove-Job $job -Force

    # Check for SSH errors in output
    if ($result -match "Permission denied|Connection refused|Host key verification failed") {
        if (-not $IgnoreError) {
            Write-Log "Remote command failed: $Command" "ERROR"
            Write-Log "Output: $result" "ERROR"
            throw "Remote command failed: $result"
        }
    }

    return $result
}

function Copy-ToRemote {
    param(
        [string]$LocalPath,
        [string]$RemotePath,
        [switch]$Recursive
    )

    if ($DryRun) {
        Write-Log "[DRY-RUN] Would copy: $LocalPath -> $RemotePath" "INFO"
        return
    }

    # CRITICAL: Ensure remote path uses forward slashes (Unix style)
    # PowerShell can convert / to \ in certain contexts
    $RemotePathUnix = $RemotePath -replace '\\', '/'

    Write-Host "  Copying: $LocalPath -> $RemotePathUnix" -ForegroundColor Gray

    # Build the full SCP destination string BEFORE passing to job
    # This prevents PowerShell from mangling the path
    $scpDest = "${SSH_TARGET}:${RemotePathUnix}"
    Write-Host "    SCP dest: $scpDest" -ForegroundColor DarkGray

    $job = Start-Job -ScriptBlock {
        param($src, $dest, $rec, $prt)
        $scpArgs = @("-o", "BatchMode=yes")
        if ($rec) { $scpArgs += "-r" }
        if ($prt) { $scpArgs += @("-P", $prt) }
        $scpArgs += $src
        $scpArgs += $dest
        Write-Host "SCP args: $($scpArgs -join ' ')"
        & scp @scpArgs 2>&1
    } -ArgumentList $LocalPath, $scpDest, $Recursive.IsPresent, $(if ($Port -ne 22) { $Port } else { $null })

    $completed = Wait-Job $job -Timeout 1800  # 30 min timeout for large files

    if ($null -eq $completed) {
        Stop-Job $job
        Remove-Job $job -Force
        throw "SCP timed out for $LocalPath (30 min limit reached)"
    }

    $result = Receive-Job $job
    $state = $job.State
    Remove-Job $job -Force

    if ($result -match "Permission denied|No such file|Connection refused") {
        throw "SCP failed for $LocalPath : $result"
    }

    # Show result for debugging
    if ($result) {
        Write-Host "    Result: $result" -ForegroundColor DarkGray
    }
}

function Get-FilesToDeploy {
    $files = @()

    foreach ($item in $DEPLOY_ITEMS) {
        # Skip data/ directory if -SkipData is specified
        if ($SkipData -and ($item -eq "data/" -or $item -like "data/*")) {
            Write-Log "Skipping data directory (-SkipData specified)" "INFO"
            continue
        }

        $fullPath = Join-Path $PROJECT_ROOT $item

        if (Test-Path $fullPath) {
            # Check if it matches any exclude pattern
            $excluded = $false
            foreach ($pattern in $EXCLUDE_PATTERNS) {
                if ($item -like $pattern -or $fullPath -like "*$pattern*") {
                    $excluded = $true
                    break
                }
            }

            if (-not $excluded) {
                $files += @{
                    LocalPath = $fullPath
                    RelativePath = $item
                    IsDirectory = (Get-Item $fullPath).PSIsContainer
                }
            }
        } else {
            Write-Log "Item not found (skipping): $item" "WARNING"
        }
    }

    return $files
}

# ==============================================================================
# MAIN DEPLOYMENT FUNCTIONS
# ==============================================================================

function New-RemoteBackup {
    if ($NoBackup) {
        Write-Log "Skipping backup (--NoBackup specified)" "WARNING"
        return
    }

    Write-Log "Creating backup on server..."

    $backupName = "${APP_NAME}_backup_$TIMESTAMP"
    $backupPath = "$BACKUP_DIR/$backupName"
    $sudoPrefix = if ($NoSudo) { "" } else { "sudo " }

    # Create backup directory
    Invoke-RemoteCommand "${sudoPrefix}mkdir -p $BACKUP_DIR"

    # Check if app exists
    $appExists = Invoke-RemoteCommand "test -d $APP_DEPLOY_PATH && echo 'yes' || echo 'no'" -IgnoreError

    if ($appExists.Trim() -eq "yes") {
        Invoke-RemoteCommand "${sudoPrefix}cp -r $APP_DEPLOY_PATH $backupPath"
        Write-Log "Backup created: $backupPath" "SUCCESS"

        # Keep only last 5 backups
        Invoke-RemoteCommand "cd $BACKUP_DIR && ls -t | tail -n +6 | xargs -r ${sudoPrefix}rm -rf" -IgnoreError
    } else {
        Write-Log "No existing deployment to backup" "INFO"
    }
}

function Deploy-Application {
    Write-Log "Starting deployment to $APP_DEPLOY_PATH..."

    $sudoPrefix = if ($NoSudo) { "" } else { "sudo " }

    # Ensure deploy directory exists
    Invoke-RemoteCommand "${sudoPrefix}mkdir -p $APP_DEPLOY_PATH"

    # Get files to deploy
    $files = Get-FilesToDeploy
    Write-Log "Found $($files.Count) items to deploy"

    # Create a temporary directory for staging
    $tempDir = Join-Path $env:TEMP "econetool_deploy_$TIMESTAMP"
    New-Item -ItemType Directory -Path $tempDir -Force | Out-Null

    try {
        # Helper function to check if path should be excluded
        function Test-ShouldExclude {
            param([string]$Path)
            $name = Split-Path -Leaf $Path
            $fullPath = $Path

            foreach ($pattern in $EXCLUDE_PATTERNS) {
                # Check filename against pattern
                if ($name -like $pattern) { return $true }
                # Check if any path component matches (for .git, .Rproj.user, etc.)
                if ($fullPath -like "*\$pattern\*" -or $fullPath -like "*\$pattern") { return $true }
                if ($fullPath -like "*/$pattern/*" -or $fullPath -like "*/$pattern") { return $true }
            }
            return $false
        }

        # Copy files to staging, filtering out excluded items
        foreach ($file in $files) {
            $destPath = Join-Path $tempDir $file.RelativePath
            $destDir = Split-Path -Parent $destPath

            if (-not (Test-Path $destDir)) {
                New-Item -ItemType Directory -Path $destDir -Force | Out-Null
            }

            if ($file.IsDirectory) {
                # For directories, recursively copy with proper exclusion
                $sourceDir = $file.LocalPath
                New-Item -ItemType Directory -Path $destPath -Force | Out-Null

                # Get all items recursively and filter
                $allItems = Get-ChildItem -Path $sourceDir -Recurse -Force -ErrorAction SilentlyContinue
                $copiedCount = 0
                $skippedCount = 0

                foreach ($item in $allItems) {
                    $relativePath = $item.FullName.Substring($sourceDir.Length).TrimStart('\', '/')
                    $itemDest = Join-Path $destPath $relativePath

                    # Check if this item or any parent should be excluded
                    if (Test-ShouldExclude $item.FullName) {
                        $skippedCount++
                        continue
                    }

                    if ($item.PSIsContainer) {
                        # Create directory
                        if (-not (Test-Path $itemDest)) {
                            New-Item -ItemType Directory -Path $itemDest -Force | Out-Null
                        }
                    } else {
                        # Copy file
                        $itemDestDir = Split-Path -Parent $itemDest
                        if (-not (Test-Path $itemDestDir)) {
                            New-Item -ItemType Directory -Path $itemDestDir -Force | Out-Null
                        }
                        Copy-Item -Path $item.FullName -Destination $itemDest -Force
                        $copiedCount++
                    }
                }

                Write-Log "Staged: $($file.RelativePath) ($copiedCount files, $skippedCount excluded)"
            } else {
                Copy-Item -Path $file.LocalPath -Destination $destPath -Force
                Write-Log "Staged: $($file.RelativePath)"
            }
        }

        # Upload staged files
        Write-Log "Uploading files to server..."

        # Create tar archive for faster transfer
        $tarFile = Join-Path $env:TEMP "econetool_deploy.tar.gz"

        # Helper function to convert Windows path to Git Bash path
        function Convert-ToGitBashPath {
            param([string]$WinPath)
            # C:\Users\... -> /c/Users/...
            if ($WinPath -match '^([A-Za-z]):(.*)$') {
                $drive = $Matches[1].ToLower()
                $rest = $Matches[2].Replace('\', '/')
                return "/$drive$rest"
            }
            return $WinPath.Replace('\', '/')
        }

        # Use tar if available (Git Bash), otherwise use scp directly
        $gitBash = "C:\Program Files\Git\bin\bash.exe"
        $tarSuccess = $false

        if ($UseSCP) {
            Write-Log "Using SCP mode (-UseSCP specified)" "INFO"
        } elseif (Test-Path $gitBash) {
            # Convert Windows paths to Git Bash format
            $bashTempDir = Convert-ToGitBashPath $tempDir
            $bashTarFile = Convert-ToGitBashPath $tarFile

            Write-Host "  DEBUG: tempDir = $tempDir" -ForegroundColor Gray
            Write-Host "  DEBUG: bashTempDir = $bashTempDir" -ForegroundColor Gray
            Write-Host "  DEBUG: bashTarFile = $bashTarFile" -ForegroundColor Gray

            # Use tar via Git Bash for faster transfer
            $tarCommand = "cd '$bashTempDir' && tar -czf '$bashTarFile' ."
            Write-Host "  DEBUG: tar command = $tarCommand" -ForegroundColor Gray

            & $gitBash -c $tarCommand 2>&1 | ForEach-Object { Write-Host "  tar: $_" -ForegroundColor Gray }

            if (Test-Path $tarFile) {
                $tarSuccess = $true
                Write-Log "Created archive: $tarFile"

                if (-not $DryRun) {
                    # Upload tar file
                    Copy-ToRemote -LocalPath $tarFile -RemotePath "/tmp/econetool_deploy.tar.gz"

                    # Extract on server
                    Invoke-RemoteCommand "${sudoPrefix}rm -rf $APP_DEPLOY_PATH/* && ${sudoPrefix}tar -xzf /tmp/econetool_deploy.tar.gz -C $APP_DEPLOY_PATH && rm /tmp/econetool_deploy.tar.gz"
                }

                Remove-Item $tarFile -Force -ErrorAction SilentlyContinue
            } else {
                Write-Log "Tar archive not created, falling back to SCP" "WARNING"
            }
        }

        if (-not $tarSuccess) {
            # Fallback: use scp for each directory
            if (-not (Test-Path $gitBash)) {
                Write-Log "Git Bash not found, using direct SCP (slower)..." "WARNING"
            }

            foreach ($file in $files) {
                $localPath = Join-Path $tempDir $file.RelativePath
                # Ensure remote paths use forward slashes
                $relPath = $file.RelativePath -replace '\\', '/'
                $relPath = $relPath.TrimEnd('/')
                $remotePath = "$APP_DEPLOY_PATH/$relPath"

                if (Test-Path $localPath) {
                    if ($file.IsDirectory) {
                        # For directories, copy to parent and let scp create the dir
                        Invoke-RemoteCommand "${sudoPrefix}mkdir -p $APP_DEPLOY_PATH" -IgnoreError
                        Copy-ToRemote -LocalPath $localPath -RemotePath $APP_DEPLOY_PATH -Recursive
                    } else {
                        # For files, ensure parent dir exists
                        $remoteDir = $remotePath -replace '/[^/]+$', ''
                        Invoke-RemoteCommand "${sudoPrefix}mkdir -p $remoteDir" -IgnoreError
                        Copy-ToRemote -LocalPath $localPath -RemotePath $remotePath
                    }
                }
            }
        }

        Write-Log "Files uploaded successfully" "SUCCESS"

    } finally {
        # Cleanup staging directory
        Remove-Item -Path $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}

function Set-RemotePermissions {
    Write-Log "Setting permissions on server..."

    if ($NoSudo) {
        # In NoSudo mode, just set basic permissions (no chown to shiny)
        Invoke-RemoteCommand "chmod -R 755 $APP_DEPLOY_PATH"
        Invoke-RemoteCommand "chmod 644 $APP_DEPLOY_PATH/app.R"
        Write-Log "Permissions set (user-level only)" "SUCCESS"
    } else {
        Invoke-RemoteCommand "sudo chown -R shiny:shiny $APP_DEPLOY_PATH"
        Invoke-RemoteCommand "sudo chmod -R 755 $APP_DEPLOY_PATH"
        Invoke-RemoteCommand "sudo chmod 644 $APP_DEPLOY_PATH/app.R"
        Write-Log "Permissions set" "SUCCESS"
    }
}

function Restart-ShinyServer {
    if (-not $RestartServer) {
        Write-Log "Skipping server restart (use -RestartServer to restart)" "INFO"
        return
    }

    if ($NoSudo) {
        Write-Log "Cannot restart server in NoSudo mode - please restart manually" "WARNING"
        return
    }

    Write-Log "Restarting Shiny Server..."

    Invoke-RemoteCommand "sudo systemctl restart shiny-server"
    Start-Sleep -Seconds 3

    $status = Invoke-RemoteCommand "sudo systemctl is-active shiny-server" -IgnoreError
    if ($status.Trim() -eq "active") {
        Write-Log "Shiny Server restarted successfully" "SUCCESS"
    } else {
        Write-Log "Shiny Server may not have restarted properly" "WARNING"
    }
}

function Test-Deployment {
    Write-Log "Verifying deployment..."

    # In dry-run mode, skip actual verification
    if ($DryRun) {
        Write-Log "[DRY-RUN] Skipping verification (nothing was deployed)" "INFO"
        return $true
    }

    # Check if app.R exists
    $appExists = Invoke-RemoteCommand "test -f $APP_DEPLOY_PATH/app.R && echo 'yes' || echo 'no'" -IgnoreError
    if ($appExists.Trim() -ne "yes") {
        Write-Log "Deployment verification FAILED: app.R not found" "ERROR"
        return $false
    }

    # Check if R directory exists
    $rDirExists = Invoke-RemoteCommand "test -d $APP_DEPLOY_PATH/R && echo 'yes' || echo 'no'" -IgnoreError
    if ($rDirExists.Trim() -ne "yes") {
        Write-Log "Deployment verification FAILED: R/ directory not found" "ERROR"
        return $false
    }

    # Count files
    $fileCount = Invoke-RemoteCommand "find $APP_DEPLOY_PATH -type f | wc -l" -IgnoreError
    Write-Log "Deployed $($fileCount.Trim()) files" "SUCCESS"

    # Check Shiny Server status (only if we have sudo)
    if (-not $NoSudo) {
        $serverStatus = Invoke-RemoteCommand "sudo systemctl is-active shiny-server" -IgnoreError
        Write-Log "Shiny Server status: $($serverStatus.Trim())"
    }

    return $true
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

Write-Host ""
Write-Host "================================================================================" -ForegroundColor Blue
Write-Host " EcoNeTool - Windows Deployment to $Server" -ForegroundColor Blue
Write-Host "================================================================================" -ForegroundColor Blue
Write-Host ""

if ($DryRun) {
    Write-Host "[DRY RUN MODE - No changes will be made]" -ForegroundColor Yellow
    Write-Host ""
}

Write-Log "Deployment started"
Write-Log "Target: $SSH_TARGET"
Write-Log "Deploy path: $APP_DEPLOY_PATH"

# Show data directory size if it exists
$dataDir = Join-Path $PROJECT_ROOT "data"
if (Test-Path $dataDir) {
    $dataSize = (Get-ChildItem -Path $dataDir -Recurse -File | Measure-Object -Property Length -Sum).Sum / 1MB
    $dataSizeMB = [math]::Round($dataSize, 2)
    if ($SkipData) {
        Write-Log "Data directory size: $dataSizeMB MB (will be SKIPPED)" "INFO"
    } else {
        Write-Log "Data directory size: $dataSizeMB MB (will be deployed)" "INFO"
        if ($dataSizeMB -gt 50) {
            Write-Host "  TIP: Use -SkipData for faster deployment if data hasn't changed" -ForegroundColor Yellow
        }
    }
}

# Check prerequisites
if (-not (Get-Command ssh -ErrorAction SilentlyContinue)) {
    Write-Log "SSH not found. Please ensure OpenSSH client is installed." "ERROR"
    Write-Log "Run: Add-WindowsCapability -Online -Name OpenSSH.Client*" "INFO"
    exit 1
}

# Test connection
if (-not (Test-SSHConnection)) {
    if (-not $Force) {
        Write-Log "Cannot connect to server. Use -Force to continue anyway." "ERROR"
        exit 1
    }
    Write-Log "Continuing despite connection failure (-Force specified)" "WARNING"
}

try {
    # Step 1: Backup
    New-RemoteBackup

    # Step 2: Deploy
    Deploy-Application

    # Step 3: Set permissions
    Set-RemotePermissions

    # Step 4: Restart server (if requested)
    Restart-ShinyServer

    # Step 5: Verify
    if (Test-Deployment) {
        Write-Host ""
        Write-Host "================================================================================" -ForegroundColor Green
        Write-Host " DEPLOYMENT SUCCESSFUL" -ForegroundColor Green
        Write-Host "================================================================================" -ForegroundColor Green
        Write-Host ""

        if ($SkipData) {
            Write-Host " NOTE: data/ directory was skipped (-SkipData)" -ForegroundColor Yellow
            Write-Host "       To deploy data separately, re-run without -SkipData" -ForegroundColor Yellow
            Write-Host ""
        }

        if ($NoSudo) {
            Write-Host " Files deployed to: $APP_DEPLOY_PATH" -ForegroundColor Cyan
            Write-Host ""
            Write-Host " NEXT STEPS (run on server via SSH):" -ForegroundColor Yellow
            Write-Host "   ssh $SSH_TARGET" -ForegroundColor Gray
            Write-Host "   sudo rm -rf /srv/shiny-server/$APP_NAME/*" -ForegroundColor Gray
            Write-Host "   sudo cp -r $APP_DEPLOY_PATH/* /srv/shiny-server/$APP_NAME/" -ForegroundColor Gray
            Write-Host "   sudo chown -R shiny:shiny /srv/shiny-server/$APP_NAME" -ForegroundColor Gray
            Write-Host "   sudo systemctl restart shiny-server" -ForegroundColor Gray
            Write-Host ""
        } else {
            Write-Host " Application URL: http://$Server/$APP_NAME/" -ForegroundColor Cyan
        }

        Write-Host " Log file: $LOG_FILE" -ForegroundColor Gray
        Write-Host ""
    } else {
        throw "Deployment verification failed"
    }

} catch {
    Write-Log "Deployment FAILED: $_" "ERROR"
    Write-Host ""
    Write-Host "================================================================================" -ForegroundColor Red
    Write-Host " DEPLOYMENT FAILED" -ForegroundColor Red
    Write-Host "================================================================================" -ForegroundColor Red
    Write-Host ""
    Write-Host " Check log file: $LOG_FILE" -ForegroundColor Gray
    Write-Host ""
    exit 1
}
