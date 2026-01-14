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
    [string]$User = "razinka",
    [string]$Host = "laguna.ku.lt",
    [int]$Port = 22
)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

$ErrorActionPreference = "Stop"

# Server paths
$SHINY_SERVER_ROOT = "/srv/shiny-server"
$APP_NAME = "EcoNeTool"
$APP_DEPLOY_PATH = "$SHINY_SERVER_ROOT/$APP_NAME"
$BACKUP_DIR = "$SHINY_SERVER_ROOT/backups"

# Local paths
$PROJECT_ROOT = Split-Path -Parent $MyInvocation.MyCommand.Path
$LOG_DIR = Join-Path $PROJECT_ROOT "deployment_logs"
$TIMESTAMP = Get-Date -Format "yyyyMMdd_HHmmss"
$LOG_FILE = Join-Path $LOG_DIR "deploy_$TIMESTAMP.log"

# SSH connection string
$SSH_TARGET = "$User@$Host"
$SSH_OPTS = @("-o", "StrictHostKeyChecking=accept-new", "-o", "ConnectTimeout=10")
if ($Port -ne 22) {
    $SSH_OPTS += @("-p", $Port)
}

# Files and directories to deploy
$DEPLOY_ITEMS = @(
    "app.R",
    "run_app.R",
    "R/",
    "www/",
    "examples/BalticFW.Rdata",
    "examples/LTCoast.Rdata",
    "metawebs/",
    "data/"
)

# Patterns to exclude
$EXCLUDE_PATTERNS = @(
    "*.Rproj",
    ".Rproj.user",
    "*.Rhistory",
    ".RData",
    ".git",
    ".gitignore",
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
    "cache/",
    "output/",
    "docs/",
    "tests/",
    "*.md",
    "*.log",
    "EUSeaMap*.zip",
    "archive/"
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
        $result = & ssh $SSH_OPTS $SSH_TARGET "echo 'Connection successful'" 2>&1
        if ($LASTEXITCODE -eq 0) {
            Write-Log "SSH connection successful" "SUCCESS"
            return $true
        } else {
            Write-Log "SSH connection failed: $result" "ERROR"
            return $false
        }
    } catch {
        Write-Log "SSH connection error: $_" "ERROR"
        return $false
    }
}

function Invoke-RemoteCommand {
    param([string]$Command, [switch]$IgnoreError)

    if ($DryRun) {
        Write-Log "[DRY-RUN] Would execute: $Command" "INFO"
        return ""
    }

    $result = & ssh $SSH_OPTS $SSH_TARGET $Command 2>&1
    if ($LASTEXITCODE -ne 0 -and -not $IgnoreError) {
        Write-Log "Remote command failed: $Command" "ERROR"
        Write-Log "Output: $result" "ERROR"
        throw "Remote command failed"
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

    $scpOpts = @("-o", "StrictHostKeyChecking=accept-new")
    if ($Port -ne 22) {
        $scpOpts += @("-P", $Port)
    }
    if ($Recursive) {
        $scpOpts += "-r"
    }

    & scp $scpOpts $LocalPath "${SSH_TARGET}:${RemotePath}" 2>&1
    if ($LASTEXITCODE -ne 0) {
        throw "SCP failed for $LocalPath"
    }
}

function Get-FilesToDeploy {
    $files = @()

    foreach ($item in $DEPLOY_ITEMS) {
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

    # Create backup directory
    Invoke-RemoteCommand "sudo mkdir -p $BACKUP_DIR"

    # Check if app exists
    $appExists = Invoke-RemoteCommand "test -d $APP_DEPLOY_PATH && echo 'yes' || echo 'no'" -IgnoreError

    if ($appExists.Trim() -eq "yes") {
        Invoke-RemoteCommand "sudo cp -r $APP_DEPLOY_PATH $backupPath"
        Write-Log "Backup created: $backupPath" "SUCCESS"

        # Keep only last 5 backups
        Invoke-RemoteCommand "cd $BACKUP_DIR && ls -t | tail -n +6 | xargs -r sudo rm -rf" -IgnoreError
    } else {
        Write-Log "No existing deployment to backup" "INFO"
    }
}

function Deploy-Application {
    Write-Log "Starting deployment to $APP_DEPLOY_PATH..."

    # Ensure deploy directory exists
    Invoke-RemoteCommand "sudo mkdir -p $APP_DEPLOY_PATH"

    # Get files to deploy
    $files = Get-FilesToDeploy
    Write-Log "Found $($files.Count) items to deploy"

    # Create a temporary directory for staging
    $tempDir = Join-Path $env:TEMP "econetool_deploy_$TIMESTAMP"
    New-Item -ItemType Directory -Path $tempDir -Force | Out-Null

    try {
        # Copy files to staging, filtering out excluded items
        foreach ($file in $files) {
            $destPath = Join-Path $tempDir $file.RelativePath
            $destDir = Split-Path -Parent $destPath

            if (-not (Test-Path $destDir)) {
                New-Item -ItemType Directory -Path $destDir -Force | Out-Null
            }

            if ($file.IsDirectory) {
                # For directories, copy contents excluding patterns
                $sourceDir = $file.LocalPath
                Copy-Item -Path "$sourceDir\*" -Destination $destPath -Recurse -Force -Exclude $EXCLUDE_PATTERNS -ErrorAction SilentlyContinue
            } else {
                Copy-Item -Path $file.LocalPath -Destination $destPath -Force
            }

            Write-Log "Staged: $($file.RelativePath)"
        }

        # Upload staged files
        Write-Log "Uploading files to server..."

        # Create tar archive for faster transfer
        $tarFile = Join-Path $env:TEMP "econetool_deploy.tar.gz"

        # Use tar if available (Git Bash), otherwise use scp directly
        $gitBash = "C:\Program Files\Git\bin\bash.exe"
        if (Test-Path $gitBash) {
            # Use tar via Git Bash for faster transfer
            $tarCommand = "cd '$tempDir' && tar -czf '$tarFile' ."
            & $gitBash -c $tarCommand.Replace('\', '/')

            if (Test-Path $tarFile) {
                Write-Log "Created archive: $tarFile"

                if (-not $DryRun) {
                    # Upload tar file
                    Copy-ToRemote -LocalPath $tarFile -RemotePath "/tmp/econetool_deploy.tar.gz"

                    # Extract on server
                    Invoke-RemoteCommand "sudo rm -rf $APP_DEPLOY_PATH/* && sudo tar -xzf /tmp/econetool_deploy.tar.gz -C $APP_DEPLOY_PATH && rm /tmp/econetool_deploy.tar.gz"
                }

                Remove-Item $tarFile -Force -ErrorAction SilentlyContinue
            }
        } else {
            # Fallback: use scp for each directory
            Write-Log "Git Bash not found, using direct SCP (slower)..." "WARNING"

            foreach ($file in $files) {
                $localPath = Join-Path $tempDir $file.RelativePath
                $remotePath = "$APP_DEPLOY_PATH/$($file.RelativePath)"

                if (Test-Path $localPath) {
                    if ($file.IsDirectory) {
                        Invoke-RemoteCommand "sudo mkdir -p $remotePath" -IgnoreError
                        Copy-ToRemote -LocalPath "$localPath\*" -RemotePath $remotePath -Recursive
                    } else {
                        $remoteDir = Split-Path -Parent $remotePath
                        Invoke-RemoteCommand "sudo mkdir -p $remoteDir" -IgnoreError
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

    Invoke-RemoteCommand "sudo chown -R shiny:shiny $APP_DEPLOY_PATH"
    Invoke-RemoteCommand "sudo chmod -R 755 $APP_DEPLOY_PATH"
    Invoke-RemoteCommand "sudo chmod 644 $APP_DEPLOY_PATH/app.R"

    Write-Log "Permissions set" "SUCCESS"
}

function Restart-ShinyServer {
    if (-not $RestartServer) {
        Write-Log "Skipping server restart (use -RestartServer to restart)" "INFO"
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

    # Check Shiny Server status
    $serverStatus = Invoke-RemoteCommand "sudo systemctl is-active shiny-server" -IgnoreError
    Write-Log "Shiny Server status: $($serverStatus.Trim())"

    return $true
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

Write-Host ""
Write-Host "================================================================================" -ForegroundColor Blue
Write-Host " EcoNeTool - Windows Deployment to $Host" -ForegroundColor Blue
Write-Host "================================================================================" -ForegroundColor Blue
Write-Host ""

if ($DryRun) {
    Write-Host "[DRY RUN MODE - No changes will be made]" -ForegroundColor Yellow
    Write-Host ""
}

Write-Log "Deployment started"
Write-Log "Target: $SSH_TARGET"
Write-Log "Deploy path: $APP_DEPLOY_PATH"

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
        Write-Host " Application URL: http://$Host/$APP_NAME/" -ForegroundColor Cyan
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
