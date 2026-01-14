# Windows Deployment Guide for EcoNeTool

Deploy EcoNeTool from your Windows computer to laguna.ku.lt Shiny Server.

## Prerequisites

### 1. SSH Client (Built into Windows 10/11)
Windows 10 (version 1809+) and Windows 11 have OpenSSH built-in. To verify:
```powershell
ssh -V
```

If not installed:
```powershell
# Run as Administrator
Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0
```

### 2. SSH Key Setup (Recommended)
Set up passwordless authentication:

```powershell
# Generate SSH key (if you don't have one)
ssh-keygen -t ed25519 -C "your_email@example.com"

# Copy public key to server
type $env:USERPROFILE\.ssh\id_ed25519.pub | ssh razinka@laguna.ku.lt "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys"
```

Or manually copy your public key to the server.

### 3. Git Bash (Optional but Recommended)
Install [Git for Windows](https://git-scm.com/download/win) for faster tar-based transfers.

## Deployment Methods

### Method 1: PowerShell Script (Recommended)

Full-featured deployment with backup, verification, and logging:

```powershell
# Basic deployment
.\deploy-windows.ps1

# Dry run (preview what will be deployed)
.\deploy-windows.ps1 -DryRun

# Skip backup
.\deploy-windows.ps1 -NoBackup

# Restart Shiny Server after deployment
.\deploy-windows.ps1 -RestartServer

# All options
.\deploy-windows.ps1 -DryRun -Verbose
```

**Features:**
- Creates server backup before deployment
- Filters out test files, documentation, and large binaries
- Creates tar archive for fast transfer (if Git Bash available)
- Sets proper permissions (shiny:shiny)
- Verifies deployment success
- Maintains deployment logs

### Method 2: Batch Script (Simple)

Simple deployment for quick updates:

```cmd
# Basic deployment
deploy-windows.bat

# Dry run
deploy-windows.bat --dry-run
```

### Method 3: Manual rsync via Git Bash

For fine-grained control:

```bash
# Open Git Bash, navigate to project
cd /c/Users/DELL/OneDrive\ -\ ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool

# Deploy with rsync
rsync -avz --progress \
  --exclude='*.Rproj' \
  --exclude='.Rproj.user' \
  --exclude='.git' \
  --exclude='.claude' \
  --exclude='tests' \
  --exclude='docs' \
  --exclude='*.ewemdb' \
  --exclude='cache' \
  --exclude='output' \
  --exclude='archive' \
  --exclude='*.md' \
  --exclude='deploy*' \
  app.R run_app.R R/ www/ examples/BalticFW.Rdata metawebs/ data/ \
  razinka@laguna.ku.lt:/srv/shiny-server/EcoNeTool/
```

## Quick Commands

### Test SSH Connection
```powershell
ssh razinka@laguna.ku.lt "echo 'Connected!'"
```

### Check Server Status
```powershell
ssh razinka@laguna.ku.lt "sudo systemctl status shiny-server"
```

### Restart Shiny Server
```powershell
ssh razinka@laguna.ku.lt "sudo systemctl restart shiny-server"
```

### View Server Logs
```powershell
ssh razinka@laguna.ku.lt "sudo tail -50 /var/log/shiny-server/*.log"
```

### Check Deployed Files
```powershell
ssh razinka@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/"
```

### Check Application Logs
```powershell
ssh razinka@laguna.ku.lt "sudo tail -100 /var/log/shiny-server/EcoNeTool-*.log"
```

## Troubleshooting

### "Permission denied" errors
```powershell
# Check your SSH key is loaded
ssh-add -l

# Add your key to the agent
ssh-add $env:USERPROFILE\.ssh\id_ed25519
```

### "Connection refused" errors
- Verify server is running: `ping laguna.ku.lt`
- Check firewall settings on server
- Verify SSH port (default: 22)

### "Host key verification failed"
```powershell
# Remove old host key and reconnect
ssh-keygen -R laguna.ku.lt
ssh razinka@laguna.ku.lt
```

### Application not loading after deployment
1. Check Shiny Server logs:
   ```powershell
   ssh razinka@laguna.ku.lt "sudo tail -100 /var/log/shiny-server/EcoNeTool-*.log"
   ```

2. Verify file permissions:
   ```powershell
   ssh razinka@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/"
   ```

3. Restart Shiny Server:
   ```powershell
   ssh razinka@laguna.ku.lt "sudo systemctl restart shiny-server"
   ```

### Missing R packages on server
```powershell
ssh razinka@laguna.ku.lt "sudo Rscript -e \"install.packages('package_name', repos='https://cloud.r-project.org')\""
```

## Files Deployed

The deployment scripts copy:
- `app.R` - Main application file
- `run_app.R` - Alternative launcher
- `R/` - All R modules and functions
- `www/` - Static web assets (images, CSS)
- `examples/BalticFW.Rdata` - Default dataset
- `examples/LTCoast.Rdata` - Alternative dataset
- `metawebs/` - Metaweb library data
- `data/` - Local trait databases (BVOL, SpeciesEnriched)

The following are **excluded**:
- Test files (`tests/`, `*test*.R`)
- Documentation (`docs/`, `*.md`)
- Development files (`.git/`, `.claude/`, `*.Rproj`)
- Large binary files (`*.ewemdb`, `*.eweaccdb`, `EUSeaMap*.zip`)
- Cache and output directories
- Deployment scripts themselves

## Server Configuration

The application is deployed to:
```
/srv/shiny-server/EcoNeTool/
```

Access URL:
```
http://laguna.ku.lt/EcoNeTool/
```

Shiny Server configuration is in:
```
/etc/shiny-server/shiny-server.conf
```
