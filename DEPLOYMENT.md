# EcoNeTool Deployment Guide

## Overview

This guide covers deploying the EcoNeTool Shiny application to the Shiny Server on **laguna.ku.lt**.

---

## Quick Start

```bash
# Set your username
export SHINY_SERVER_USER=your_username

# Make deploy script executable
chmod +x deploy.sh

# Run deployment
./deploy.sh
```

Application will be available at: `http://laguna.ku.lt:3838/EcoNeTool/`

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Configuration](#configuration)
3. [Deployment Methods](#deployment-methods)
4. [Deployment Options](#deployment-options)
5. [Troubleshooting](#troubleshooting)
6. [Rollback Procedure](#rollback-procedure)
7. [Post-Deployment](#post-deployment)

---

## Prerequisites

### Local Machine

1. **Git Bash / WSL / Linux Terminal**
   - Deployment script requires bash
   - On Windows: Use Git Bash or WSL

2. **SSH Client**
   ```bash
   ssh --version
   ```

3. **rsync**
   ```bash
   rsync --version
   ```
   - Windows: Install via Git Bash or WSL
   - Linux/Mac: Usually pre-installed

4. **R and Required Packages** (for local testing)
   ```r
   install.packages(c("shiny", "bs4Dash", "igraph", "fluxweb", "visNetwork"))
   ```

### Server (laguna.ku.lt)

1. **Shiny Server Installed**
   - Version 1.5.x or higher recommended

2. **R Installed**
   - Version 4.0+ recommended
   - R packages will be installed automatically

3. **Server Access**
   - SSH access with sudo privileges (for server restart)
   - Write permissions on `/srv/shiny-server/`

4. **Network Configuration**
   - Port 3838 open for Shiny Server
   - Port 22 open for SSH

---

## Configuration

### 1. SSH Setup

#### Configure SSH Key Authentication

```bash
# Generate SSH key if you don't have one
ssh-keygen -t rsa -b 4096 -C "your_email@example.com"

# Copy key to server
ssh-copy-id your_username@laguna.ku.lt

# Test connection
ssh your_username@laguna.ku.lt "echo 'Connection successful'"
```

#### Set Up SSH Config (Optional but Recommended)

Create/edit `~/.ssh/config`:

```
Host laguna
    HostName laguna.ku.lt
    User your_username
    Port 22
    IdentityFile ~/.ssh/id_rsa
```

Now you can connect with just: `ssh laguna`

### 2. Environment Variables

Set your username:

```bash
# For current session
export SHINY_SERVER_USER=your_username

# Permanent (add to ~/.bashrc or ~/.bash_profile)
echo 'export SHINY_SERVER_USER=your_username' >> ~/.bashrc
source ~/.bashrc
```

### 3. Verify Configuration

```bash
# Source the configuration file
Rscript -e "source('deploy_config.R'); validate_config()"
```

---

## Deployment Methods

### Method 1: Automated Deployment (Recommended)

Use the provided deployment script:

```bash
# Standard deployment
./deploy.sh

# See what would be deployed (no changes)
./deploy.sh --dry-run

# Deploy without backup
./deploy.sh --no-backup

# Force deployment (skip some checks)
./deploy.sh --force
```

### Method 2: Manual Deployment

If automated script doesn't work:

```bash
# 1. Create app directory on server
ssh your_username@laguna.ku.lt "mkdir -p /srv/shiny-server/EcoNeTool"

# 2. Copy files using rsync
rsync -avz --progress \
  --exclude='*.Rproj' \
  --exclude='.git' \
  --exclude='*test*.R' \
  ./ your_username@laguna.ku.lt:/srv/shiny-server/EcoNeTool/

# 3. Install packages on server
ssh your_username@laguna.ku.lt "Rscript -e \"install.packages(c('shiny','bs4Dash','igraph','fluxweb','visNetwork'), repos='https://cloud.r-project.org')\""

# 4. Restart Shiny Server
ssh your_username@laguna.ku.lt "sudo systemctl restart shiny-server"
```

### Method 3: Direct File Copy (Simple but not recommended)

```bash
# Zip the application
zip -r EcoNeTool.zip *.R *.Rdata *.md -x "*test*" "*backup*"

# Copy to server
scp EcoNeTool.zip your_username@laguna.ku.lt:/tmp/

# SSH to server and extract
ssh your_username@laguna.ku.lt
cd /srv/shiny-server/
sudo mkdir -p EcoNeTool
cd EcoNeTool
sudo unzip /tmp/EcoNeTool.zip
sudo systemctl restart shiny-server
```

---

## Deployment Options

### Command Line Flags

| Flag | Description |
|------|-------------|
| `--dry-run` | Show what would be deployed without making changes |
| `--no-backup` | Skip backup creation |
| `--force` | Skip pre-deployment checks |
| `--help` | Show help message |

### Examples

```bash
# Test deployment without making changes
./deploy.sh --dry-run

# Quick deployment without backup (use with caution)
./deploy.sh --no-backup

# Force deployment even if some files are missing
./deploy.sh --force

# Combine flags
./deploy.sh --dry-run --no-backup
```

---

## Pre-Deployment Checklist

Before deploying, ensure:

### Code Quality
- [ ] All tests pass: `Rscript test_v2.1.R`
- [ ] No syntax errors: `Rscript check_syntax.R`
- [ ] Application runs locally: `Rscript -e "shiny::runApp()"`

### Files
- [ ] `app.R` is present and updated
- [ ] `plotfw.R` is present
- [ ] `BalticFW.Rdata` is present
- [ ] Documentation files are up to date

### Configuration
- [ ] SSH access configured
- [ ] `SHINY_SERVER_USER` environment variable set
- [ ] Server has enough disk space
- [ ] Required R packages available

### Backup
- [ ] Recent backup exists (or will be created automatically)
- [ ] Know how to rollback if needed

---

## Troubleshooting

### SSH Connection Fails

**Error:** `Permission denied (publickey)`

**Solutions:**
```bash
# 1. Check if SSH key exists
ls -la ~/.ssh/id_rsa*

# 2. Add key to SSH agent
ssh-add ~/.ssh/id_rsa

# 3. Test with verbose mode
ssh -v your_username@laguna.ku.lt

# 4. Try password authentication (if allowed)
ssh -o PreferredAuthentications=password your_username@laguna.ku.lt
```

### rsync Not Found

**Error:** `rsync: command not found`

**Solutions:**
```bash
# Windows (Git Bash)
# rsync should be included with Git Bash
# If not, install via: pacman -S rsync

# Windows (WSL)
sudo apt-get install rsync

# Mac
brew install rsync

# Linux
sudo apt-get install rsync  # Debian/Ubuntu
sudo yum install rsync      # CentOS/RHEL
```

### Permission Denied on Server

**Error:** `Permission denied` when copying files

**Solutions:**
```bash
# 1. Check current permissions
ssh your_username@laguna.ku.lt "ls -la /srv/shiny-server/"

# 2. Fix permissions (requires sudo)
ssh your_username@laguna.ku.lt "sudo chown -R your_username:shiny /srv/shiny-server/EcoNeTool"

# 3. Or create directory with correct permissions
ssh your_username@laguna.ku.lt "sudo mkdir -p /srv/shiny-server/EcoNeTool && sudo chown your_username:shiny /srv/shiny-server/EcoNeTool"
```

### Package Installation Fails

**Error:** Packages fail to install on server

**Solutions:**
```bash
# 1. Install packages manually
ssh your_username@laguna.ku.lt
R
install.packages(c("shiny", "bs4Dash", "igraph", "fluxweb", "visNetwork"))

# 2. Check R version
ssh your_username@laguna.ku.lt "R --version"

# 3. Check available repositories
ssh your_username@laguna.ku.lt "Rscript -e 'getOption(\"repos\")'"

# 4. Install with specific repository
ssh your_username@laguna.ku.lt "Rscript -e \"install.packages('shiny', repos='https://cloud.r-project.org')\""
```

### Application Doesn't Start

**Error:** App not accessible after deployment

**Solutions:**
```bash
# 1. Check Shiny Server status
ssh your_username@laguna.ku.lt "sudo systemctl status shiny-server"

# 2. Check Shiny Server logs
ssh your_username@laguna.ku.lt "sudo tail -f /var/log/shiny-server.log"

# 3. Check application logs
ssh your_username@laguna.ku.lt "sudo tail -f /var/log/shiny-server/EcoNeTool-*.log"

# 4. Restart Shiny Server
ssh your_username@laguna.ku.lt "sudo systemctl restart shiny-server"

# 5. Check file permissions
ssh your_username@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/"
```

### Application Shows Errors

**Check logs on server:**
```bash
# SSH to server
ssh your_username@laguna.ku.lt

# View Shiny Server logs
sudo tail -f /var/log/shiny-server.log

# View application-specific logs
sudo tail -f /var/log/shiny-server/EcoNeTool-shiny-*.log

# Check for R errors
sudo grep -i error /var/log/shiny-server/EcoNeTool-*.log
```

---

## Rollback Procedure

If deployment fails or causes issues:

### Automatic Rollback (Using Backup)

```bash
# 1. SSH to server
ssh your_username@laguna.ku.lt

# 2. List available backups
ls -lht /srv/shiny-server/backups/EcoNeTool/

# 3. Restore from backup
cd /srv/shiny-server
sudo rm -rf EcoNeTool
sudo tar -xzf backups/EcoNeTool/EcoNeTool_YYYYMMDD_HHMMSS.tar.gz

# 4. Restart Shiny Server
sudo systemctl restart shiny-server
```

### Manual Rollback (From Git)

```bash
# 1. Checkout previous version locally
git log --oneline
git checkout <previous-commit-hash>

# 2. Redeploy
./deploy.sh

# 3. Return to latest
git checkout main
```

---

## Post-Deployment

### 1. Verification Checklist

- [ ] Application URL accessible: `http://laguna.ku.lt:3838/EcoNeTool/`
- [ ] Dashboard loads correctly
- [ ] Data Import tab visible and functional
- [ ] Network visualization works
- [ ] All analysis tabs load
- [ ] No error messages in browser console (F12)

### 2. Testing Procedure

```bash
# Open application in browser
http://laguna.ku.lt:3838/EcoNeTool/

# Test each feature:
1. Dashboard - Check welcome message and value boxes
2. Data Import - Verify format documentation displays
3. Food Web Network - Ensure interactive network loads
4. Topological Metrics - Check calculations display
5. Biomass Analysis - Verify plots render
6. Energy Fluxes - Check flux calculations
```

### 3. Monitor Logs

```bash
# Watch logs in real-time
ssh your_username@laguna.ku.lt "sudo tail -f /var/log/shiny-server.log"

# Check for errors
ssh your_username@laguna.ku.lt "sudo grep -i error /var/log/shiny-server/EcoNeTool-*.log"
```

### 4. Performance Check

```bash
# Check server resources
ssh your_username@laguna.ku.lt "top -n 1 | grep -E 'shiny|R'"

# Check disk space
ssh your_username@laguna.ku.lt "df -h /srv/shiny-server/"

# Check memory usage
ssh your_username@laguna.ku.lt "free -h"
```

---

## Server Configuration

### Shiny Server Configuration

Default location: `/etc/shiny-server/shiny-server.conf`

```conf
# Run as user 'shiny'
run_as shiny;

# Define server
server {
  listen 3838;

  # Define application location
  location /EcoNeTool {
    app_dir /srv/shiny-server/EcoNeTool;
    log_dir /var/log/shiny-server;

    # Optional: Set custom timeouts
    app_idle_timeout 60;
    app_init_timeout 120;
  }

  # Define general location for other apps
  location / {
    site_dir /srv/shiny-server;
    log_dir /var/log/shiny-server;
    directory_index on;
  }
}
```

### Restart Shiny Server After Config Changes

```bash
ssh your_username@laguna.ku.lt "sudo systemctl restart shiny-server"
```

---

## Continuous Deployment

### Set Up Automated Deployments (Optional)

Create a deployment script that runs on git push:

```bash
# On your local machine, create git hook
cat > .git/hooks/post-commit << 'EOF'
#!/bin/bash
echo "Deploying to laguna.ku.lt..."
./deploy.sh --no-backup
EOF

chmod +x .git/hooks/post-commit
```

### Schedule Regular Backups

```bash
# On server, add to crontab
ssh your_username@laguna.ku.lt "crontab -e"

# Add line (backup daily at 2 AM):
0 2 * * * cd /srv/shiny-server && tar -czf backups/EcoNeTool/backup_$(date +\%Y\%m\%d).tar.gz EcoNeTool
```

---

## Security Considerations

### 1. File Permissions

```bash
# Set correct permissions on server
ssh your_username@laguna.ku.lt << 'EOF'
  sudo chown -R shiny:shiny /srv/shiny-server/EcoNeTool
  sudo chmod -R 755 /srv/shiny-server/EcoNeTool
  sudo chmod 644 /srv/shiny-server/EcoNeTool/*.R
  sudo chmod 644 /srv/shiny-server/EcoNeTool/*.Rdata
EOF
```

### 2. Restrict Access (Optional)

If you want to restrict access to specific IPs:

```conf
# In /etc/shiny-server/shiny-server.conf
location /EcoNeTool {
  app_dir /srv/shiny-server/EcoNeTool;

  # Allow only specific IPs
  allow 192.168.1.0/24;
  deny all;
}
```

### 3. Use HTTPS (Recommended)

Set up reverse proxy with nginx and SSL certificate.

---

## Maintenance

### Regular Tasks

**Weekly:**
- Check application logs for errors
- Verify all features working
- Monitor disk space

**Monthly:**
- Review and clean old backups
- Update R packages if needed
- Check for EcoNeTool updates

**As Needed:**
- Restart Shiny Server after updates
- Clear cached data if needed
- Update deployment scripts

---

## Quick Reference

### Common Commands

```bash
# Deploy
./deploy.sh

# Dry run
./deploy.sh --dry-run

# SSH to server
ssh your_username@laguna.ku.lt

# Restart Shiny Server
ssh your_username@laguna.ku.lt "sudo systemctl restart shiny-server"

# View logs
ssh your_username@laguna.ku.lt "sudo tail -f /var/log/shiny-server.log"

# Check status
ssh your_username@laguna.ku.lt "sudo systemctl status shiny-server"
```

### Important Paths

| Item | Path |
|------|------|
| Application | `/srv/shiny-server/EcoNeTool/` |
| Backups | `/srv/shiny-server/backups/EcoNeTool/` |
| Logs | `/var/log/shiny-server/` |
| Config | `/etc/shiny-server/shiny-server.conf` |
| URL | `http://laguna.ku.lt:3838/EcoNeTool/` |

---

## Support

### Getting Help

1. Check logs for specific errors
2. Review this documentation
3. Check Shiny Server documentation: https://docs.rstudio.com/shiny-server/
4. Contact server administrator for permissions issues

### Reporting Issues

When reporting deployment issues, include:
- Deployment log file
- Error messages
- Server logs (if accessible)
- Steps to reproduce

---

## Appendix

### A. Deployment Script Reference

See `deploy.sh` for the full deployment script.

**Key functions:**
- `check_prerequisites()` - Verify system requirements
- `create_backup()` - Backup existing application
- `deploy_files()` - Copy files to server
- `install_packages()` - Install R packages
- `restart_shiny_server()` - Restart service
- `verify_deployment()` - Check deployment success

### B. Configuration Reference

See `deploy_config.R` for all configuration options.

### C. Network Ports

| Port | Service | Protocol |
|------|---------|----------|
| 22 | SSH | TCP |
| 3838 | Shiny Server | TCP |
| 80 | HTTP (if using nginx) | TCP |
| 443 | HTTPS (if using nginx) | TCP |

---

**Last Updated:** 2025-11-27
**Version:** 2.1
**Status:** Production Ready

---

**Happy Deploying! 🚀**
