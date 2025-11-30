# Pre-Deployment Checklist for EcoNeTool

## Overview
Complete this checklist before deploying EcoNeTool to laguna.ku.lt to ensure a smooth deployment.

**Date:** _______________
**Deployer:** _______________
**Version:** 2.1

---

## 1. Code Quality & Testing

### Local Testing
- [ ] Application runs locally without errors
  ```r
  library(shiny)
  runApp()
  ```
- [ ] All tests pass
  ```r
  source("test_v2.1.R")
  ```
- [ ] No syntax errors
  ```r
  source("check_syntax.R")
  ```
- [ ] Configuration valid
  ```r
  source("deploy_config.R")
  validate_config()
  ```

### Feature Testing
- [ ] Dashboard page loads correctly
- [ ] Data Import tab displays documentation
- [ ] Food Web Network visualization works
- [ ] Topological Metrics calculate correctly
- [ ] Biomass Analysis plots render
- [ ] Energy Fluxes calculate correctly
- [ ] No JavaScript errors in browser console (F12)

### Browser Compatibility
- [ ] Tested in Chrome
- [ ] Tested in Firefox
- [ ] Tested in Edge/Safari (if available)

---

## 2. Files & Documentation

### Required Files Present
- [ ] `app.R` - Main application
- [ ] `plotfw.R` - Plotting functions
- [ ] `BalticFW.Rdata` - Default dataset
- [ ] `README.md` - Project documentation
- [ ] `BS4DASH_README.md` - Interface documentation
- [ ] `QUICK_START_BS4DASH.md` - Quick reference
- [ ] `LAUNCH_GUIDE.md` - User guide
- [ ] `IMPROVEMENTS.md` - Code improvements
- [ ] `FIXES_APPLIED.md` - Bug fix documentation
- [ ] `CHANGELOG_v2.1.md` - Version changelog

### Deployment Files
- [ ] `deploy_config.R` - Configuration file
- [ ] `deploy.sh` - Bash deployment script
- [ ] `deploy.R` - R deployment script
- [ ] `DEPLOYMENT.md` - Deployment guide
- [ ] `PRE_DEPLOYMENT_CHECKLIST.md` - This file

### File Verification
- [ ] All required files have correct permissions (readable)
- [ ] No temporary or backup files in deployment list
- [ ] Data files not corrupted (test loading)
  ```r
  load("BalticFW.Rdata")
  str(net)
  str(info)
  ```

---

## 3. Server Configuration

### SSH Access
- [ ] SSH key-based authentication configured
  ```bash
  ssh your_username@laguna.ku.lt "echo 'Success'"
  ```
- [ ] SSH config file set up (optional but recommended)
- [ ] Can connect without password prompt
- [ ] Have sudo access (for server restart)

### Environment Variables
- [ ] `SHINY_SERVER_USER` environment variable set
  ```bash
  echo $SHINY_SERVER_USER
  # or in R:
  Sys.getenv("SHINY_SERVER_USER")
  ```
- [ ] Username is correct for laguna.ku.lt

### Server Access Verification
- [ ] Can access `/srv/shiny-server/` directory
  ```bash
  ssh your_username@laguna.ku.lt "ls -la /srv/shiny-server/"
  ```
- [ ] Have write permissions to deploy location
  ```bash
  ssh your_username@laguna.ku.lt "touch /srv/shiny-server/test && rm /srv/shiny-server/test && echo 'OK'"
  ```

### Server Requirements
- [ ] Shiny Server installed on laguna.ku.lt
  ```bash
  ssh your_username@laguna.ku.lt "which shiny-server"
  ```
- [ ] R installed (version 4.0+)
  ```bash
  ssh your_username@laguna.ku.lt "R --version"
  ```
- [ ] Sufficient disk space available
  ```bash
  ssh your_username@laguna.ku.lt "df -h /srv/shiny-server/"
  ```

---

## 4. Dependencies

### Local Tools
- [ ] rsync installed (for file transfer)
  ```bash
  rsync --version
  ```
- [ ] SSH client available
  ```bash
  ssh -V
  ```
- [ ] Git Bash or WSL available (Windows users)

### R Packages (Local)
- [ ] shiny
- [ ] bs4Dash
- [ ] igraph
- [ ] fluxweb
- [ ] visNetwork

```r
required <- c("shiny", "bs4Dash", "igraph", "fluxweb", "visNetwork")
installed <- installed.packages()[, "Package"]
missing <- required[!required %in% installed]
if (length(missing) > 0) {
  cat("Missing packages:", paste(missing, collapse=", "), "\n")
} else {
  cat("All required packages installed ✓\n")
}
```

### R Packages (Server)
- [ ] Packages will be installed automatically during deployment
- [ ] OR manually verify packages are available:
  ```bash
  ssh your_username@laguna.ku.lt "Rscript -e 'installed.packages()[,\"Package\"]'"
  ```

---

## 5. Backup & Rollback Plan

### Backup Strategy
- [ ] Automatic backup will be created during deployment
- [ ] OR manually created backup:
  ```bash
  ssh your_username@laguna.ku.lt "cd /srv/shiny-server && tar -czf backups/EcoNeTool/manual_backup_$(date +%Y%m%d).tar.gz EcoNeTool"
  ```
- [ ] Know backup location: `/srv/shiny-server/backups/EcoNeTool/`
- [ ] Tested rollback procedure

### Rollback Plan
- [ ] Know how to restore from backup (see DEPLOYMENT.md)
- [ ] Have previous version in git (if using version control)
- [ ] Have contact info for server admin if help needed

---

## 6. Communication & Coordination

### Stakeholders
- [ ] Informed team about planned deployment
- [ ] Scheduled deployment during low-usage period
- [ ] Have backup contact if primary deployer unavailable

### Downtime Window
- [ ] Deployment time estimated: ______ minutes
- [ ] Acceptable downtime window identified
- [ ] Users notified (if applicable)

---

## 7. Deployment Preparation

### Configuration Review
- [ ] Reviewed `deploy_config.R`
- [ ] Correct server hostname (laguna.ku.lt)
- [ ] Correct application name (EcoNeTool)
- [ ] Correct file list for deployment
- [ ] Correct package dependencies list

### Deployment Script Selection
- [ ] Chosen deployment method:
  - [ ] `deploy.sh` (Bash script - Linux/Mac/Git Bash)
  - [ ] `deploy.R` (R script - cross-platform)
  - [ ] Manual deployment (DEPLOYMENT.md steps)

### Dry Run
- [ ] Performed dry-run deployment
  ```bash
  ./deploy.sh --dry-run
  # or
  Rscript deploy.R --dry-run
  ```
- [ ] Reviewed what would be deployed
- [ ] No unexpected files or exclusions

---

## 8. Security Review

### Sensitive Information
- [ ] No passwords or API keys in code
- [ ] No sensitive data in default dataset
- [ ] Environment variables used for credentials
- [ ] `.git` and other dev files excluded from deployment

### Permissions
- [ ] Application will run as 'shiny' user on server
- [ ] No unnecessary elevated permissions required
- [ ] File permissions will be set correctly

---

## 9. Post-Deployment Plan

### Verification Steps
- [ ] Have list of features to test after deployment
- [ ] Know application URL: `http://laguna.ku.lt:3838/EcoNeTool/`
- [ ] Have browser bookmarks or test script ready

### Monitoring
- [ ] Know how to access server logs
  ```bash
  ssh your_username@laguna.ku.lt "sudo tail -f /var/log/shiny-server.log"
  ```
- [ ] Have phone/email access during deployment
- [ ] Prepared to rollback if issues arise

### Documentation
- [ ] Will update deployment log after completion
- [ ] Will document any issues encountered
- [ ] Will note any manual steps required

---

## 10. Final Checklist

### Just Before Deployment
- [ ] All items above completed
- [ ] Working directory is project root
- [ ] No uncommitted changes (if using git)
- [ ] Terminal/console ready
- [ ] Have at least 30 minutes available
- [ ] Not deploying during peak usage time
- [ ] Have rollback plan ready

### Ready to Deploy?
- [ ] **YES** - All checks passed, ready to proceed
- [ ] **NO** - Address issues above before deploying

---

## Deployment Command

Once all checks complete, run:

### Option 1: Bash Script
```bash
# Set username
export SHINY_SERVER_USER=your_username

# Make executable
chmod +x deploy.sh

# Deploy
./deploy.sh
```

### Option 2: R Script
```r
# Set username
Sys.setenv(SHINY_SERVER_USER = "your_username")

# Deploy
source("deploy.R")
deploy_to_server()
```

### Option 3: Manual Deployment
Follow steps in `DEPLOYMENT.md` section "Manual Deployment"

---

## Post-Deployment Verification

After deployment completes:

### Immediate Checks (< 5 minutes)
- [ ] Application URL loads: `http://laguna.ku.lt:3838/EcoNeTool/`
- [ ] No error messages on screen
- [ ] Dashboard displays correctly
- [ ] Can navigate between tabs

### Full Testing (10-15 minutes)
- [ ] All 6 tabs load without errors
- [ ] Interactive network visualization works
- [ ] Data calculations display correctly
- [ ] No console errors (F12 Developer Tools)
- [ ] Default Gulf of Riga data loads
- [ ] Data Import documentation displays

### Server Health (Optional)
- [ ] Check server logs for errors
- [ ] Monitor memory/CPU usage
- [ ] Verify no other apps affected

---

## Sign-Off

**Checklist Completed By:** _______________
**Date:** _______________
**Time:** _______________

**Deployment Status:**
- [ ] ✅ SUCCESS - Application deployed and verified
- [ ] ⚠️ PARTIAL - Deployed with minor issues (document below)
- [ ] ❌ FAILED - Deployment failed (document below)

**Notes:**
```
_______________________________________________________________
_______________________________________________________________
_______________________________________________________________
```

**Issues Encountered:**
```
_______________________________________________________________
_______________________________________________________________
_______________________________________________________________
```

**Resolution/Next Steps:**
```
_______________________________________________________________
_______________________________________________________________
_______________________________________________________________
```

---

## Quick Reference

### Key Commands
```bash
# Test SSH
ssh your_username@laguna.ku.lt "echo OK"

# Deploy (Bash)
./deploy.sh

# Deploy (R)
Rscript deploy.R

# View logs
ssh your_username@laguna.ku.lt "sudo tail -f /var/log/shiny-server.log"

# Restart server
ssh your_username@laguna.ku.lt "sudo systemctl restart shiny-server"
```

### Important Paths
- Server: laguna.ku.lt
- App path: `/srv/shiny-server/EcoNeTool/`
- Backup path: `/srv/shiny-server/backups/EcoNeTool/`
- Logs: `/var/log/shiny-server/`
- URL: `http://laguna.ku.lt:3838/EcoNeTool/`

---

**Last Updated:** 2025-11-27
**Version:** 2.1
**For:** EcoNeTool Deployment to laguna.ku.lt
