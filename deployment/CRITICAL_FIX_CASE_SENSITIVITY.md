# 🚨 CRITICAL FIX: Case Sensitivity Issue in Deployment

## Problem Identified

**Root Cause**: The server had **TWO directories** with different cases:
- `/srv/shiny-server/EcoNeTool` (capital letters) - **ACTUAL server location**
- `/srv/shiny-server/econetool` (lowercase) - **WHERE deployment was copying files**

**Result**: Deployment scripts were copying files to the wrong directory (`econetool`), so the server kept serving the old version from `EcoNeTool`.

## The Issue

Linux is **case-sensitive**, so these are completely different directories:
- `EcoNeTool` ≠ `econetool`

The deployment was silently "succeeding" by creating a NEW lowercase directory and copying files there, but Shiny Server was configured to serve from the uppercase directory!

## Files Fixed

All deployment scripts and configuration files have been updated to use the **correct case-sensitive path**:

### Before (WRONG)
```bash
/srv/shiny-server/econetool/     # lowercase - WRONG!
http://server:3838/econetool     # lowercase URL
```

### After (CORRECT)
```bash
/srv/shiny-server/EcoNeTool/     # capital E, N, T - CORRECT!
http://server:3838/EcoNeTool     # capital letters in URL
```

## Changes Made

### 1. **deploy.sh**
- ✅ Changed directory: `/srv/shiny-server/econetool` → `/srv/shiny-server/EcoNeTool`
- ✅ Updated all file copy paths
- ✅ Fixed cache clearing paths
- ✅ Updated Shiny Server config location
- ✅ Corrected URL output

### 2. **force-reload.sh**
- ✅ Changed all paths to `/srv/shiny-server/EcoNeTool`
- ✅ Updated file copy destinations

### 3. **verify-deployment.sh**
- ✅ Changed `DEPLOY_DIR` variable to `/srv/shiny-server/EcoNeTool`
- ✅ Now verifies correct directory

### 4. **shiny-server.conf**
- ✅ Updated location: `/econetool` → `/EcoNeTool`
- ✅ Updated app_dir: `/srv/shiny-server/econetool` → `/srv/shiny-server/EcoNeTool`

### 5. **Documentation**
- ✅ deployment/README.md - All paths updated
- ✅ deployment/DEPLOYMENT_IMPROVEMENTS.md - Examples updated
- ✅ Main README.md - URL updated to http://laguna.ku.lt:3838/EcoNeTool/

## Correct URLs

### Production
```
http://laguna.ku.lt:3838/EcoNeTool/
```
**Note the capital letters**: E-co-N-e-T-ool

### Local/Development
```
http://localhost:3838/EcoNeTool/
```

## How This Happened

1. Initial deployment created `/srv/shiny-server/EcoNeTool/` (with capitals)
2. Later, deployment scripts were written with lowercase `econetool`
3. Scripts created a second directory with lowercase name
4. Files were deployed to lowercase directory
5. Server kept serving from uppercase directory (old files)
6. **Result**: Deployments appeared to succeed but changes never appeared!

## Verification

After deployment, verify you're deploying to the correct directory:

```bash
# Run verification script
cd deployment
sudo ./verify-deployment.sh

# Should show:
# DEPLOY_DIR="/srv/shiny-server/EcoNeTool"  ← Capital letters!
# ✅ Deployment directory exists

# Check both directories
ls -la /srv/shiny-server/EcoNeTool/   # Should have files
ls -la /srv/shiny-server/econetool/   # Should NOT exist (or be old)
```

## Cleanup Recommendation

**After successful deployment to the correct directory**, you may want to remove the old lowercase directory:

```bash
# ONLY do this after verifying new deployment works!
sudo rm -rf /srv/shiny-server/econetool/
```

This will:
- Free up disk space
- Prevent future confusion
- Ensure only one deployment location exists

## Testing the Fix

### 1. Deploy to Correct Location
```bash
cd deployment
sudo ./deploy.sh --shiny-server
```

**Expected output**:
```
Copying application files...
'/path/to/app.R' -> '/srv/shiny-server/EcoNeTool/app.R'
✓ Copied: app.R
...
Application is running at: http://[IP]:3838/EcoNeTool
```

### 2. Verify Deployment
```bash
sudo ./verify-deployment.sh
```

**Should show**:
```
DEPLOY_DIR="/srv/shiny-server/EcoNeTool"
✅ app.R exists
✅ functions.R exists
✅ BalticFW.Rdata exists
✅ app.R sources functions.R (v1.0.0+)
```

### 3. Check the App
Visit: `http://laguna.ku.lt:3838/EcoNeTool/`

**Clear browser cache first!** (Ctrl+Shift+R)

### 4. Verify New Version
The welcome page should show:
- 📊 Analysis Features section with function names
- Version 1.0.0 - Refactoring Release
- All new features listed

## Lesson Learned

**Always check case sensitivity on Linux servers!**

- File systems on Linux are case-sensitive
- `EcoNeTool` ≠ `econetool` ≠ `ECONETOOL`
- Always verify deployment paths match server configuration
- Use `verify-deployment.sh` to catch issues like this

## Quick Reference

| Item | Correct Value (case-sensitive) |
|------|-------------------------------|
| Directory | `/srv/shiny-server/EcoNeTool` |
| URL Path | `/EcoNeTool` |
| Full URL | `http://laguna.ku.lt:3838/EcoNeTool/` |
| Config Location | `location /EcoNeTool { ... }` |

## Status

✅ **FIXED** - All deployment scripts now use correct case-sensitive paths

Date: 2024-12-01
Version: 1.0.0

---

**Next Deployment**: Files will now be copied to the correct location and changes will appear immediately (after clearing browser cache).
