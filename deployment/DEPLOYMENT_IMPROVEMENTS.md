# Deployment System Improvements

## Overview

The EcoNeTool deployment system has been significantly enhanced to ensure reliable file copying, comprehensive logging, and easy troubleshooting.

## Problems Solved

### 1. **Silent File Copy Failures**
**Problem**: Files weren't being copied to the server, but no errors were shown.

**Solution**: Added comprehensive error checking for every file copy operation:
- Each file copy now has individual error checking
- Verbose output (`cp -v`) shows exactly what's being copied
- Script exits immediately if any critical file fails to copy
- Success/failure messages for each file
- All operations logged to `/var/log/econetool/deployment.log`

### 2. **No Deployment Verification**
**Problem**: No way to check if deployment actually worked.

**Solution**: Created comprehensive `verify-deployment.sh` script that checks:
- Shiny Server status
- File existence and sizes
- File modification timestamps
- Source vs deployed version comparison
- File permissions
- Recent error logs
- Deployment log entries

### 3. **Insufficient Logging**
**Problem**: Hard to debug deployment issues without logs.

**Solution**: Added complete logging system:
- All deployments logged to `/var/log/econetool/deployment.log`
- Timestamps for every operation
- User and method tracking
- Success/failure logging
- Easy to review: `sudo tail -f /var/log/econetool/deployment.log`

## Enhanced Scripts

### 1. deploy.sh

**Improvements**:
```bash
# Before (silent failure)
cp "$APP_DIR/app.R" /srv/shiny-server/EcoNeTool/

# After (error checked, logged, verbose)
if cp -v "$APP_DIR/app.R" /srv/shiny-server/EcoNeTool/; then
    print_success "Copied: app.R"
    log_message "DEPLOY" "Copied app.R from $APP_DIR"
else
    print_error "Failed to copy app.R"
    log_message "ERROR" "Failed to copy app.R from $APP_DIR"
    exit 1
fi
```

**Features**:
- ✅ Verbose file copying with `-v` flag
- ✅ Error checking for each file
- ✅ Immediate exit on failure
- ✅ Complete logging to `/var/log/econetool/deployment.log`
- ✅ Color-coded console output
- ✅ Permission setting with error checking

### 2. force-reload.sh

**Improvements**:
- Same error checking as deploy.sh
- Shows exactly what files are being copied
- Exits on first failure
- Clear success/failure messages

**Usage**:
```bash
cd deployment
sudo ./force-reload.sh
```

### 3. verify-deployment.sh

**New comprehensive verification tool** with 8 checks:

1. **Shiny Server Status**
   - Running/stopped
   - Service status

2. **Deployment Directory**
   - Exists
   - File listing with sizes
   - Disk usage

3. **Required Files Check**
   - app.R (with size, timestamp)
   - functions.R (with size, timestamp)
   - BalticFW.Rdata (with size, timestamp)
   - run_app.R (optional)

4. **Version Check**
   - Detects old plotfw.R references
   - Confirms functions.R sourcing
   - Identifies version (v1.0.0+)

5. **Source vs Deployed Comparison**
   - Compares timestamps
   - Shows age difference if mismatched
   - Warns if source is newer

6. **File Permissions**
   - Owner (should be shiny:shiny)
   - Permission bits

7. **Recent Logs**
   - Checks for errors in last 5 minutes
   - Shows recent log entries

8. **Deployment Log**
   - Last 5 deployment entries
   - Shows deployment history

**Usage**:
```bash
cd deployment
sudo ./verify-deployment.sh
```

**Example Output**:
```
═══════════════════════════════════════════
1. SHINY SERVER STATUS
═══════════════════════════════════════════
   ✅ Shiny Server is running
   ✅ Status: active

═══════════════════════════════════════════
2. DEPLOYMENT DIRECTORY
═══════════════════════════════════════════
   ✅ Deployment directory exists
   Location: /srv/shiny-server/EcoNeTool

   Files in deployment directory:
   -rw-r--r-- 1 shiny shiny 103K Dec  1 12:36 app.R
   -rw-r--r-- 1 shiny shiny  27K Dec  1 12:16 functions.R
   -rw-r--r-- 1 shiny shiny  27K Nov 27 11:18 BalticFW.Rdata

═══════════════════════════════════════════
3. REQUIRED FILES CHECK
═══════════════════════════════════════════
   ✅ app.R exists
      Size: 102.82KiB
      Modified: 2024-12-01 12:36:45
   ✅ functions.R exists
      Size: 26.99KiB
      Modified: 2024-12-01 12:16:30
   ✅ BalticFW.Rdata exists
      Size: 26.34KiB
      Modified: 2024-11-27 11:18:00

═══════════════════════════════════════════
4. VERSION CHECK
═══════════════════════════════════════════
   ✅ No plotfw.R reference (correct)
   ✅ app.R sources functions.R (v1.0.0+)

═══════════════════════════════════════════
5. SOURCE vs DEPLOYED COMPARISON
═══════════════════════════════════════════
   app.R:
      Source: 2024-12-01 12:36:45
      Deployed: 2024-12-01 12:36:45
      ✅ Versions match (same timestamp)

   functions.R:
      Source: 2024-12-01 12:16:30
      Deployed: 2024-12-01 12:16:30
      ✅ Versions match (same timestamp)

═══════════════════════════════════════════
SUMMARY
═══════════════════════════════════════════
✅ No issues detected. Deployment appears healthy.
```

## Deployment Workflow

### Normal Deployment

```bash
# 1. Pre-deployment check
cd deployment
Rscript pre-deploy-check.R

# 2. Deploy
sudo ./deploy.sh --shiny-server

# 3. Verify
sudo ./verify-deployment.sh

# 4. Clear browser cache
# Ctrl+Shift+R or Cmd+Shift+R
```

### Troubleshooting Deployment

If something goes wrong:

```bash
# 1. Check what's deployed
sudo ./verify-deployment.sh

# 2. Review deployment log
sudo tail -f /var/log/econetool/deployment.log

# 3. Check Shiny Server logs
sudo journalctl -u shiny-server -n 50

# 4. Force complete reload
sudo ./force-reload.sh

# 5. Verify again
sudo ./verify-deployment.sh
```

## File Copy Process

The deployment now follows this process for each file:

1. **Pre-flight**: Check source file exists
2. **Copy**: Use `cp -v` for verbose output
3. **Verify**: Check copy succeeded
4. **Log**: Record success/failure
5. **Report**: Show color-coded message
6. **Exit**: Stop immediately on failure

## Logging System

### Log Location
```
/var/log/econetool/deployment.log
```

### Log Format
```
[YYYY-MM-DD HH:MM:SS] [LEVEL] message
```

### Log Levels
- **DEPLOY**: Deployment lifecycle events
- **INFO**: General information
- **SUCCESS**: Successful operations
- **WARNING**: Non-critical issues
- **ERROR**: Critical failures

### Viewing Logs
```bash
# Tail deployment log
sudo tail -f /var/log/econetool/deployment.log

# View last 50 lines
sudo tail -50 /var/log/econetool/deployment.log

# Search for errors
sudo grep ERROR /var/log/econetool/deployment.log

# Today's deployments
sudo grep "$(date +%Y-%m-%d)" /var/log/econetool/deployment.log
```

## Error Messages

### Clear Error Output

**File Copy Failure**:
```
✗ Failed to copy app.R
```
Then check logs:
```bash
sudo tail /var/log/econetool/deployment.log
```

**Permission Failure**:
```
✗ Failed to set permissions
```

**Missing Source File**:
```
cp: cannot stat '/path/to/file': No such file or directory
```

## Verification Checks

The `verify-deployment.sh` script provides actionable recommendations:

**If files are missing**:
```
⚠️  1 issue(s) detected. Review details above.

Recommended actions:
   - Run: sudo ./force-reload.sh
```

**If source is newer**:
```
⚠️  Source is NEWER than deployed version
Difference: 15 minutes

Recommended actions:
   - Source is newer. Run: sudo ./deploy.sh --shiny-server
```

## Benefits

1. **Reliability**: No more silent failures
2. **Transparency**: See exactly what's being deployed
3. **Debugging**: Complete logs for troubleshooting
4. **Verification**: Easy to check deployment status
5. **Confidence**: Know deployment succeeded
6. **Traceability**: Full audit trail of deployments

## Quick Reference

| Task | Command |
|------|---------|
| Deploy | `sudo ./deploy.sh --shiny-server` |
| Verify | `sudo ./verify-deployment.sh` |
| Force reload | `sudo ./force-reload.sh` |
| View logs | `sudo tail -f /var/log/econetool/deployment.log` |
| Check errors | `sudo journalctl -u shiny-server -n 50` |
| Pre-check | `Rscript pre-deploy-check.R` |

## Files Modified

- ✅ `deploy.sh` - Added error checking and logging
- ✅ `force-reload.sh` - Added error checking
- ✅ `verify-deployment.sh` - Complete rewrite with 8 checks
- ✅ `deployment/README.md` - Updated documentation

## Version

These improvements are part of **EcoNeTool v1.0.0** (2024-12-01)

---

*For more information, see [deployment/README.md](README.md)*
