# Quick Start: Deploy EcoNeTool to laguna.ku.lt

## 🚀 5-Minute Deployment

### Prerequisites Check
```bash
# 1. Can you SSH to the server?
ssh your_username@laguna.ku.lt "echo 'Success'"

# 2. Is rsync installed?
rsync --version
```

If both work, continue! If not, see [DEPLOYMENT.md](DEPLOYMENT.md).

---

## Option 1: R Script (Recommended for Windows)

### Step 1: Set Username
```r
# In R console or RStudio
Sys.setenv(SHINY_SERVER_USER = "your_username")  # Replace with your actual username
```

### Step 2: Deploy
```r
source("deploy.R")
deploy_to_server()
```

### Step 3: Visit App
Open browser: `http://laguna.ku.lt:3838/EcoNeTool/`

**Done!** ✅

---

## Option 2: Bash Script (Linux/Mac/Git Bash)

### Step 1: Set Username
```bash
export SHINY_SERVER_USER=your_username  # Replace with your actual username
```

### Step 2: Make Script Executable
```bash
chmod +x deploy.sh
```

### Step 3: Deploy
```bash
./deploy.sh
```

### Step 4: Visit App
Open browser: `http://laguna.ku.lt:3838/EcoNeTool/`

**Done!** ✅

---

## What Happens During Deployment?

1. **Validates** configuration and tests SSH connection
2. **Creates backup** of existing app (if any)
3. **Copies files** to server using rsync
4. **Installs** required R packages on server
5. **Restarts** Shiny Server
6. **Verifies** deployment succeeded

Total time: ~2-5 minutes (depending on network speed)

---

## Common Issues & Quick Fixes

### ❌ "Permission denied (publickey)"
```bash
# Generate SSH key and copy to server
ssh-keygen -t rsa
ssh-copy-id your_username@laguna.ku.lt
```

### ❌ "SHINY_SERVER_USER not set"
```bash
# Bash
export SHINY_SERVER_USER=your_username

# R
Sys.setenv(SHINY_SERVER_USER = "your_username")
```

### ❌ "rsync: command not found"
Use the R script instead - it has a fallback method.

### ❌ "Cannot restart shiny-server"
```bash
# SSH to server and restart manually
ssh your_username@laguna.ku.lt
sudo systemctl restart shiny-server
```

---

## Dry Run First?

Test what would happen without making changes:

```bash
# Bash
./deploy.sh --dry-run

# R
source("deploy.R")
deploy_to_server(dry_run = TRUE)
```

---

## Need More Help?

- **Detailed guide:** [DEPLOYMENT.md](DEPLOYMENT.md)
- **Checklist:** [PRE_DEPLOYMENT_CHECKLIST.md](PRE_DEPLOYMENT_CHECKLIST.md)
- **Configuration:** [deploy_config.R](deploy_config.R)

---

## After Deployment

### ✅ Verify It Works

Visit: `http://laguna.ku.lt:3838/EcoNeTool/`

Test:
- [ ] Dashboard loads
- [ ] Data Import tab shows documentation
- [ ] Network visualization works
- [ ] Topological Metrics display
- [ ] Biomass Analysis plots render
- [ ] Energy Fluxes calculate

### 📊 Check Logs (if needed)

```bash
ssh your_username@laguna.ku.lt "sudo tail -f /var/log/shiny-server.log"
```

---

## Rollback (if something goes wrong)

```bash
# SSH to server
ssh your_username@laguna.ku.lt

# List backups
ls -lht /srv/shiny-server/backups/EcoNeTool/

# Restore latest backup
cd /srv/shiny-server
sudo rm -rf EcoNeTool
sudo tar -xzf backups/EcoNeTool/EcoNeTool_YYYYMMDD_HHMMSS.tar.gz
sudo systemctl restart shiny-server
```

---

## Summary

**Deployment in 3 commands:**

```r
# R Method (works everywhere)
Sys.setenv(SHINY_SERVER_USER = "your_username")
source("deploy.R")
deploy_to_server()
```

**or**

```bash
# Bash Method (Linux/Mac/Git Bash)
export SHINY_SERVER_USER=your_username
chmod +x deploy.sh
./deploy.sh
```

**Application URL:** `http://laguna.ku.lt:3838/EcoNeTool/`

---

**That's it! Your food web explorer is now live! 🐟🌊🎉**
