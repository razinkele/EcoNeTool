# EcoNeTool v1.0.19 Deployment Checklist

**Version**: 1.0.19 - visNetwork Label & Topology Fix
**Date**: 2025-12-08
**Critical Fix**: Network graph node labels

---

## 📋 Pre-Deployment

### 1. Version Verification
- [ ] `VERSION` file shows 1.0.19
- [ ] `CHANGELOG.md` includes v1.0.19 entry
- [ ] `README.md` shows recent updates

### 2. Code Validation
- [ ] Run syntax check:
  ```bash
  cd R/functions
  Rscript -e "source('network_visualization.R'); cat('✓ Syntax OK\n')"
  ```

### 3. Test Scripts
- [ ] Run vertex name test:
  ```bash
  Rscript test_vertex_names.R
  ```
  Expected: ✓ SUCCESS: Vertex names are PROPER SPECIES NAMES

- [ ] Run visNetwork label test:
  ```bash
  Rscript test_visnetwork_labels.R
  ```
  Expected: ✓ SUCCESS: Node labels are SPECIES NAMES

- [ ] Run node structure test:
  ```bash
  Rscript test_node_structure.R
  ```
  Expected: ✓ All structure checks passed!

### 4. Local Testing
- [ ] Start app locally: `shiny::runApp()`
- [ ] Navigate to: Food Web Network → Interactive Network
- [ ] Verify nodes show species names (not numbers)
- [ ] Verify trophic level layering (vertical)
- [ ] Verify horizontal spacing (no overlap)
- [ ] Check Biomass Analysis → Biomass Network
- [ ] Check Energy Fluxes → Flux Network
- [ ] Test node selection dropdown
- [ ] Test tooltips on hover

### 5. Pre-Deploy Check Script
- [ ] Run comprehensive validation:
  ```bash
  cd deployment
  Rscript pre-deploy-check.R
  ```
  Expected: ✅ ALL CHECKS PASSED

---

## 🚀 Deployment Steps

### Option A: Shiny Server Deployment

1. **Backup Current Version**
   ```bash
   sudo cp -r /srv/shiny-server/EcoNeTool /srv/shiny-server/EcoNeTool.backup.$(date +%Y%m%d_%H%M%S)
   ```
   - [ ] Backup created successfully

2. **Run Deployment Script**
   ```bash
   cd deployment
   sudo ./deploy.sh --shiny-server
   ```
   - [ ] Deployment script completed
   - [ ] No errors reported

3. **Verify File Deployment**
   ```bash
   ls -la /srv/shiny-server/EcoNeTool/R/functions/network_visualization.R
   ```
   - [ ] File exists and is recent

4. **Check Shiny Server Status**
   ```bash
   sudo systemctl status shiny-server
   ```
   - [ ] Service is active (running)

5. **Force Reload** (if needed)
   ```bash
   cd deployment
   sudo ./force-reload.sh
   ```
   - [ ] App reloaded successfully

### Option B: Docker Deployment

1. **Build Docker Image**
   ```bash
   cd deployment
   ./deploy.sh --docker
   ```
   - [ ] Image built successfully

2. **Start Container**
   ```bash
   docker-compose up -d
   ```
   - [ ] Container started

3. **Check Container Logs**
   ```bash
   docker logs econetool
   ```
   - [ ] No errors in logs

---

## ✅ Post-Deployment Verification

### 1. Application Access
- [ ] Access app URL: http://laguna.ku.lt:3838/EcoNeTool/
- [ ] Dashboard loads without errors
- [ ] No console errors in browser (F12)

### 2. Network Visualization Tests

#### Interactive Network Tab
- [ ] Navigate to: Food Web Network → Interactive Network
- [ ] Nodes display species names (e.g., "Synchaeta", "Acartia", "Clupea harengus")
- [ ] NOT displaying numbers (1, 2, 3...)
- [ ] Trophic levels properly layered vertically
- [ ] Basal species at bottom
- [ ] Top predators at top
- [ ] Good horizontal spacing between nodes
- [ ] No overlapping nodes
- [ ] Tooltips show full species information
- [ ] Node selection dropdown lists species names

#### Biomass Network Tab
- [ ] Navigate to: Biomass Analysis → Network Visualization
- [ ] Node sizes reflect biomass (larger = more biomass)
- [ ] Species names displayed on nodes
- [ ] Colors represent functional groups
- [ ] Trophic level layout preserved

#### Flux Network Tab
- [ ] Navigate to: Energy Fluxes → Flux Network
- [ ] Edge widths reflect flux magnitudes
- [ ] Species names displayed on nodes
- [ ] Tooltips show flux values
- [ ] Layout is readable and clear

### 3. Data Import Tests
- [ ] Test ECOPATH database import
- [ ] Verify imported network shows species names
- [ ] Test RData file upload
- [ ] Test CSV import (if applicable)

### 4. Performance Check
- [ ] Page load time < 3 seconds
- [ ] Network rendering time < 2 seconds
- [ ] No memory leaks (monitor for 5 minutes)
- [ ] No R process crashes

### 5. Cross-Browser Testing
- [ ] Chrome/Edge - Network displays correctly
- [ ] Firefox - Network displays correctly
- [ ] Safari - Network displays correctly (if available)

---

## 🔧 Troubleshooting

### Issue: Nodes still showing numbers

**Solutions**:
1. Clear browser cache: Ctrl+F5
2. Force reload app:
   ```bash
   cd deployment
   sudo ./force-reload.sh
   ```
3. Check file deployment:
   ```bash
   sudo ls -lh /srv/shiny-server/EcoNeTool/R/functions/network_visualization.R
   ```
4. Restart Shiny Server:
   ```bash
   sudo systemctl restart shiny-server
   ```

### Issue: Topology looks broken

**Check**:
1. Verify fixed parameter structure in nodes data
2. Run test: `Rscript test_node_structure.R`
3. Check browser console for JavaScript errors

### Issue: App won't start

**Solutions**:
1. Check Shiny Server logs:
   ```bash
   sudo tail -f /var/log/shiny-server.log
   ```
2. Check app-specific logs:
   ```bash
   sudo tail -f /var/log/shiny-server/EcoNeTool-*.log
   ```
3. Verify R packages installed:
   ```bash
   Rscript -e "library(visNetwork); library(igraph); cat('OK\n')"
   ```

### Issue: Performance degraded

**Solutions**:
1. Clear R cache
2. Restart Shiny Server
3. Check server resources (RAM, CPU)
4. Verify no memory leaks

---

## 📊 Rollback Procedure

If deployment fails or issues are found:

1. **Stop Shiny Server**
   ```bash
   sudo systemctl stop shiny-server
   ```

2. **Restore Backup**
   ```bash
   # Find your backup
   ls -lh /srv/shiny-server/EcoNeTool.backup.*

   # Restore
   sudo rm -rf /srv/shiny-server/EcoNeTool
   sudo cp -r /srv/shiny-server/EcoNeTool.backup.YYYYMMDD_HHMMSS /srv/shiny-server/EcoNeTool
   ```

3. **Restart Shiny Server**
   ```bash
   sudo systemctl start shiny-server
   ```

4. **Verify Rollback**
   - Access app and check version number
   - Verify functionality

---

## 📝 Deployment Sign-Off

**Deployment Details**:
- Deployed By: ___________________
- Deployment Date: ___________________
- Deployment Time: ___________________
- Version Confirmed: v1.0.19 ⬜

**Verification**:
- All pre-deployment checks passed: ⬜
- All post-deployment checks passed: ⬜
- Network graphs display species names: ⬜
- No critical errors observed: ⬜
- Performance acceptable: ⬜

**Approval**:
- Deployment Approved By: ___________________
- Signature: ___________________
- Date: ___________________

---

## 📞 Support Contacts

- **Technical Issues**: [GitHub Issues](https://github.com/razinkele/EcoNeTool/issues)
- **Documentation**: See `RELEASE_NOTES_v1.0.19.md`
- **Rollback**: See Rollback Procedure section above

---

## 📚 Additional Resources

- `CHANGELOG.md` - Complete version history
- `VERSION` - Version tracking file
- `RELEASE_NOTES_v1.0.19.md` - Detailed release notes
- `VISNETWORK_FIX_COMPLETE.md` - Technical fix details
- `TOPOLOGY_FIX_COMPLETE.md` - Topology preservation guide

---

**Deployment Status**: ⬜ In Progress | ⬜ Completed | ⬜ Rolled Back
