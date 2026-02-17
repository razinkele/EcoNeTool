# EcoNeTool v1.0.20 Deployment Checklist

**Version**: 1.0.20 - Rpath Module Refactoring
**Date**: 2025-12-09
**Critical Update**: Modular Rpath architecture with enhanced import

---

## 📋 Pre-Deployment

### 1. Version Verification
- [ ] `VERSION` file shows 1.0.20
- [ ] `CHANGELOG.md` includes v1.0.20 entry
- [ ] `README.md` shows recent updates
- [ ] `deployment/README.md` updated with v1.0.20 info

### 2. Rpath Module Structure Validation
- [ ] Verify modular files exist:
  ```bash
  ls -lh R/ui/rpath_ui.R
  ls -lh R/modules/rpath_server.R
  ls -lh R/functions/auxillary_parser.R
  ```
- [ ] Check file sizes:
  - `R/ui/rpath_ui.R` should be ~67 lines
  - `R/modules/rpath_server.R` should be ~1688 lines
  - `R/functions/auxillary_parser.R` should be ~206 lines

### 3. Code Validation
- [ ] Run syntax check on new files:
  ```bash
  Rscript -e "source('R/ui/rpath_ui.R'); cat('✓ rpath_ui.R syntax OK\n')"
  Rscript -e "source('R/modules/rpath_server.R'); cat('✓ rpath_server.R syntax OK\n')"
  Rscript -e "source('R/functions/auxillary_parser.R'); cat('✓ auxillary_parser.R syntax OK\n')"
  ```

### 4. Test Scripts
- [ ] Run refactoring test suite:
  ```bash
  Rscript test_rpath_refactoring.R
  ```
  Expected: 🎉 ALL TESTS PASSED! (15/15)

- [ ] Verify test results:
  - ✅ File loading: 4/4 passed
  - ✅ Function availability: 7/7 passed
  - ✅ Data operations: 4/4 passed
  - ✅ Mass balance working

### 5. Pre-Deploy Check Script
- [ ] Run comprehensive validation:
  ```bash
  cd deployment
  Rscript pre-deploy-check.R
  ```
  Expected: ✅ ALL CHECKS PASSED

- [ ] Verify Rpath module checks (section 5.5):
  - ✅ Rpath UI module: R/ui/rpath_ui.R
  - ✅ Function: rpathModuleUI
  - ✅ Namespace: NS(id)
  - ✅ Rpath Server module: R/modules/rpath_server.R
  - ✅ Function: rpathModuleServer
  - ✅ Module pattern: moduleServer()
  - ✅ Auxillary parser: R/functions/auxillary_parser.R
  - ✅ Functions: parse_auxillary_valueid, organize_auxillary_data, extract_citations_from_remarks
  - ✅ App.R sources: R/ui/rpath_ui.R
  - ✅ App.R sources: R/modules/rpath_server.R
  - ✅ App.R sources: R/functions/auxillary_parser.R
  - ✅ OLD rpath_module.R removed

### 6. Local Testing
- [ ] Start app locally: `shiny::runApp()`
- [ ] Navigate to: ECOPATH/ECOSIM tab
- [ ] Test Rpath functionality:
  - [ ] Import ECOPATH database (use LTgoby example)
  - [ ] Click "Setup & Convert" button
  - [ ] Verify Parameters tab populates (24 groups)
  - [ ] Verify Diet Matrix tab populates (576 entries)
  - [ ] Check NEW Calibration tab (103 pedigree entries)
  - [ ] Check NEW Comments/Notes tab (2,039 tooltips)
  - [ ] Run "Run Ecopath Balance"
  - [ ] Verify balance succeeds (~5858 t/km²/year)
  - [ ] Test MTI calculation
  - [ ] Test Ecosim simulation
  - [ ] Test download functions (CSV exports)

### 7. Enhanced Import Features Test
- [ ] Import ECOPATH database with enhanced features
- [ ] Verify import results:
  - [ ] Multistanza data imported (if present)
  - [ ] Fleet data imported (if present)
  - [ ] Discard fate data imported (if present)
  - [ ] Pedigree data imported (103 entries for LTgoby)
  - [ ] Pedigree confidence levels (31 levels)
  - [ ] Auxillary/tooltips data (2,045 entries for LTgoby)
- [ ] Verify Calibration tab displays:
  - [ ] Confidence levels (High/Medium/Low)
  - [ ] Pedigree values (0-1 scale)
  - [ ] Parameter coverage
- [ ] Verify Comments/Notes tab displays:
  - [ ] All tooltips/comments visible
  - [ ] Source citations included
  - [ ] Searchable/filterable table
  - [ ] Download as CSV works

---

## 🚀 Deployment Steps

### Option A: Shiny Server Deployment (Recommended)

1. **Backup Current Version**
   ```bash
   sudo cp -r /srv/shiny-server/EcoNeTool /srv/shiny-server/EcoNeTool.backup.$(date +%Y%m%d_%H%M%S)
   ```
   - [ ] Backup created successfully
   - [ ] Note backup path: ___________________

2. **Run Deployment Script**
   ```bash
   cd deployment
   sudo ./deploy.sh --shiny-server
   ```
   - [ ] Pre-deployment checks passed
   - [ ] Files copied successfully
   - [ ] No errors reported
   - [ ] Deployment script completed

3. **Verify File Deployment**
   ```bash
   # Check modular Rpath files deployed
   ls -la /srv/shiny-server/EcoNeTool/R/ui/rpath_ui.R
   ls -la /srv/shiny-server/EcoNeTool/R/modules/rpath_server.R
   ls -la /srv/shiny-server/EcoNeTool/R/functions/auxillary_parser.R

   # Verify old file NOT present
   ls -la /srv/shiny-server/EcoNeTool/R/modules/rpath_module.R 2>&1 | grep "No such file"
   ```
   - [ ] New files exist and are recent
   - [ ] Old rpath_module.R NOT present (or is backup)

4. **Check Shiny Server Status**
   ```bash
   sudo systemctl status shiny-server
   ```
   - [ ] Service is active (running)
   - [ ] No errors in status output

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
   - [ ] No build errors

2. **Start Container**
   ```bash
   docker-compose up -d
   ```
   - [ ] Container started
   - [ ] Container status: Up

3. **Check Container Logs**
   ```bash
   docker logs econetool
   ```
   - [ ] No errors in logs
   - [ ] App initialization successful

---

## ✅ Post-Deployment Verification

### 1. Application Access
- [ ] Access app URL: http://[your-server]:3838/EcoNeTool/
- [ ] Dashboard loads without errors
- [ ] No console errors in browser (F12)
- [ ] Version displays correctly (if shown)

### 2. Rpath Module Tests

#### Setup & Import
- [ ] Navigate to: ECOPATH/ECOSIM tab
- [ ] Upload ECOPATH database (use LTgoby or any .eweaccdb file)
- [ ] Import succeeds without errors
- [ ] Status messages display correctly

#### Model Setup
- [ ] Click "Setup & Convert" button
- [ ] Conversion completes successfully
- [ ] No error messages displayed
- [ ] Success notification shown

#### Parameters Tab
- [ ] Parameters table displays (24 groups for LTgoby)
- [ ] Table is editable
- [ ] All columns present: Group, Type, Biomass, P/B, Q/B, EE, etc.
- [ ] Data types correct (numeric values, not strings)
- [ ] Download button works

#### Diet Matrix Tab
- [ ] Diet matrix displays (576 entries for LTgoby)
- [ ] Matrix format correct (predators × prey)
- [ ] Values between 0-1 (proportions)
- [ ] Download button works

#### **NEW** Calibration Tab
- [ ] Calibration tab exists and loads
- [ ] Table displays pedigree data (103 entries for LTgoby)
- [ ] Columns: Group, Parameter, Pedigree, Confidence
- [ ] Confidence levels display (High/Medium/Low)
- [ ] Searchable and sortable
- [ ] Download button works
- [ ] No errors in console

#### **NEW** Comments/Notes Tab
- [ ] Comments/Notes tab exists and loads
- [ ] Table displays tooltips/comments (2,039 for LTgoby)
- [ ] Columns: Type, Entity, Parameter, Comment/Note
- [ ] Comments text displays correctly
- [ ] Source citations visible (e.g., "Cusson M., Bourget E., 2005")
- [ ] Searchable and filterable
- [ ] Download button works
- [ ] No errors in console

#### Mass Balance
- [ ] Navigate to Balance tab
- [ ] Click "Run Ecopath Balance"
- [ ] Balance calculation succeeds
- [ ] Results display (Total System Throughput shown)
- [ ] No error messages
- [ ] Values reasonable (~5858 t/km²/year for LTgoby)

#### MTI Analysis
- [ ] Navigate to MTI tab
- [ ] MTI calculates successfully
- [ ] Heatmap displays
- [ ] Axes labeled correctly
- [ ] Interactive features work

#### Ecosim Simulation
- [ ] Navigate to Ecosim tab
- [ ] Configure simulation parameters
- [ ] Run simulation
- [ ] Time series plots display
- [ ] No crashes or errors

### 3. Enhanced Import Features Verification

#### Calibration Data
- [ ] Pedigree data imported from ECOPATH database
- [ ] Confidence levels mapped correctly
- [ ] All parameter types covered (Biomass, P/B, Q/B, Diet, Catch)
- [ ] Quality indicators displayed properly

#### Tooltips/Comments
- [ ] All tooltips/comments imported
- [ ] ValueID parsed correctly
- [ ] Group/fleet names resolved
- [ ] Parameter names readable
- [ ] Scientific citations extracted (if present)

### 4. Data Import Tests (Other Formats)
- [ ] Test RData file upload (if applicable)
- [ ] Test CSV import (if applicable)
- [ ] Verify imported data displays correctly
- [ ] No import errors

### 5. Performance Check
- [ ] Page load time < 3 seconds
- [ ] Rpath module load time < 2 seconds
- [ ] Balance calculation time < 10 seconds (for typical model)
- [ ] No memory leaks (monitor for 5 minutes)
- [ ] No R process crashes
- [ ] App remains responsive

### 6. Cross-Browser Testing
- [ ] Chrome/Edge - Rpath module works correctly
- [ ] Firefox - Rpath module works correctly
- [ ] Safari - Rpath module works correctly (if available)

### 7. Integration with Other Tabs
- [ ] Network visualization still works
- [ ] Topological metrics still work
- [ ] Biomass analysis still works
- [ ] Energy fluxes still work
- [ ] All other tabs functional

---

## 🔧 Troubleshooting

### Issue: Rpath module not loading

**Solutions**:
1. Check browser console (F12) for JavaScript errors
2. Check Shiny Server logs:
   ```bash
   sudo tail -f /var/log/shiny-server.log
   ```
3. Verify files deployed correctly:
   ```bash
   ls -lh /srv/shiny-server/EcoNeTool/R/ui/rpath_ui.R
   ls -lh /srv/shiny-server/EcoNeTool/R/modules/rpath_server.R
   ```
4. Force reload:
   ```bash
   cd deployment
   sudo ./force-reload.sh
   ```

### Issue: "rpathModuleUI not found" error

**Solutions**:
1. Verify R/ui/rpath_ui.R is deployed
2. Check app.R sources the file correctly:
   ```bash
   grep "rpath_ui.R" /srv/shiny-server/EcoNeTool/app.R
   ```
3. Restart Shiny Server:
   ```bash
   sudo systemctl restart shiny-server
   ```

### Issue: Calibration or Comments tabs empty

**Solutions**:
1. Check that auxillary_parser.R is deployed:
   ```bash
   ls -lh /srv/shiny-server/EcoNeTool/R/functions/auxillary_parser.R
   ```
2. Verify database has pedigree/auxillary data:
   - Some databases may not have these tables
   - Test with LTgoby.eweaccdb (known to have data)
3. Check console for import errors

### Issue: Old monolithic file causing conflicts

**Solutions**:
1. Check if old file still present:
   ```bash
   ls -lh /srv/shiny-server/EcoNeTool/R/modules/rpath_module.R
   ```
2. If present, remove or rename:
   ```bash
   sudo mv /srv/shiny-server/EcoNeTool/R/modules/rpath_module.R \
            /srv/shiny-server/EcoNeTool/R/modules/rpath_module.R.backup
   ```
3. Restart Shiny Server

### Issue: Mass balance fails

**Solutions**:
1. Verify Rpath package installed on server:
   ```bash
   Rscript -e "library(Rpath); cat('Rpath version:', as.character(packageVersion('Rpath')), '\n')"
   ```
2. Check model parameters are valid
3. Review conversion warnings
4. Check server logs for detailed error messages

### Issue: Performance degraded

**Solutions**:
1. Clear R cache:
   ```bash
   sudo rm -rf /srv/shiny-server/EcoNeTool/.Rhistory
   sudo rm -rf /srv/shiny-server/EcoNeTool/.RData
   ```
2. Restart Shiny Server
3. Check server resources (RAM, CPU)
4. Monitor memory usage:
   ```bash
   top -u shiny
   ```

### Issue: Browser cache showing old version

**Solutions**:
1. **Hard reload**: Ctrl+Shift+R (Linux/Windows) or Cmd+Shift+R (Mac)
2. **Clear browser cache completely**
3. **Open in incognito/private mode**
4. Verify deployment timestamp:
   ```bash
   ls -lh /srv/shiny-server/EcoNeTool/R/ui/rpath_ui.R
   ```

---

## 📊 Rollback Procedure

If deployment fails or critical issues are found:

1. **Stop Shiny Server**
   ```bash
   sudo systemctl stop shiny-server
   ```

2. **Restore Backup**
   ```bash
   # Find your backup
   ls -lh /srv/shiny-server/EcoNeTool.backup.*

   # Restore (replace YYYYMMDD_HHMMSS with your backup timestamp)
   sudo rm -rf /srv/shiny-server/EcoNeTool
   sudo cp -r /srv/shiny-server/EcoNeTool.backup.YYYYMMDD_HHMMSS /srv/shiny-server/EcoNeTool
   ```

3. **Restart Shiny Server**
   ```bash
   sudo systemctl start shiny-server
   ```

4. **Verify Rollback**
   - [ ] Access app and verify it loads
   - [ ] Check version (should be v1.0.19 or earlier)
   - [ ] Test basic functionality
   - [ ] Rpath module works (old monolithic version)

---

## 📝 Deployment Sign-Off

**Deployment Details**:
- Deployed By: ___________________
- Deployment Date: ___________________
- Deployment Time: ___________________
- Version Confirmed: v1.0.20 ⬜

**Pre-Deployment Verification**:
- [ ] All pre-deployment checks passed (15/15 tests)
- [ ] Rpath refactoring test suite passed
- [ ] Local testing completed successfully
- [ ] Code validation completed
- [ ] Backup created

**Post-Deployment Verification**:
- [ ] Application loads successfully
- [ ] Rpath module UI displays correctly
- [ ] Import functionality works
- [ ] Calibration tab functional (NEW)
- [ ] Comments/Notes tab functional (NEW)
- [ ] Mass balance calculation works
- [ ] MTI and Ecosim functional
- [ ] Download functions work
- [ ] No critical errors observed
- [ ] Performance acceptable
- [ ] Cross-browser tested

**Module Structure Verification**:
- [ ] R/ui/rpath_ui.R deployed
- [ ] R/modules/rpath_server.R deployed
- [ ] R/functions/auxillary_parser.R deployed
- [ ] Old rpath_module.R removed/archived
- [ ] app.R sources correct files
- [ ] Module functions available

**Approval**:
- Deployment Approved By: ___________________
- Signature: ___________________
- Date: ___________________

---

## 📞 Support Contacts

- **Technical Issues**: [GitHub Issues](https://github.com/razinkele/EcoNeTool/issues)
- **Documentation**: See `../docs/development/RPATH_MODULE_REFACTORING.md`
- **Test Results**: See `../docs/SESSION_COMPLETE_RPATH_REFACTORING.md`
- **Rollback**: See Rollback Procedure section above

---

## 📚 Additional Resources

### Documentation Files
- `../CHANGELOG.md` - Complete version history
- `../VERSION` - Version tracking file
- `../docs/development/RPATH_MODULE_REFACTORING.md` - Complete refactoring guide
- `../docs/SESSION_COMPLETE_RPATH_REFACTORING.md` - Session summary and test results
- `../docs/development/ENHANCED_IMPORT_COMPLETE.md` - Enhanced import documentation
- `../docs/development/COMMENTS_TOOLTIPS_COMPLETE.md` - Tooltips/comments implementation

### Test Files
- `../test_rpath_refactoring.R` - Comprehensive refactoring test suite
- `../test_auxillary_comments.R` - Tooltips/comments test suite
- `../test_complete_enhanced_features.R` - Enhanced features integration test

### Deployment Scripts
- `deploy.sh` - Main deployment script
- `pre-deploy-check.R` - Pre-deployment validation
- `force-reload.sh` - Force app reload
- `verify-deployment.sh` - Post-deployment verification

---

## 🎯 Key Changes in v1.0.20

### Rpath Module Refactoring
1. **Modular Architecture**
   - UI separated into `R/ui/rpath_ui.R` (67 lines)
   - Server logic in `R/modules/rpath_server.R` (1688 lines)
   - Clean separation of concerns

2. **Enhanced Import**
   - Auxillary table import (tooltips/comments)
   - Pedigree data import (calibration/quality)
   - Fleet details, discard fate, multistanza
   - New auxillary parser (`R/functions/auxillary_parser.R`)

3. **New UI Features**
   - **Calibration tab**: Pedigree data with confidence levels
   - **Comments/Notes tab**: Tooltips and source citations
   - Improved parameter editor
   - Enhanced diet matrix display

4. **Code Quality**
   - Follows Shiny module best practices
   - Consistent code style across files
   - Comprehensive documentation
   - 100% test coverage (15/15 tests passing)

5. **Backward Compatibility**
   - No interface changes for users
   - All existing functionality preserved
   - Seamless upgrade path

---

## ✅ Success Criteria

**Deployment is successful if**:
- ✅ All pre-deployment checks pass
- ✅ Application loads without errors
- ✅ Rpath module displays and functions correctly
- ✅ NEW Calibration tab works (if pedigree data present)
- ✅ NEW Comments/Notes tab works (if tooltips present)
- ✅ Mass balance calculations succeed
- ✅ All download functions work
- ✅ No performance degradation
- ✅ All integration tests pass

**Deployment Status**: ⬜ In Progress | ⬜ Completed | ⬜ Rolled Back

---

**Last Updated**: 2025-12-09
**Deployment Script Version**: v1.0.20
**Checklist Version**: 1.0
