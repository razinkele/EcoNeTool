# EcoNeTool Tests

This directory contains all test, validation, debug, and fix scripts for the EcoNeTool application.

## Test Suites

### Habitat Map Loading Tests (NEW)
Comprehensive tests for EMODnet EUSeaMap habitat data loading functionality.

**Files:**
- `test_habitat_functions.R` - Unit tests for habitat loading functions (~30s)
- `test_habitat_loading.R` - Integration tests across all regions/BBTs (~3 min)
- `run_tests.R` - Test runner for all habitat tests

**Quick Start:**
```bash
# Run all habitat tests
Rscript tests/run_tests.R

# Run unit tests only
Rscript tests/test_habitat_functions.R

# Run integration tests only
Rscript tests/test_habitat_loading.R
```

**Coverage:**
- ✅ All 5 European regions (Baltic, North Sea, Mediterranean, Atlantic, Arctic)
- ✅ 5+ BBT-specific areas
- ✅ Custom bbox sizes
- ✅ Invalid geometry handling
- ✅ Performance benchmarks
- ✅ Edge cases

See [test_habitat_loading.R](test_habitat_loading.R) and [test_habitat_functions.R](test_habitat_functions.R) for details.

---

## Directory Structure

### `unit/`
Unit tests for individual functions and components. These scripts test specific functionality in isolation.
- `test_auxillary_comments.R`
- `test_balance_na_fixes.R`
- `test_complete_enhanced_features.R`
- `test_ecopath_results_table.R`
- `test_enhanced_import.R`
- `test_final_balancing.R`
- `test_producer_validation.R`
- `test_results_highlighting.R`
- `test_rpath_fixes.R`
- `test_rpath_fixes_simple.R`
- `test_rpath_frontend_improvements.R`
- `test_rpath_refactoring.R`
- `test_rpath_ui_improvements.R`

### `validation/`
Validation scripts that check data integrity and model requirements.
- `check_circular_feeding_deep.R`
- `check_diet_matrix_structure.R`
- `check_rpath_requirements.R`
- `check_tl_in_database.R`
- `compare_model_structures.R`

### `debug/`
Debugging and diagnostic scripts for troubleshooting issues.
- `debug_rpath_structure.R`
- `diagnose_trophic_levels.R`
- `inspect_balanced_model.R`
- `investigate_comments_tooltips.R`
- `investigate_ewe_data.R`

### `fixes/`
Scripts that contain fixes and analyses for specific issues.
- `analyze_files.R`
- `fix_phytoplankton_ee.R`

## Running Tests

To run all unit tests:
```r
source("tests/unit/test_*.R")
```

To run specific validation checks:
```r
source("tests/validation/check_rpath_requirements.R")
```

## Future Improvements

- [ ] Migrate to `testthat` framework for proper unit testing
- [ ] Add CI/CD integration
- [ ] Create test coverage reports
- [ ] Consolidate overlapping tests
- [ ] Add integration tests
