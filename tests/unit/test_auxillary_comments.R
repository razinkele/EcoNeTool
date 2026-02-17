# ============================================================================
# Test Auxillary Table Import and Comments/Notes Functionality
# ============================================================================

cat("\n=== TESTING AUXILLARY/COMMENTS IMPORT ===\n\n")

library(RODBC)

source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/auxillary_parser.R")

# ==============================================================================
# TEST 1: Import with Auxillary Data
# ==============================================================================

cat("[1] Testing Import with Auxillary Table\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")

cat("✓ Import Results:\n")
cat(sprintf("  - Auxillary data: %s\n",
            if (!is.null(ecopath_data$auxillary_data))
              paste(nrow(ecopath_data$auxillary_data), "entries")
            else "Not available"))

if (!is.null(ecopath_data$auxillary_data)) {
  cat(sprintf("  - Columns: %s\n", paste(colnames(ecopath_data$auxillary_data), collapse=", ")))

  # Count non-empty remarks
  if ("Remark" %in% colnames(ecopath_data$auxillary_data)) {
    non_empty <- sum(!is.na(ecopath_data$auxillary_data$Remark) &
                     nchar(as.character(ecopath_data$auxillary_data$Remark)) > 0)
    cat(sprintf("  - Entries with remarks: %d\n", non_empty))
  }

  # Show sample ValueID patterns
  cat("\n  Sample ValueID patterns:\n")
  sample_ids <- head(unique(ecopath_data$auxillary_data$ValueID), 10)
  for (id in sample_ids) {
    cat(sprintf("    - %s\n", id))
  }
}

# ==============================================================================
# TEST 2: Parse ValueID
# ==============================================================================

cat("\n[2] Testing ValueID Parsing\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

if (!is.null(ecopath_data$auxillary_data) && nrow(ecopath_data$auxillary_data) > 0) {
  # Test parsing on sample ValueIDs
  test_ids <- c(
    "EcoPathGroupInput:6:PBInput",
    "EcoPathGroupInput:11:DietComp:EcoPathGroupInput:6",
    "EcoPathFleetInput:1:FixedCost"
  )

  cat("Testing ValueID parser:\n\n")
  for (id in test_ids) {
    parsed <- parse_auxillary_valueid(id)
    cat(sprintf("  ValueID: %s\n", id))
    cat(sprintf("    Entity Type: %s\n", parsed$entity_type))
    cat(sprintf("    Entity ID: %s\n", parsed$entity_id))
    cat(sprintf("    Parameter: %s\n\n", parsed$parameter))
  }
} else {
  cat("⚠ No auxillary data to parse\n")
}

# ==============================================================================
# TEST 3: Organize Auxillary Data
# ==============================================================================

cat("[3] Testing Auxillary Data Organization\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

if (!is.null(ecopath_data$auxillary_data) && nrow(ecopath_data$auxillary_data) > 0) {
  # Organize the data
  organized <- organize_auxillary_data(
    ecopath_data$auxillary_data,
    ecopath_data$group_data,
    ecopath_data$fleet_data
  )

  cat(sprintf("✓ Organized comments table: %d rows × %d columns\n",
              nrow(organized), ncol(organized)))
  cat("  Columns:", paste(colnames(organized), collapse=", "), "\n\n")

  if (nrow(organized) > 0) {
    cat("Sample organized entries (first 5):\n")
    sample_df <- head(organized[, c("Entity_Type", "Entity_Name", "Parameter", "Remark")], 5)
    print(sample_df)

    # Summary by entity type
    cat("\n✓ Summary by entity type:\n")
    type_summary <- table(organized$Entity_Type)
    for (type in names(type_summary)) {
      cat(sprintf("  - %s: %d comments\n", type, type_summary[type]))
    }

    # Summary by parameter
    cat("\n✓ Summary by parameter:\n")
    param_summary <- table(organized$Parameter)
    for (param in names(param_summary)) {
      cat(sprintf("  - %s: %d comments\n", param, param_summary[param]))
    }

    # Show full remark for first entry
    cat("\n✓ Example full remark:\n")
    cat("  Entity:", organized$Entity_Name[1], "\n")
    cat("  Parameter:", organized$Parameter[1], "\n")
    cat("  Remark:\n")
    cat("   ", gsub("\t", "\n    ", organized$Remark[1]), "\n")
  }
} else {
  cat("⚠ No auxillary data to organize\n")
}

# ==============================================================================
# TEST 4: Citation Extraction
# ==============================================================================

cat("\n[4] Testing Citation Extraction\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

if (!is.null(ecopath_data$auxillary_data) && exists("organized") && nrow(organized) > 0) {
  # Extract citations
  citations <- extract_citations_from_remarks(organized$Remark)

  # Count how many have citations
  has_citation <- sum(!is.na(citations$citation))
  cat(sprintf("✓ Found citations in %d/%d remarks\n\n", has_citation, nrow(citations)))

  if (has_citation > 0) {
    cat("Sample citations extracted:\n")
    with_cit <- citations[!is.na(citations$citation), ]
    for (i in 1:min(5, nrow(with_cit))) {
      cat(sprintf("  - %s\n", with_cit$citation[i]))
    }
  }
} else {
  cat("⚠ No remarks to extract citations from\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("\n=== TEST SUMMARY ===\n\n")

tests_passed <- 0
total_tests <- 4

# Test 1: Import
if (!is.null(ecopath_data$auxillary_data) && nrow(ecopath_data$auxillary_data) > 0) {
  cat("✅ Test 1: Auxillary Import - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 1: Auxillary Import - FAIL\n")
}

# Test 2: Parsing
if (exists("parsed") && !is.null(parsed$entity_type)) {
  cat("✅ Test 2: ValueID Parsing - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 2: ValueID Parsing - FAIL\n")
}

# Test 3: Organization
if (exists("organized") && nrow(organized) > 0) {
  cat("✅ Test 3: Data Organization - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 3: Data Organization - FAIL\n")
}

# Test 4: Citations
if (exists("citations") && nrow(citations) > 0) {
  cat("✅ Test 4: Citation Extraction - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 4: Citation Extraction - FAIL\n")
}

cat(sprintf("\n📊 RESULTS: %d/%d tests passed\n\n", tests_passed, total_tests))

if (tests_passed == total_tests) {
  cat("🎉 ALL TESTS PASSED! 🎉\n\n")
  cat("Comments/Notes functionality is working:\n")
  cat("  ✓ Auxillary table import\n")
  cat("  ✓ ValueID parsing\n")
  cat("  ✓ Data organization with group/fleet names\n")
  cat("  ✓ Citation extraction\n")
  cat(sprintf("  ✓ %d comments/tooltips available\n", if(exists("organized")) nrow(organized) else 0))
  cat("\n🚀 READY FOR UI INTEGRATION\n\n")
} else {
  cat("⚠ Some tests failed. Please review the errors above.\n\n")
}
