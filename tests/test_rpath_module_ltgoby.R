# Rpath Module Testing Suite - LTgoby ECOPATH Model
# Test Coverage: Import, Conversion, Mass Balance, MTI, Ecosim
# Date: 2025-12-08

suppressPackageStartupMessages({
  library(shiny)
})

# Configuration
TEST_FILE <- "examples/LTgoby.eweaccdb"
OUTPUT_DIR <- "output/rpath_tests"
TIMESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

if (!dir.exists("output")) dir.create("output")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

test_results <- list()
test_counter <- 0

run_test <- function(test_name, test_function) {
  test_counter <<- test_counter + 1
  cat(sprintf("\n[Test %d] %s\n", test_counter, test_name))
  cat(strrep("=", 70), "\n")
  start_time <- Sys.time()
  result <- tryCatch({
    test_function()
    list(status = "PASS", error = NULL)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    list(status = "FAIL", error = e$message)
  })
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  test_results[[test_counter]] <<- list(
    test_number = test_counter, test_name = test_name,
    status = result$status, error = result$error,
    duration = duration, timestamp = start_time
  )
  if (result$status == "PASS") {
    cat(sprintf("PASSED (%.2f seconds)\n", duration))
  }
  return(result$status == "PASS")
}

cat("\n", strrep("=", 70), "\n")
cat("RPATH MODULE TEST SUITE - LTgoby Model\n")
cat(strrep("=", 70), "\n\n")

cat("Loading required files...\n")
source("ecopath_windows_import.R")
source("rpath_integration.R")
source("R/modules/rpath_module.R")
cat("Files loaded\n")

# TEST 1: File Existence
ecopath_data <- NULL
run_test("File Existence Check", function() {
  if (!file.exists(TEST_FILE)) stop(paste("File not found:", TEST_FILE))
  cat("Test file found:", TEST_FILE, "\n")
  cat("File size:", round(file.info(TEST_FILE)$size / 1024, 2), "KB\n")
})

# TEST 2: ECOPATH Import
run_test("ECOPATH Import", function() {
  ecopath_data <<- parse_ecopath_native_cross_platform(TEST_FILE)
  if (is.null(ecopath_data)) stop("Import returned NULL")
  required_fields <- c("group_data", "diet_data", "metadata", "tables")
  missing <- setdiff(required_fields, names(ecopath_data))
  if (length(missing) > 0) stop(paste("Missing fields:", paste(missing, collapse = ", ")))
  cat("Import successful\n")
  cat("Method:", ecopath_data$method, "\n")
  cat("Groups:", nrow(ecopath_data$group_data), "\n")
  cat("Diet links:", nrow(ecopath_data$diet_data), "\n")
})

# TEST 3: Rpath Package
rpath_available <- FALSE
run_test("Rpath Package Check", function() {
  rpath_available <<- requireNamespace("Rpath", quietly = TRUE)
  if (!rpath_available) {
    cat("Rpath not installed - skipping Rpath tests\n")
    return()
  }
  cat("Rpath available, version:", as.character(packageVersion("Rpath")), "\n")
})

# TEST 4: Conversion to Rpath
rpath_params <- NULL
if (rpath_available) {
  run_test("Conversion to Rpath Format", function() {
    rpath_params <<- convert_ecopath_to_rpath(ecopath_data)
    if (is.null(rpath_params)) stop("Conversion returned NULL")
    if (!inherits(rpath_params, "Rpath.params")) stop("Not an Rpath.params object")
    cat("Conversion successful\n")
    cat("Groups:", nrow(rpath_params$model), "\n")
    saveRDS(rpath_params, file.path(OUTPUT_DIR, paste0("rpath_params_", TIMESTAMP, ".rds")))
  })
}

# TEST 5: Mass Balance
ecopath_model <- NULL
if (rpath_available && !is.null(rpath_params)) {
  run_test("Mass Balance Model", function() {
    ecopath_model <<- Rpath::rpath(rpath_params)
    if (is.null(ecopath_model)) stop("Mass balance returned NULL")
    cat("Mass balance successful\n")
    cat("Total biomass:", round(sum(ecopath_model$Biomass, na.rm = TRUE), 2), "\n")
    saveRDS(ecopath_model, file.path(OUTPUT_DIR, paste0("ecopath_model_", TIMESTAMP, ".rds")))
  })
}

# TEST 6: MTI Analysis
mti_result <- NULL
if (rpath_available && !is.null(ecopath_model) && !is.null(rpath_params)) {
  run_test("MTI Analysis", function() {
    mti_result <<- Rpath::MTI(ecopath_model, rpath_params)
    if (is.null(mti_result)) stop("MTI returned NULL")
    if (!is.matrix(mti_result)) stop("MTI is not a matrix")
    cat("MTI successful\n")
    cat("Matrix dimensions:", nrow(mti_result), "x", ncol(mti_result), "\n")
    saveRDS(mti_result, file.path(OUTPUT_DIR, paste0("mti_matrix_", TIMESTAMP, ".rds")))
  })
}

# TEST 7: Ecosim Simulation
ecosim_result <- NULL
if (rpath_available && !is.null(ecopath_model) && !is.null(rpath_params)) {
  run_test("Ecosim Simulation (10 years)", function() {
    # Create scenario - rsim.scenario expects the full Rpath.params object, not just stanzas
    cat("Creating scenario...\n")
    ecosim_params <- Rpath::rsim.scenario(ecopath_model, rpath_params, years = 1:10)
    if (is.null(ecosim_params)) stop("Failed to create scenario")
    cat("Running simulation...\n")
    ecosim_result <<- Rpath::rsim.run(ecosim_params, method = 'RK4')
    if (is.null(ecosim_result)) stop("Simulation returned NULL")
    cat("Simulation successful\n")
    cat("Years simulated:", length(unique(ecosim_result$out_Biomass[, 1])), "\n")
    saveRDS(ecosim_result, file.path(OUTPUT_DIR, paste0("ecosim_result_", TIMESTAMP, ".rds")))
  })
}

# SUMMARY
cat("\n", strrep("=", 70), "\n")
cat("TEST SUMMARY\n")
cat(strrep("=", 70), "\n")
total <- length(test_results)
passed <- sum(sapply(test_results, function(x) x$status == "PASS"))
failed <- total - passed
cat(sprintf("Total: %d | Passed: %d (%.1f%%) | Failed: %d\n", 
            total, passed, 100*passed/total, failed))
cat(sprintf("Total Time: %.2f seconds\n", 
            sum(sapply(test_results, function(x) x$duration))))

if (failed > 0) {
  cat("\nFailed Tests:\n")
  for (r in test_results) {
    if (r$status == "FAIL") {
      cat(sprintf("  [%d] %s - %s\n", r$test_number, r$test_name, r$error))
    }
  }
}

# Save report
report_file <- file.path(OUTPUT_DIR, paste0("test_report_", TIMESTAMP, ".txt"))
sink(report_file)
cat("RPATH MODULE TEST REPORT\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("File:", TEST_FILE, "\n\n")
for (r in test_results) {
  cat(sprintf("[%d] %s: %s (%.2fs)\n", r$test_number, r$test_name, r$status, r$duration))
  if (!is.null(r$error)) cat("  Error:", r$error, "\n")
}
cat(sprintf("\nSummary: %d/%d passed\n", passed, total))
sink()

cat("\nReport saved:", report_file, "\n")
cat("Output directory:", OUTPUT_DIR, "\n\n")

if (!rpath_available) {
  cat("NOTE: Install Rpath for full testing: install.packages('Rpath')\n\n")
}
