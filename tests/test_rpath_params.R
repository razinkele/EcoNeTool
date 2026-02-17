# Test different ways to create Rpath params

library(Rpath)

# Try simple creation without stgroup
cat("Test 1: Without stgroup parameter\n")
tryCatch({
  params1 <- create.rpath.params(
    group = c("Detritus", "Phyto", "Zoo", "Fish"),
    type = c(2, 1, 0, 0)
  )
  cat("✓ Success!\n")
  print(str(params1))
}, error = function(e) {
  cat("✗ Failed:", e$message, "\n")
})

# Try with NA for stgroup
cat("\nTest 2: With NA for stgroup\n")
tryCatch({
  params2 <- create.rpath.params(
    group = c("Detritus", "Phyto", "Zoo", "Fish"),
    type = c(2, 1, 0, 0),
    stgroup = rep(NA, 4)
  )
  cat("✓ Success!\n")
}, error = function(e) {
  cat("✗ Failed:", e$message, "\n")
})

# Try with empty string for stgroup
cat("\nTest 3: With empty strings for stgroup\n")
tryCatch({
  params3 <- create.rpath.params(
    group = c("Detritus", "Phyto", "Zoo", "Fish"),
    type = c(2, 1, 0, 0),
    stgroup = rep("", 4)
  )
  cat("✓ Success!\n")
}, error = function(e) {
  cat("✗ Failed:", e$message, "\n")
})

# Try trimming whitespace
cat("\nTest 4: Trimming group names\n")
tryCatch({
  params4 <- create.rpath.params(
    group = trimws(c("Detritus ", "Phyto", "Zoo ", "Fish")),
    type = c(2, 1, 0, 0)
  )
  cat("✓ Success!\n")
}, error = function(e) {
  cat("✗ Failed:", e$message, "\n")
})
