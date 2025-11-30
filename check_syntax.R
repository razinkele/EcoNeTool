# Check syntax of app.R
cat("Checking app.R syntax...\n")

result <- tryCatch({
  parse("app.R")
  cat("SUCCESS: No syntax errors found in app.R\n")
  TRUE
}, error = function(e) {
  cat("SYNTAX ERROR in app.R:\n")
  cat(e$message, "\n")
  FALSE
})

if (result) {
  cat("\nFile appears to be syntactically correct.\n")
  cat("Try running: library(shiny); runApp()\n")
}
