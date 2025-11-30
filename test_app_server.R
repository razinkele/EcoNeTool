# Test if app.R loads on server
tryCatch({
  source("app.R")
  cat("SUCCESS: App loaded without errors\n")
}, error = function(e) {
  cat("ERROR loading app:\n")
  cat(e$message, "\n")
  traceback()
})
