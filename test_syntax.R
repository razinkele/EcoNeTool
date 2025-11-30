tryCatch({
  source("app.R")
  cat("SUCCESS: App loads with new visualization!\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})
