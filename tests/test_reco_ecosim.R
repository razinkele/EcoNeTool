# Test Ecosim with REco example to understand proper usage

library(Rpath)

cat("Loading REco example data...\n")
data('REco.params')

cat("Running Ecopath...\n")
m <- rpath(REco.params)

cat("Checking stanza structure:\n")
cat("NStanzaGroups:", REco.params$stanzas$NStanzaGroups, "\n")
cat("Stanza groups:\n")
print(REco.params$stanzas$stgroups)

cat("\nCreating scenario...\n")
tryCatch({
  sc <- rsim.scenario(m, REco.params$stanzas$stgroups, years = 1:10)
  cat("✓ Scenario created!\n")
  cat("Class:", class(sc), "\n")

  cat("\nRunning simulation...\n")
  result <- rsim.run(sc, method = 'RK4')
  cat("✓ Simulation complete!\n")
  cat("Output dims:", dim(result$out_Biomass), "\n")

}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
  print(traceback())
})
