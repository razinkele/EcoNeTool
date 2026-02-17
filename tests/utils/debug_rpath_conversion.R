# Debug Rpath conversion issue with LTgoby

cat("Loading functions...\n")
source("ecopath_windows_import.R")
source("rpath_integration.R")

cat("\nImporting LTgoby...\n")
data <- parse_ecopath_native_cross_platform('examples/LTgoby.eweaccdb')

cat("\nGroup Types:\n")
print(data$group_data[, c('GroupID', 'GroupName', 'Type', 'Biomass', 'ProdBiom', 'ConsBiom')])

cat("\nFiltering living groups (Type >= 0)...\n")
living_groups <- data$group_data[data$group_data$Type >= 0, ]
cat("Living groups:", nrow(living_groups), "\n")
print(living_groups[, c('GroupID', 'GroupName', 'Type')])

cat("\nAttempting to create Rpath params with detailed tracing...\n")
cat("Group names:\n")
print(living_groups$GroupName)

cat("\nGroup types:\n")
print(living_groups$Type)

cat("\nCalling Rpath::create.rpath.params()...\n")
tryCatch({
  params <- Rpath::create.rpath.params(
    group = living_groups$GroupName,
    type = living_groups$Type,
    stgroup = living_groups$GroupName
  )
  cat("✓ Success!\n")
  cat("Params structure:\n")
  str(params$model)
}, error = function(e) {
  cat("✗ Error in create.rpath.params:\n")
  cat("Message:", e$message, "\n")
  cat("\nChecking for NA values in inputs:\n")
  cat("  GroupName NAs:", sum(is.na(living_groups$GroupName)), "\n")
  cat("  Type NAs:", sum(is.na(living_groups$Type)), "\n")
  cat("\nChecking for factor issues:\n")
  cat("  GroupName class:", class(living_groups$GroupName), "\n")
  cat("  Type class:", class(living_groups$Type), "\n")
})
