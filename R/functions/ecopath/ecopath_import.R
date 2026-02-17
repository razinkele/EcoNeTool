# ==============================================================================
# ECOPATH CROSS-PLATFORM IMPORT
# ==============================================================================
# Main entry point for ECOPATH database import
#
# Features:
#   - Automatic platform detection (Windows vs Linux/Mac)
#   - Seamless cross-platform database reading
#   - Support for .ewemdb, .eweaccdb, .mdb, .eiidb, .accdb files
#
# Usage:
#   result <- parse_ecopath_native_cross_platform("your-file.ewemdb")
#
# ==============================================================================

parse_ecopath_native_cross_platform <- function(db_file) {
  #' Cross-Platform ECOPATH Database Parser
  #'
  #' Automatically detects operating system and uses appropriate method:
  #' - Windows: RODBC
  #' - Linux/Mac: Hmisc + mdbtools
  #'
  #' @param db_file Path to ECOPATH database file (.ewemdb, .eweaccdb, .mdb, .eiidb, .accdb)
  #' @return List containing group_data, diet_data, and metadata
  #' @export

  # Detect operating system
  os_type <- Sys.info()["sysname"]

  message("Operating System: ", os_type)
  message("Database file: ", basename(db_file))
  message("File size: ", round(file.size(db_file) / 1024 / 1024, 2), " MB")

  if (os_type == "Windows") {
    message("Using Windows RODBC method...")
    result <- parse_ecopath_native_windows(db_file)

  } else {
    message("Using Linux/Mac mdbtools method...")
    result <- parse_ecopath_native_linux(db_file)
  }

  return(result)
}
