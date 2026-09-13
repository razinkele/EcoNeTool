# =============================================================================
# deployment/deploy.sh must not destroy server-only state
# =============================================================================
# This is the third deploy script (the others are ./deploy.sh and
# ./deploy-windows.ps1). It wiped the deploy tree with
# `rm -rf /srv/shiny-server/EcoNeTool/*` and then copied back only the names in
# CRITICAL_ITEMS. Two runtime-critical directories were in neither list:
#
#   r-libs/  app-local R library (icesSAG); server-only, absent from the repo,
#            and made discoverable at app.R:9. Recoverable only by unpacking
#            the backup tar by hand.
#   models/  trait_ml_models.rds, loaded by ml_trait_prediction.R:83. Tracked
#            in git, so the fix is simply to deploy it.
#
# Bash is not exercised by this suite, so these are source guards. The delete
# behaviour itself was verified separately against a scratch tree.

source_app_dependencies()

deploy_sh <- function() {
  readLines(app_path("deployment/deploy.sh"), warn = FALSE)
}

test_that("deploy.sh no longer wipes the deploy tree unconditionally", {
  code <- deploy_sh()
  code <- code[!startsWith(trimws(code), "#")]

  offenders <- grep("rm -rf /srv/shiny-server/EcoNeTool/*", code,
                    value = TRUE, fixed = TRUE)

  expect_equal(length(offenders), 0L,
               label = paste("unconditional wipe:",
                             paste(trimws(offenders), collapse = " | ")))
})

test_that("deploy.sh preserves the server-only state it cannot restore", {
  code <- paste(deploy_sh(), collapse = "\n")

  for (keep in c("r-libs", "cache", "restart.txt")) {
    expect_true(grepl(keep, code, fixed = TRUE),
                info = paste("no preserve entry for", keep))
  }
})

test_that("deploy.sh keeps dotfiles, so .Renviron survives", {
  # The old `rm -rf dir/*` never matched dotfiles. A find-based delete does,
  # so dropping .Renviron would be a regression introduced by the fix itself.
  code <- paste(deploy_sh(), collapse = "\n")
  expect_true(grepl("-name '.*'", code, fixed = TRUE) ||
                grepl('-name ".*"', code, fixed = TRUE) ||
                grepl(".Renviron", code, fixed = TRUE),
              info = "nothing protects dotfiles from the find-based delete")
})

test_that("deploy.sh deploys models/, which the ML tier loads at runtime", {
  code <- deploy_sh()
  in_list <- which(trimws(code) == '"models"')

  expect_true(length(in_list) > 0,
              info = "models/ missing from CRITICAL_ITEMS; ml_trait_prediction.R would find no model file")
})
