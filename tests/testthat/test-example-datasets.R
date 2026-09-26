# =============================================================================
# Bundled example datasets follow the prey -> predator edge contract
# =============================================================================
# The Import panel serves examples/*.Rdata and examples/*_network.csv for
# download, and its help text says network CSV rows = prey, columns =
# predators. Before the A1 final fix wave Simple_3Species and Caribbean_Reef
# were generated predator -> prey (rows = predators). LTCoast and BalticFW are
# guards: they were already correct.

suppressPackageStartupMessages(library(igraph))

EXAMPLE_BASAL_PATTERN <- "detrit|phyto|diatom|autotroph|macroalgae|microalgae|macrophyto"

EXAMPLE_NETWORKS <- list(
  Simple_3Species = list(top = "Fish", csv = "Simple_3Species_network.csv"),
  Caribbean_Reef = list(top = "Barracuda", csv = "Caribbean_Reef_network.csv"),
  LTCoast = list(top = "Great cormorant", csv = NULL),
  BalticFW = list(top = "Gadus morhua", csv = NULL)
)

load_example_net <- function(name) {
  path <- file.path(get_app_root(), "examples", paste0(name, ".Rdata"))
  env <- new.env()
  suppressWarnings(suppressMessages({
    load(path, envir = env)
    net <- igraph::upgrade_graph(env$net)
  }))
  net
}

for (name in names(EXAMPLE_NETWORKS)) {
  spec <- EXAMPLE_NETWORKS[[name]]

  test_that(sprintf("example %s.Rdata is prey -> predator", name), {
    net <- load_example_net(name)
    expect_gt(ecount(net), 0)

    basal <- grep(EXAMPLE_BASAL_PATTERN, V(net)$name, ignore.case = TRUE, value = TRUE)
    expect_gt(length(basal), 0)
    in_deg <- degree(net, basal, mode = "in")
    expect_true(all(in_deg == 0),
                label = sprintf("basal groups eat nothing (in-degree 0): %s",
                                paste(names(in_deg)[in_deg > 0], collapse = ", ")))
    expect_true(all(degree(net, basal, mode = "out") > 0),
                label = "basal groups are eaten by someone (out-degree > 0)")

    expect_true(spec$top %in% V(net)$name)
    expect_equal(unname(degree(net, spec$top, mode = "out")), 0,
                 label = sprintf("top predator %s eaten by nobody (out-degree)", spec$top))
  })

  if (!is.null(spec$csv)) {
    test_that(sprintf("example %s has rows = prey, columns = predators", spec$csv), {
      adj <- as.matrix(read.csv(file.path(get_app_root(), "examples", spec$csv),
                                row.names = 1, check.names = FALSE))
      basal <- grep(EXAMPLE_BASAL_PATTERN, rownames(adj), ignore.case = TRUE, value = TRUE)
      expect_gt(length(basal), 0)
      # A producer row lists its predators; a producer column (what it eats) is empty.
      expect_true(all(rowSums(adj[basal, , drop = FALSE]) > 0))
      expect_true(all(colSums(adj[, basal, drop = FALSE]) == 0))
      # The top predator is eaten by nobody: its row is empty.
      expect_equal(sum(adj[spec$top, ]), 0)
      expect_gt(sum(adj[, spec$top]), 0)
    })
  }
}
