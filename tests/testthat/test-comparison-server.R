# =============================================================================
# comparison_server(): snapshot slots A/B and the outputs built on them
# =============================================================================
# The module uses the flat input/output pattern (no NS), so shiny::testServer()
# wraps it in a plain server function with the two live reactiveVals. The
# module returns its slots and comparison reactive, which the wrapper keeps as
# `mod` so the test body (evaluated in the wrapper's environment) can read them.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/functional_group_utils.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/trophic_levels.R"), local = FALSE)
  source(file.path(root, "R/functions/topological_metrics.R"), local = FALSE)
  source(file.path(root, "R/functions/network_comparison.R"), local = FALSE)
  source(file.path(root, "R/ui/comparison_ui.R"), local = FALSE)
  source(file.path(root, "R/modules/comparison_server.R"), local = FALSE)
})
suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(igraph)
})

chain_web <- function(names) {
  edges <- as.vector(rbind(names[-length(names)], names[-1]))
  net <- make_graph(edges, directed = TRUE)
  info <- data.frame(species = names, meanB = seq_along(names), stringsAsFactors = FALSE)
  finalize_network(net, info)
}

# Wrapper server: the live reactiveVals are globals the wrapper closes over.
make_app <- function(net_reactive, info_reactive) {
  function(input, output, session) {
    mod <- comparison_server(input, output, session, net_reactive, info_reactive)
  }
}

test_that("storing the current network into A and B yields an identical-web comparison", {
  skip_if_not_installed("shiny")
  live <- chain_web(c("Phyto", "Zoo", "Fish"))
  app <- make_app(reactiveVal(live$net), reactiveVal(live$info))
  testServer(app, {
    session$setInputs(cmp_use_current_a = 1)
    session$setInputs(cmp_use_current_b = 1)
    cmp <- mod$comparison()
    expect_equal(cmp$species$jaccard, 1)
    expect_equal(cmp$links$jaccard, 1)
    expect_true(all(cmp$metrics$delta == 0, na.rm = TRUE))
    expect_match(output$cmp_slot_summary_a, "Species: 3")
    expect_match(output$cmp_species_summary, "Shared \\(3\\)")
    expect_match(output$cmp_links_summary, "Shared links:     2")
  })
})

test_that("slot A is a snapshot: changing the live network afterwards does not alter it", {
  skip_if_not_installed("shiny")
  first <- chain_web(c("Phyto", "Zoo", "Fish"))
  net_reactive <- reactiveVal(first$net)
  info_reactive <- reactiveVal(first$info)
  app <- make_app(net_reactive, info_reactive)
  testServer(app, {
    session$setInputs(cmp_use_current_a = 1)
    other <- chain_web(c("Phyto", "Fish", "Seal"))
    net_reactive(other$net)
    info_reactive(other$info)
    session$setInputs(cmp_use_current_b = 1)
    cmp <- mod$comparison()
    expect_setequal(cmp$species$shared, c("Phyto", "Fish"))
    expect_identical(cmp$species$only_a, "Zoo")
    expect_identical(cmp$species$only_b, "Seal")
    expect_equal(nrow(cmp$per_species), 2)
  })
})

test_that("an example web can be loaded into slot B", {
  skip_if_not_installed("shiny")
  example <- file.path(get_app_root(), "examples", "Simple_3Species.Rdata")
  skip_if(!file.exists(example), "examples/Simple_3Species.Rdata missing")
  live <- chain_web(c("Phyto", "Zoo", "Fish"))
  app <- make_app(reactiveVal(live$net), reactiveVal(live$info))
  testServer(app, {
    session$setInputs(cmp_use_current_a = 1)
    # The select carries the example NAME, never a path.
    session$setInputs(cmp_example_b = "Simple_3Species", cmp_load_example_b = 1)
    cmp <- mod$comparison()
    expect_identical(unname(cmp$labels[["b"]]), "Simple_3Species")
    expect_equal(nrow(cmp$metrics), 11)  # both webs carry meanB -> nw* rows present
    expect_match(output$cmp_slot_summary_b, "Simple_3Species")
  })
})

test_that("a client-supplied path is never loaded: only known example names resolve", {
  skip_if_not_installed("shiny")
  rogue <- tempfile(fileext = ".Rdata")
  net <- make_graph(c("X", "Y"), directed = TRUE)
  info <- data.frame(species = c("X", "Y"), stringsAsFactors = FALSE)
  save(net, info, file = rogue)
  on.exit(unlink(rogue), add = TRUE)
  live <- chain_web(c("Phyto", "Zoo", "Fish"))
  app <- make_app(reactiveVal(live$net), reactiveVal(live$info))
  testServer(app, {
    session$setInputs(cmp_example_b = rogue, cmp_load_example_b = 1)
    expect_null(mod$slot_b())
    session$setInputs(cmp_example_b = "../examples/Simple_3Species", cmp_load_example_b = 2)
    expect_null(mod$slot_b())
  })
})

test_that("pressing 'use current' with no live network leaves the slot empty", {
  skip_if_not_installed("shiny")
  app <- make_app(reactiveVal(NULL), reactiveVal(NULL))
  testServer(app, {
    session$setInputs(cmp_use_current_a = 1)
    expect_null(mod$slot_a())
    expect_match(output$cmp_slot_summary_a, "Empty")
  })
})

test_that("every cmp_* element in the UI has a server consumer", {
  skip_if_not_installed("shiny")
  html <- as.character(comparison_ui())
  hits <- regmatches(html, gregexpr('\\sid="cmp_[A-Za-z0-9_]+"', html))[[1]]
  ids <- unique(sub('^\\sid="(.*)"$', "\\1", hits))
  ids <- ids[!grepl("_progress$", ids)]
  src <- paste(readLines(file.path(get_app_root(), "R/modules/comparison_server.R"), warn = FALSE),
               collapse = "\n")
  literal <- regmatches(src, gregexpr("(input|output)\\$cmp_[A-Za-z0-9_]+", src))[[1]]
  literal <- unique(sub("^(input|output)\\$", "", literal))
  generated <- as.vector(outer(c("cmp_use_current_", "cmp_load_example_", "cmp_example_",
                                 "cmp_slot_summary_"), c("a", "b"), paste0))
  expect_gt(length(ids), 10L)
  expect_identical(setdiff(ids, c(literal, generated)), character(0))
})
