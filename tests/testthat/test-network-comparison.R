# =============================================================================
# compare_networks(): two-web comparison used by the Network Comparison tab
# =============================================================================
# Species match by trimmed, case-folded vertex name. Edges are prey -> predator
# (app-wide contract, R/functions/network_finalize.R).

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/functional_group_utils.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/trophic_levels.R"), local = FALSE)
  source(file.path(root, "R/functions/topological_metrics.R"), local = FALSE)
  source(file.path(root, "R/functions/network_comparison.R"), local = FALSE)
})
suppressPackageStartupMessages(library(igraph))

# Chain web: Phyto -> Zoo -> Fish, plus Phyto -> Fish (omnivory).
web_a <- function() {
  net <- make_graph(c("Phyto", "Zoo", "Zoo", "Fish", "Phyto", "Fish"), directed = TRUE)
  info <- data.frame(species = c("Phyto", "Zoo", "Fish"), meanB = c(10, 2, 0.5),
                     stringsAsFactors = FALSE)
  list(net = net, info = info)
}

# Shares Phyto + Fish, drops Zoo, adds Seal; drops Phyto -> Fish, adds Fish -> Seal.
web_b <- function() {
  net <- make_graph(c("Phyto", "Fish", "Fish", "Seal"), directed = TRUE)
  net <- delete_edges(net, "Phyto|Fish")
  net <- add_edges(net, c("Phyto", "Seal"))
  info <- data.frame(species = c("Phyto", "Fish", "Seal"), meanB = c(8, 1, 0.1),
                     stringsAsFactors = FALSE)
  list(net = net, info = info)
}

test_that("identical webs give zero deltas and Jaccard 1 everywhere", {
  a <- web_a()
  cmp <- compare_networks(a$net, a$info, a$net, a$info, "A", "B")

  expect_s3_class(cmp$metrics, "data.frame")
  expect_true(all(cmp$metrics$delta == 0, na.rm = TRUE))
  expect_setequal(cmp$metrics$metric[1:7], c("S", "C", "G", "V", "ShortPath", "TL", "Omni"))
  expect_true(all(c("nwC", "nwG", "nwV", "nwTL") %in% cmp$metrics$metric))

  expect_setequal(cmp$species$shared, c("Phyto", "Zoo", "Fish"))
  expect_length(cmp$species$only_a, 0)
  expect_length(cmp$species$only_b, 0)
  expect_equal(cmp$species$jaccard, 1)

  expect_equal(nrow(cmp$links$shared), 3)
  expect_equal(nrow(cmp$links$only_a), 0)
  expect_equal(nrow(cmp$links$only_b), 0)
  expect_equal(cmp$links$jaccard, 1)

  expect_equal(nrow(cmp$per_species), 3)
  expect_true(all(cmp$per_species$delta_tl == 0))
  expect_true(all(cmp$per_species$delta_prey == 0))
  expect_identical(cmp$labels, c(a = "A", b = "B"))
})

test_that("partial overlap reports exact species and link sets", {
  a <- web_a()
  b <- web_b()
  cmp <- compare_networks(a$net, a$info, b$net, b$info)

  expect_setequal(cmp$species$shared, c("Phyto", "Fish"))
  expect_identical(cmp$species$only_a, "Zoo")
  expect_identical(cmp$species$only_b, "Seal")
  expect_equal(cmp$species$jaccard, 2 / 4)

  # Among shared species {Phyto, Fish}: A has Phyto->Fish, B has none.
  expect_equal(nrow(cmp$links$shared), 0)
  expect_equal(cmp$links$only_a, data.frame(prey = "Phyto", predator = "Fish",
                                            stringsAsFactors = FALSE))
  expect_equal(nrow(cmp$links$only_b), 0)
  expect_equal(cmp$links$jaccard, 0)
  expect_equal(cmp$links$n_unshared_species_a, 2)  # Phyto->Zoo, Zoo->Fish
  expect_equal(cmp$links$n_unshared_species_b, 2)  # Fish->Seal, Phyto->Seal

  ps <- cmp$per_species[order(cmp$per_species$species), ]
  expect_identical(ps$species, c("Fish", "Phyto"))
  fish <- ps[ps$species == "Fish", ]
  expect_equal(fish$prey_a, 2)
  expect_equal(fish$prey_b, 0)
  expect_equal(fish$predators_a, 0)
  expect_equal(fish$predators_b, 1)
  expect_equal(fish$delta_prey, -2)
  expect_equal(fish$tl_a, 2.5)
  expect_equal(fish$tl_b, 1)
  expect_equal(fish$delta_tl, -1.5)
})

test_that("disjoint webs give Jaccard 0 and an empty per-species table", {
  a <- web_a()
  net_b <- make_graph(c("X", "Y"), directed = TRUE)
  info_b <- data.frame(species = c("X", "Y"), stringsAsFactors = FALSE)
  cmp <- compare_networks(a$net, a$info, net_b, info_b)

  expect_length(cmp$species$shared, 0)
  expect_equal(cmp$species$jaccard, 0)
  expect_equal(cmp$links$jaccard, 0)
  expect_equal(nrow(cmp$links$shared), 0)
  expect_equal(nrow(cmp$per_species), 0)
  expect_true(all(c("species", "prey_a", "prey_b", "tl_a", "tl_b", "delta_tl") %in%
                    names(cmp$per_species)))
})

test_that("species names match after trimming and case folding", {
  a <- web_a()
  net_b <- make_graph(c("phyto ", " ZOO", " ZOO", "fish"), directed = TRUE)
  info_b <- data.frame(species = c("phyto ", " ZOO", "fish"), stringsAsFactors = FALSE)
  cmp <- compare_networks(a$net, a$info, net_b, info_b)

  expect_length(cmp$species$shared, 3)
  expect_equal(cmp$species$jaccard, 1)
  expect_equal(nrow(cmp$links$shared), 2)
  expect_equal(nrow(cmp$links$only_a), 1)   # Phyto -> Fish missing in B
  # Display names come from web A.
  expect_setequal(cmp$per_species$species, c("Phyto", "Zoo", "Fish"))
})

test_that("node-weighted metrics are skipped when either web lacks biomass", {
  a <- web_a()
  info_no_b <- a$info[, "species", drop = FALSE]
  cmp <- compare_networks(a$net, a$info, a$net, info_no_b)
  expect_false(any(c("nwC", "nwG", "nwV", "nwTL") %in% cmp$metrics$metric))
  expect_equal(nrow(cmp$metrics), 7)
})

test_that("compare_networks rejects non-igraph input with a clear error", {
  a <- web_a()
  expect_error(compare_networks("x", a$info, a$net, a$info), "igraph")
})

test_that("list_example_networks finds bundled RData webs and load_example_network reads one", {
  root <- get_app_root()
  files <- list_example_networks(file.path(root, "examples"))
  expect_true("Simple_3Species.Rdata" %in% basename(files))
  expect_false(any(grepl("Template_Empty", files)))

  web <- load_example_network(file.path(root, "examples", "Simple_3Species.Rdata"))
  expect_true(igraph::is_igraph(web$net))
  expect_s3_class(web$info, "data.frame")
  expect_equal(nrow(web$info), igraph::vcount(web$net))
  expect_true(all(c("species", "fg", "colfg") %in% names(web$info)))
  expect_identical(web$label, "Simple_3Species")
})

test_that("load_example_network errors on a file without net/info", {
  tmp <- tempfile(fileext = ".Rdata")
  x <- 1
  save(x, file = tmp)
  expect_error(load_example_network(tmp), "net")
})
