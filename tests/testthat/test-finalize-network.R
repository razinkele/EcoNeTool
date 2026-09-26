# =============================================================================
# finalize_network(): align info to the graph and derive colfg (#18 / #27)
# =============================================================================
# plotfw()/create_foodweb_visnetwork() index info$colfg[i] by VERTEX index, so
# info's row order must match V(net)$name exactly. Two paths broke that:
#
#   #18 data_import_server.R - merge(info, extra, by = "species") re-sorts
#       alphabetically and info was never reordered to V(net)$name, so node
#       traits and colours attached to the wrong species.
#   #27 metaweb_manager_server.R - the export wrote an info frame with no
#       colfg at all, no `species` column (the metaweb calls it species_name)
#       and fg = "Other", which is not one of get_functional_group_levels().
#       The Food Web tab then errored on its required-columns check.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/functional_group_utils.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
})

make_net <- function(names_in_order) {
  net <- igraph::make_empty_graph(n = length(names_in_order), directed = TRUE)
  igraph::V(net)$name <- names_in_order
  net
}

# Deliberately NOT alphabetical: this is what a matrix-ordered graph looks like.
GRAPH_ORDER <- c("Sprat", "Cod", "Algae", "Herring")

# ---------------------------------------------------------------------------
# #18 - row alignment
# ---------------------------------------------------------------------------

test_that("finalize_network reorders info to match vertex order", {
  net <- make_net(GRAPH_ORDER)
  # Alphabetical, as merge() leaves it.
  info <- data.frame(
    species = c("Algae", "Cod", "Herring", "Sprat"),
    meanB = c(10, 1, 5, 7),
    fg = c("Phytoplankton", "Fish", "Fish", "Fish"),
    stringsAsFactors = FALSE
  )

  out <- finalize_network(net, info)

  expect_equal(out$info$species, GRAPH_ORDER)
  # The biomass must travel with its species, not with its old row number.
  expect_equal(out$info$meanB, c(7, 1, 10, 5))
})

test_that("finalize_network keeps info aligned with V(net)$name one-to-one", {
  net <- make_net(GRAPH_ORDER)
  info <- data.frame(species = rev(GRAPH_ORDER), meanB = 1:4,
                     fg = "Fish", stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_equal(nrow(out$info), igraph::vcount(net))
  expect_equal(out$info$species, igraph::V(out$net)$name)
})

test_that("finalize_network adds rows for vertices missing from info", {
  net <- make_net(GRAPH_ORDER)
  info <- data.frame(species = c("Cod", "Algae"), meanB = c(1, 10),
                     fg = c("Fish", "Phytoplankton"), stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_equal(out$info$species, GRAPH_ORDER)
  expect_equal(out$info$meanB[out$info$species == "Cod"], 1)
  # A vertex with no info row still gets a usable default rather than NA.
  expect_false(is.na(out$info$meanB[out$info$species == "Sprat"]))
})

test_that("finalize_network drops info rows that are not vertices", {
  net <- make_net(c("Cod", "Algae"))
  info <- data.frame(species = c("Cod", "Algae", "Whale"), meanB = c(1, 10, 99),
                     fg = c("Fish", "Phytoplankton", "Mammals"),
                     stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_equal(nrow(out$info), 2L)
  expect_false("Whale" %in% out$info$species)
})

# ---------------------------------------------------------------------------
# #27 - colfg, fg canonicalisation, alternative key column
# ---------------------------------------------------------------------------

test_that("finalize_network derives colfg from fg via COLOR_SCHEME", {
  net <- make_net(c("Cod", "Algae"))
  info <- data.frame(species = c("Cod", "Algae"),
                     fg = c("Fish", "Phytoplankton"),
                     meanB = c(1, 10), stringsAsFactors = FALSE)

  out <- finalize_network(net, info)
  levels_ref <- get_functional_group_levels()

  expect_true("colfg" %in% names(out$info))
  expect_equal(out$info$colfg[1], unname(COLOR_SCHEME[which(levels_ref == "Fish")]))
  expect_equal(out$info$colfg[2],
               unname(COLOR_SCHEME[which(levels_ref == "Phytoplankton")]))
})

test_that("an fg outside the canonical levels gets a grey colour, not an error", {
  net <- make_net(c("Unknown sp"))
  info <- data.frame(species = "Unknown sp", fg = "Other", meanB = 1,
                     stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_equal(out$info$colfg[1], "gray")
})

test_that("finalize_network canonicalises the fg factor levels", {
  net <- make_net(c("Cod"))
  info <- data.frame(species = "Cod", fg = "Fish", meanB = 1,
                     stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_true(is.factor(out$info$fg))
  expect_equal(levels(out$info$fg), get_functional_group_levels())
})

test_that("finalize_network accepts a frame keyed by species_name", {
  # The metaweb export shape: no `species` column at all.
  net <- make_net(c("Cod", "Algae"))
  info <- data.frame(species_name = c("Algae", "Cod"), meanB = c(10, 1),
                     stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_equal(out$info$species, c("Cod", "Algae"))
  expect_true("colfg" %in% names(out$info))
})

test_that("finalize_network fills the columns the Food Web tab requires", {
  net <- make_net(c("Cod", "Algae"))
  info <- data.frame(species_name = c("Cod", "Algae"), stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  # plotfw()/create_foodweb_visnetwork() hard-require these three.
  for (col in c("fg", "colfg", "meanB")) {
    expect_true(col %in% names(out$info), info = paste("missing", col))
    expect_false(any(is.na(out$info[[col]])), info = paste("NA in", col))
  }
  for (col in c("bodymasses", "met.types", "efficiencies", "losses")) {
    expect_true(col %in% names(out$info), info = paste("missing", col))
  }
})

test_that("finalize_network is idempotent", {
  net <- make_net(GRAPH_ORDER)
  info <- data.frame(species = rev(GRAPH_ORDER), meanB = 1:4,
                     fg = "Fish", stringsAsFactors = FALSE)

  once <- finalize_network(net, info)
  twice <- finalize_network(once$net, once$info)

  expect_equal(twice$info$species, once$info$species)
  expect_equal(twice$info$colfg, once$info$colfg)
  expect_equal(twice$info$meanB, once$info$meanB)
})

test_that("finalize_network rejects a network with no vertex names", {
  net <- igraph::make_empty_graph(n = 2, directed = TRUE)
  info <- data.frame(species = c("a", "b"), stringsAsFactors = FALSE)

  expect_error(finalize_network(net, info), "name", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# The two broken call sites must actually use the helper
# ---------------------------------------------------------------------------

test_that("data_import_server.R finalises through the helper (#18)", {
  code <- readLines(app_path("R/modules/data_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]

  expect_true(any(grepl("finalize_network(", code, fixed = TRUE)),
              info = "finalize_import must go through finalize_network()")
  # The hand-rolled colour loop it replaced must not come back.
  expect_false(any(grepl("colfg <- sapply", code, fixed = TRUE)),
               info = "hand-rolled colfg derivation reintroduced")
})

test_that("metaweb_manager_server.R finalises and refreshes on export (#27)", {
  code <- readLines(app_path("R/modules/metaweb_manager_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]

  expect_true(any(grepl("finalize_network(", code, fixed = TRUE)),
              info = "the export must go through finalize_network()")
  expect_true(any(grepl("refresh_data_editor", code, fixed = TRUE)),
              info = "the export must refresh the data editor")
  # A non-canonical fg default must not return. Scoped to fg assignment: the
  # metaweb preview plot legitimately uses "Other" as a visNetwork display
  # group, which has nothing to do with get_functional_group_levels().
  expect_false(any(grepl('fg <- factor(rep("Other"', code, fixed = TRUE)),
               info = "non-canonical fg default reintroduced")
})

# ---------------------------------------------------------------------------
# fg_to_color(): the one derivation, replacing four copy-pasted blocks
# ---------------------------------------------------------------------------
# The same sapply-over-COLOR_SCHEME loop was written out in five modules. Two
# of those copies drifted into #18 and #27. These pin fg_to_color() against
# the exact legacy formula so the consolidation is provably behaviour-neutral.

legacy_colfg <- function(fg_vec) {
  fg_levels <- get_functional_group_levels()
  unname(sapply(as.character(fg_vec), function(fg) {
    idx <- which(fg_levels == fg)
    if (length(idx) == 0) return("gray")
    COLOR_SCHEME[idx]
  }))
}

test_that("fg_to_color reproduces the legacy per-module derivation exactly", {
  cases <- list(
    c("Fish", "Phytoplankton", "Benthos"),
    c("Birds", "Mammals", "Detritus", "Zooplankton"),
    c("Other", "Unknown", "Fish"),
    factor(c("Fish", "Benthos"), levels = get_functional_group_levels()),
    c(NA, "Fish")
  )
  for (case in cases) {
    expect_identical(unname(fg_to_color(case)), legacy_colfg(case),
                     info = paste("differs for:", paste(case, collapse = "/")))
  }
})

test_that("fg_to_color returns one colour per input, unnamed", {
  out <- fg_to_color(c("Fish", "Benthos", "Other"))
  expect_length(out, 3L)
  expect_null(names(out))
})

test_that("no module still hand-rolls the colfg derivation", {
  modules <- list.files(app_path("R/modules"), pattern = "[.]R$",
                        full.names = TRUE)
  offenders <- character(0)
  for (f in modules) {
    code <- readLines(f, warn = FALSE)
    code <- code[!startsWith(trimws(code), "#")]
    if (any(grepl("colfg <- sapply", code, fixed = TRUE))) {
      offenders <- c(offenders, basename(f))
    }
  }

  expect_equal(offenders, character(0),
               label = "modules still deriving colfg by hand")
})

# ---------------------------------------------------------------------------
# F66 - fg inferred per vertex, not recycled from the first one
# ---------------------------------------------------------------------------

test_that("finalize_network infers fg per vertex when info has no fg column (F66)", {
  net <- make_net(c("Gadus morhua", "Calanus finmarchicus", "Diatoma"))
  info <- data.frame(species = c("Gadus morhua", "Calanus finmarchicus", "Diatoma"),
                     meanB = c(1, 2, 3), stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_equal(as.character(out$info$fg), c("Fish", "Zooplankton", "Phytoplankton"))
  expect_equal(length(unique(out$info$colfg)), 3L)
})

# ---------------------------------------------------------------------------
# F67 - metaweb export keeps its real attributes
# ---------------------------------------------------------------------------

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/metaweb_core.R"), local = FALSE)
})

template_metaweb <- function() {
  create_metaweb(
    species = data.frame(
      species_id = c("SP001", "SP002", "SP003"),
      species_name = c("Gadus morhua", "Clupea harengus", "Calanus finmarchicus"),
      functional_group = c("Fish", "Fish", "Zooplankton"),
      biomass = c(1, 5, 20),
      stringsAsFactors = FALSE
    ),
    interactions = data.frame(
      predator_id = c("SP001", "SP002"),
      prey_id = c("SP002", "SP003"),
      stringsAsFactors = FALSE
    )
  )
}

test_that("metaweb_to_igraph names vertices by species name, prey -> predator (F67)", {
  g <- metaweb_to_igraph(template_metaweb())

  expect_equal(igraph::V(g)$name, c("Gadus morhua", "Clupea harengus", "Calanus finmarchicus"))
  expect_equal(igraph::V(g)$species_id, c("SP001", "SP002", "SP003"))
  assert_prey_to_predator(g, "Clupea harengus", "Gadus morhua")
  assert_prey_to_predator(g, "Calanus finmarchicus", "Clupea harengus")
})

test_that("metaweb_to_igraph keeps duplicate species names distinct", {
  mw <- template_metaweb()
  mw$species$species_name[3] <- "Clupea harengus"
  g <- metaweb_to_igraph(mw)

  expect_equal(igraph::V(g)$name, c("Gadus morhua", "Clupea harengus", "Clupea harengus.1"))
  expect_equal(igraph::ecount(g), 2L)
  info <- metaweb_species_to_info(mw$species)
  expect_equal(info$species, igraph::V(g)$name)
})

test_that("metaweb_to_igraph drops links to unknown ids with a warning", {
  mw <- template_metaweb()
  mw$interactions <- rbind(mw$interactions,
                           data.frame(predator_id = "SP001", prey_id = "SP999",
                                      quality_code = 1, source = "x"))
  expect_warning(g <- metaweb_to_igraph(mw), "SP999")
  expect_equal(igraph::ecount(g), 2L)
})

test_that("metaweb_species_to_info maps the metaweb columns onto info columns", {
  species <- data.frame(
    species_id = c("SP001", "SP002"), species_name = c("Cod", "Mixed plankton"),
    functional_group = c("Fish", "pelagic"), biomass = c(2.5, 7),
    body_mass = c(1000, 0.001), metabolic_type = c("ectotherm vertebrates", "invertebrates"),
    efficiency = c(0.85, 0.75), stringsAsFactors = FALSE
  )

  info <- metaweb_species_to_info(species)

  expect_equal(info$species, c("Cod", "Mixed plankton"))
  expect_equal(info$fg, c("Fish", NA))  # "pelagic" is not canonical -> inferred later
  expect_equal(info$meanB, c(2.5, 7))
  expect_equal(info$bodymasses, c(1000, 0.001))
  expect_equal(info$met.types, c("ectotherm vertebrates", "invertebrates"))
  expect_equal(info$efficiencies, c(0.85, 0.75))
  expect_equal(info$species_id, c("SP001", "SP002"))
})

test_that("the bundled Baltic metaweb exports with real biomass and several fg (F67)", {
  rds <- app_path("metawebs/baltic/baltic_kortsch2021.rds")
  skip_if_not(file.exists(rds), "Baltic metaweb .rds not found")
  mw <- readRDS(rds)

  net <- metaweb_to_igraph(mw)
  out <- finalize_network(net, metaweb_species_to_info(mw$species))

  expect_equal(out$info$species, make.unique(mw$species$species_name))
  expect_equal(out$info$meanB, as.numeric(mw$species$biomass))
  expect_false(all(out$info$meanB == 1))
  expect_gt(length(unique(as.character(out$info$fg))), 1)
})

test_that("metaweb_manager_server.R exports through metaweb_species_to_info (F67)", {
  code <- readLines(app_path("R/modules/metaweb_manager_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("finalize_network(new_net, metaweb_species_to_info(", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# H-5 - duplicated names are warned about, not silently joined to one row
# ---------------------------------------------------------------------------

test_that("finalize_network warns naming duplicated vertex names (H-5)", {
  net <- make_net(c("Cod", "Herring", "Cod"))
  info <- data.frame(species = c("Cod", "Herring"), fg = c("Fish", "Fish"), stringsAsFactors = FALSE)
  expect_warning(finalize_network(net, info), "duplicated vertex name.*Cod")
})

test_that("finalize_network warns when info repeats a species (H-5)", {
  net <- make_net(c("Cod", "Herring"))
  info <- data.frame(species = c("Cod", "Herring", "Cod"), meanB = c(1, 2, 3), stringsAsFactors = FALSE)
  expect_warning(res <- finalize_network(net, info), "duplicated species.*Cod")
  expect_equal(res$info$meanB, c(1, 2))  # join unchanged: first row wins
})

test_that("finalize_network is silent on unique names (H-5)", {
  net <- make_net(c("Cod", "Herring"))
  info <- data.frame(species = c("Herring", "Cod"), fg = c("Fish", "Fish"), stringsAsFactors = FALSE)
  expect_no_warning(finalize_network(net, info))
})
