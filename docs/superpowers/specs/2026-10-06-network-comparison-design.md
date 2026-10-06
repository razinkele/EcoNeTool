# Network Comparison tab - design

Date: 2026-10-06. Branch: feature/network-comparison (off master, post 1.7.0 C-8 merge).

## Purpose

A sidebar entry "Network Comparison" that places two food webs side by side
(before/after, two regions, EwE model vs trait-built web) and reports what
differs in structure, species composition and trophic links.

## Sources of the two networks

The app holds one live `(net, info)` pair. The tab keeps two session-scoped
snapshot slots, A and B:

- "Use current network as A / B" copies the live pair into the slot.
- A dropdown lists `examples/*.Rdata` files that contain `net` + `info`; a
  "Load into A / B" button reads one straight into the slot through
  `finalize_network()`, bypassing the Data Import modals.
- Each slot shows a label, species count and link count.

Both slots are snapshots so loading B never clobbers A.

## Comparison semantics (pure functions, `R/functions/network_comparison.R`)

Species are matched by vertex name after `trimws()` and case folding.
Edges follow the app-wide contract (prey -> predator).

`compare_networks(net_a, info_a, net_b, info_b, label_a, label_b)` returns:

- `metrics`: data frame of S, C, G, V, ShortPath, TL, Omni for A and B with
  delta (B - A); node-weighted nwC/nwG/nwV/nwTL appended only when both
  infos carry a `meanB` column.
- `species`: list with `shared`, `only_a`, `only_b` character vectors and
  `jaccard`.
- `links`: list with `shared`, `only_a`, `only_b` data frames (prey,
  predator) restricted to shared species, plus `jaccard` and counts of links
  in A / B that involve a species absent from the other web.
- `per_species`: data frame for shared species: prey_a, prey_b, predators_a,
  predators_b, tl_a, tl_b and deltas.
- `labels`: the two labels.

## UI / server

- `R/ui/comparison_ui.R`: `comparison_ui()` -> `tabItem(tabName = "comparison")`.
  IDs prefixed `cmp_`.
- `R/modules/comparison_server.R`: `comparison_server(input, output, session,
  net_reactive, info_reactive)`, plain-function pattern, snapshot
  `reactiveVal`s, comparison computed in a `reactive()` guarded by `req()`.
- Outputs: slot summaries, metrics table (DT), species overlap value boxes +
  lists, link overlap counts + table, per-species DT, two CSV downloads
  (per-species table, link differences).
- `app.R`: source lines, `menuItem`, `comparison_ui()`, server call.
- `R/config/plugins.R`: `comparison` entry under `analysis`.

## Tests

`tests/testthat/test-network-comparison.R`: identical webs (zero deltas,
Jaccard 1), disjoint webs (Jaccard 0, empty per-species), partial overlap
(exact counts), case/whitespace name matching, missing `meanB` (no
node-weighted rows), example-file loader on `examples/Simple_3Species.Rdata`.

## Out of scope (v1)

Side-by-side network plots, more than two networks, persistence across
sessions, version bump.
