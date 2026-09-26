# Sub-project A: Network-Science Correctness (edge contract, MTI/KS, Rpath TL, finalize & import)

- **Status:** Approved 2026-09-26. No code changed yet.
- **Date:** 2026-09-26
- **Source:** `docs/econetool-deep-analysis-2026-09-26.md`. This spec covers remediation batches 1, 2 and 7.
- **Precondition:** The F1 hotfix (harmonization slider loop, spec B) ships **before** any A PR.
- **Target release:** 1.5.0 ("Results changed").

---

## 1. Goal & context

Several EcoNeTool outputs are scientifically wrong today: trophic level (TL), Mixed Trophic Impact (MTI), keystoneness, per-hexagon TL, Rpath TL and several import attributes. The app shows no error for any of them. There are two root causes:

1. **Edge orientation is inconsistent.** The app-wide contract is that an edge `A -> B` means "A is eaten by B" (prey -> predator), so `adj[prey, predator] = 1`. Diet matrices are laid out prey x predator, as in EwE and Rpath. Most of the app follows this contract: `trophic_levels.R:55-58`, fluxweb `ef.level = "prey"`, `network_visualization.R` edge colouring, and the native `.ewemdb` import (`ecopath_import_server.R:318-329`). Several ingest and metric boundaries break it. Two of those breaks cancel each other out: four bundled metaweb CSVs store their columns swapped, and `metaweb_to_igraph()` swaps them back. As a result, **every edge fix in A1 must land in one PR**.
2. **Estimator bugs.** MTI is normalised along the wrong axis and can never be positive. Rpath's correct TL is overwritten with a transposed recomputation. Non-converged TL values (about 101) are returned as if valid.

Batch 7 fixes the attribute layer that feeds these estimators: `finalize_network()`, the EcoBase and EwE imports, the data editor, and the trait food-web builder.

## 2. Scope

Line numbers were re-verified against `master` @ `7a96789` on 2026-09-26.

| ID | Sev | Current file:line | Status | Defect (one line) |
|----|-----|-------------------|--------|-------------------|
| F64 | crit | `R/functions/metaweb_core.R:138` | CONFIRMED | `edges <- interactions[, c("predator_id","prey_id")]` builds predator->prey edges |
| F56 | crit | `R/functions/spatial_analysis.R:259` (TL loop 409-419) | CONFIRMED, **fix differs from report** | `local_net` is built predator->prey, so `neighbors(mode="in")` at :414 returns predators. Fix the edge at :259 and keep `mode="in"` |
| F48 | crit | `R/functions/ecopath/ecopath_csv.R:110-114` | CONFIRMED | `t(diet_matrix > 0)` reverses every CSV/Excel EwE link. The FG topology heuristic is inverted as a result |
| F55 | med | `R/functions/ecopath/ecopath_csv.R:71-72` | CONFIRMED | `^detritus$` in the summary-row filter drops the Detritus group |
| F65 | crit | `R/functions/keystoneness.R:1-59` (norm 18-25, MTI 47), KS 97-159 (formula 127, class 134-139) | CONFIRMED | Row-normalises over prey rows. `MTI = -(I-DC)^-1 DC` is always <= 0. KS is not Libralato's |
| F69 | low | `R/functions/trophic_levels.R:77-80` | CONFIRMED | Non-converged TL (self-loop C gives 101) is returned with only a warning |
| F85 | low | `tests/testthat/` (none) | CONFIRMED | No testthat file references `calculate_mti`, `calculate_keystoneness`, `calculate_trophic_levels` or `metaweb_to_igraph` |
| N1 | crit | `R/modules/ecopath_import_server.R:1806-1808` | NEW (found in verification) | EwE->metaweb writer sets `predator_id = as_edgelist(native_net)[,1]`, which is the prey. This is correct today only because of F64 |
| N2 | low | `R/modules/metaweb_manager_server.R:163-165` | NEW | Metaweb visNetwork arrows run `from = predator_id`, the opposite of the Food Web tab |
| N3 | high | `R/functions/trait_foodweb.R:339-341` | NEW | `trait_foodweb_to_igraph()` builds `from = Consumer, to = Resource` (predator->prey) for the RDS/GraphML exports |
| F40 | crit | `R/functions/rpath/rpath_balancing.R:81` (override 266-284) | CONFIRMED | Reads `DC[i, j]` as "prey j of predator i" although Rpath's DC is prey x predator, then overwrites Rpath's TL |
| F44 | high | `R/functions/rpath/rpath_workflows.R:319`, `:350`, `:374` | CONFIRMED | `!is.data.frame(model)` rejects the class-`Rpath` list, and `type < 3` counts detritus as living |
| F41 | high | `R/functions/rpath/rpath_conversion.R:343-368` | CONFIRMED | Self-diet is set to 0 without renormalising, and the user is told only via `message()` |
| F67 | high | `R/functions/network_finalize.R:21-22`; `metaweb_core.R:141-145` | CONFIRMED | Vertices are named `SP001`, so key `species_name` matches nothing. Every attribute falls back to its default, and metaweb columns are never mapped |
| F66 | high | `R/functions/network_finalize.R:102` | CONFIRMED | `fg_char` is a length-1 NA, so the first inferred fg is recycled to every vertex |
| F50 | high | `R/functions/ecobase_connection.R:456-467`, `:642-653`; `R/modules/ecobase_server.R:246-252` | CONFIRMED | No `met.types`; `bodymasses = biomass * 100`; `efficiencies = 0.8`; `finalize_network()` is never called |
| F49 | high | `R/modules/ecopath_import_server.R:661-667` | CONFIRMED | EwE `Type` is never read. The regex `detritus\|det\\.` misses `Detritu` and `Rdetrit` |
| F52 | med | `R/modules/ecopath_import_server.R:132-140` vs `rpath_conversion.R:203` | PARTIAL | The divergence is real: the network path multiplies by `Area` and the Rpath path does not. Which one is correct depends on EwE field semantics, to be verified in A3 step 1 |
| F68 | med | `R/modules/dataeditor_inline_server.R:105`, save 110-120 | CONFIRMED | The DT string is assigned uncoerced, so the whole numeric column becomes character |
| F51 | med | `R/modules/ecopath_import_server.R:1598-1601`, path :1623 | CONFIRMED | `current_network()` is undefined anywhere. The fallback bbox is hard-coded and the GDB path is wd-relative |
| F74 | med | `R/functions/trait_foodweb.R:347-351`; observer `foodweb_construction_server.R:354-400` | CONFIRMED | `graph_from_data_frame(vertices = species_data)` keys on the first column, and the observer has no `tryCatch` |

Bundled metaweb orientation was classified by in-/out-degree of detritus and producers. On a correct file these must have no prey.

| Metaweb | Detritus as `predator_id` / `prey_id` | Verdict |
|---|---|---|
| baltic/baltic_kortsch2021 | 12 / 0 (basal as labelled: *Gadus morhua*) | **SWAPPED** |
| arctic/barents_boreal_kortsch2015 | 52 / 0 | **SWAPPED** |
| arctic/barents_arctic_kortsch2015 | 55 / 0 | **SWAPPED** |
| atlantic/north_sea_frelat2022 | 62 / 0 | **SWAPPED** |
| arctic/kongsfjorden_farage2021 | 0 / 170 | correct |

For each of the five, the `.rds` interactions are identical to the CSV.

## 3. Non-goals

- F1 and the rest of spec B (harmonization loop, deploy/CI, admin gating) and spec C (trait vocabulary, including F71).
- Rpath schema work (F42 fleets/detritus fate, F43 Ecosim, F45 sensitivity, F46 fishing, F47 reset).
- Replacing the binary-adjacency TL with a flow-weighted TL.
- A full detritus treatment in MTI. EwE handles detritus separately; see Risks.
- Rpath dynamics (spec E) and spatial session state (spec D).

## 4. Design

### A1 Edge contract & MTI (batch 1 + N1-N3). One atomic PR.

**Contract (documented once).** The header of `R/functions/network_finalize.R` gets an "Edge contract" block. It states the following:

- `A -> B` means that B eats A.
- `adj[prey, predator]`.
- Diet matrices are prey x predator, and each predator column sums to 1.
- `metaweb$interactions` columns mean exactly what their names say.

`tests/testthat/helper-fixtures.R` gets `assert_prey_to_predator(net, prey, predator)`. It calls `expect_true(igraph::are_adjacent(net, prey, predator))` and `expect_false(igraph::are_adjacent(net, predator, prey))`. It is used by every test below.

**Per-file changes**

| File | Change |
|---|---|
| `metaweb_core.R:138` | Use `interactions[, c("prey_id","predator_id")]` |
| `spatial_analysis.R:259` | Use `local_interactions[, c("prey_id","predator_id")]`. **Keep `mode = "in"` at :414**; under the contract it returns prey. Replace the fixed 10-pass loop (409-419) with `calculate_trophic_levels(net)`, using `mean(tl, na.rm = TRUE)` / `max(tl, na.rm = TRUE)` for meanTL/maxTL |
| `ecopath_csv.R:107-114` | `adjacency_matrix <- (diet_matrix > 0) * 1`, with row and column names kept as prey and predator. Set `E(net)$diet_prop` from `diet_matrix`. Remove the wrong comment |
| `ecopath_csv.R:71` | Remove `^detritus$` from the filter (F55) |
| `ecopath_import_server.R:1806-1808` (N1) | Use `prey_id = edges[,1], predator_id = edges[,2]` |
| `metaweb_manager_server.R:163-165` (N2) | Use `from = prey_id, to = predator_id`, so arrows show energy flow as in the Food Web tab |
| `trait_foodweb.R:339-341` (N3) | Use `from = colnames(prob)[edges[,2]]` (resource) and `to = rownames(prob)[edges[,1]]` (consumer). The `construct_trait_foodweb()` matrix (rows = consumers) and its heatmap are unchanged |
| `functional_group_utils.R:127-137` | Comments only. The logic is already correct for prey->predator; the comments describe the reverse |
| EwE importers | Native (`ecopath_import_server.R` ~L318-329) and EcoBase (`ecobase_connection.R:437-441`) also set `E(net)$diet_prop` from their diet matrix |
| `R/ui/import_ui.R` / metaweb help, `metawebs/README.md` | One sentence stating the column semantics plus the prey->predator graph direction |

**MTI / keystoneness rewrite (`keystoneness.R`)**

This implements Ulanowicz & Puccia (1990) and Libralato et al. (2006). All matrices are indexed prey x predator, matching `adj`.

1. `D` (DC): take `as_adjacency_matrix(net, attr = "diet_prop")` when the edge attribute exists, otherwise the binary adjacency. Normalise **columns** (predators) to sum to 1. Predators with no prey get a zero column.
2. Consumption per predator: `cons_k = B_k * QB_k` when `info$QB` exists and is finite and positive for k. Otherwise use `cons_k = B_k`, the biomass proxy. `B = info$meanB`, aligned by vertex order through `finalize_network()`.
3. Flows `T[p,k] = D[p,k] * cons_k`. `FC = T / rowSums(T)` (zero rows stay 0). `FC[j,i]` is the fraction of prey j's total predation taken by predator i.
4. `Q = D - t(FC)`. `MTI = solve(I - Q) - I`. If `rcond(I - Q) < 1e-10`, use `MASS::ginv` and call `warning()`. `MTI[i, j]` is the impact of i on j. The diagonal is **kept** in the returned matrix; the heatmap (`analysis_server.R` ~L240) sets it to `NA` for display.
5. `epsilon_i = sqrt(sum_{j != i} MTI_ij^2)` (row-wise, impactor = row). `p_i = B_i / sum(B)`. `KS_i = log(epsilon_i * (1 - p_i))`. Non-finite values become `NA` with status "Undefined".
6. Classification. The old `KS > 1` thresholds are meaningless on a log scale. Species are ranked by KS in descending order, then:
   - **Keystone:** `KS >= quantile(KS, 0.75, na.rm = TRUE)` and `p < 0.05`.
   - **Dominant:** the same KS condition and `p >= 0.05`.
   - **Other:** everything else.
   The returned columns keep their names for UI compatibility: `overall_effect` now holds epsilon, plus `relative_biomass`, `keystoneness`, `keystone_status` and a new `ks_rank`.
7. `validate_network(..., require_directed = TRUE)`. The roxygen blocks (L1, L61-96) are rewritten with the formulas above.
8. UI help (`R/ui/keystoneness_ui.R:18-38`) documents two approximations:
   - Without EwE Q/B, predation pressure is apportioned by predator biomass.
   - Without diet proportions, each predator's diet is split equally among its prey.
   It also explains that producers now show positive impacts on their consumers.

**Trophic levels (F69) - `trophic_levels.R`**

- Basal nodes are those with in-degree 0. A self-loop counts as an incoming edge, so a node whose only prey is itself is not basal. Compute `reach <- nodes reachable from any basal node` using `igraph::subcomponent(mode = "out")`. Every node outside `reach` gets `NA`, and `warning()` names the nodes (for example "3 node(s) have no path from a basal species: C, ...").
- Iterate only over `reach`. Any node still changing by more than `convergence` at `max_iter` also gets `NA`, with a `warning()`. If no basal node exists, every node is `NA` and there is one warning.
- Callers must tolerate `NA`:

| Caller | Required handling |
|---|---|
| `network_visualization.R:61-63` | `max(tl, na.rm = TRUE)`. `NA` nodes go to their own bottom "unplaced" row: bin `nylevel + 1`, drawn at y = 0.5 |
| `network_visualization.R:147` | Compute `y_pos` with `na.rm`. `NA` nodes are placed at `max + spacing` |
| `topological_metrics.R:19-20` | `TL <- mean(tlnodes, na.rm = TRUE)`. The omnivory `sd` already uses `na.rm` |
| `topological_metrics.R:87-91` | `nwTL` over non-NA nodes only, with biomass re-normalised over those nodes |
| `spatial_analysis.R` meanTL/maxTL | `na.rm = TRUE`, as specified above |

  `app.R:634-636`, `analysis_server.R:44` and `visualization_server.R:22,120` pass the vector through and need no change beyond the helpers above.

**Error handling.** All new `tryCatch` handlers call `warning()`, never `message()`, and use `<<-` for any outer mutation. Estimators keep their current `stop(sprintf("Failed to ...: %s"))` wrapping so the UI error paths are unchanged.

**Data migration (bundled metawebs).**

- New script `scripts/initialization/fix_metaweb_orientation.R`. No generator for these files exists in the repo; they were produced out of tree.
- The script keeps a fixed list of the four SWAPPED metawebs. For each one it:
  1. reads the CSV and swaps the `predator_id` and `prey_id` values;
  2. writes the CSV back;
  3. rebuilds the `.rds` with `import_metaweb_csv(species_file, interactions_file, metadata = readRDS(old_rds)$metadata)`, which preserves the metadata;
  4. appends `metadata$orientation_fixed <- "2026-09 (A1)"`.
- The script refuses to run if the orientation check already passes, so a second run cannot re-swap a file. Kongsfjorden is untouched.
- The script uses `app_path()` for every path.
- Net effect for users: the four swapped metawebs gave correct Food Web and spatial TL before A1 (the two bugs cancelled) and still do after A1. Their MTI/KS output changes because of the rewrite.

### A2 Rpath TL & diagnostics (batch 2). Separate PR.

- **F40:** delete the override block `rpath_balancing.R:266-284` and `calculate_rpath_trophic_levels()` (L22-113). Rpath solves TL linearly and correctly, including loops, so `model$TL` stays as Rpath returns it. Also fix `rpath_balancing.R:289-290`, which reads `model$Type`; the balanced object uses lowercase `type`, so the summary line always printed 0.
- **F44:** add `.as_balanced_frame(model)`.
  - `inherits(model, "Rpath")`: build `data.frame(Group, type, TL, Biomass, PB, QB, EE)` from the list vectors. Each vector has length `NUM_GROUPS` and includes fleets as type 3.
  - `is.data.frame(model)`: pass it through.
  - Anything else: `warning()` and return `NULL`.
  `.require_balanced_model()` then runs on that frame. Living groups become `type < 2` at `rpath_workflows.R:350` and `:374`. Diagnostics report detritus separately (`n_detritus = sum(type == 2)`).
- **F41:** delete `rpath_conversion.R:343-368`. Cannibalism stays as entered, so diet columns keep summing to 1. If a column sums to more than 1 + 1e-6, call `warning()` with the group name. The data is not changed.
- **Error handling:** Rpath-absent paths stay behind `requireNamespace("Rpath")`. Diagnostics failures return `NULL` plus `warning()`, the pattern already used.

### A3 Finalize & import (batch 7). Separate PR, after A1.

- **F66:** `network_finalize.R:102` becomes `fg_char <- if ("fg" %in% names(aligned)) as.character(aligned$fg) else rep(NA_character_, length(vertex_names))`.
- **F67:**
  - `metaweb_to_igraph()` names vertices `make.unique(species$species_name)`, keeps `V(g)$species_id`, and maps edge ids to names with `match()` on `species_id`, falling back to `species_name`.
  - New `metaweb_species_to_info(species)` in `metaweb_core.R` renames columns as follows:

    | Metaweb column | Info column | Notes |
    |---|---|---|
    | `species_name` | `species` | |
    | `functional_group` | `fg` | Only when the value is a canonical level, else `NA` so it is inferred |
    | `biomass` | `meanB` | |
    | `body_mass` | `bodymasses` | |
    | `metabolic_type` | `met.types` | |
    | `efficiency` | `efficiencies` | |

  - `metaweb_manager_server.R:448-456` calls `finalize_network(net, metaweb_species_to_info(species))`.
  - `spatial_analysis.R` keeps id-named vertices; it computes metrics only, and :250-253 already matches on id or name.
- **F50:** both EcoBase converters stop emitting `bodymasses`, `efficiencies` and `losses`. `finalize_network()` fills only `NA` or absent columns, so the proxy must be removed, not overwritten. `ecobase_server.R:246-252` then calls `finalize_network(net, info)`, which estimates `met.types`, `bodymasses` and `efficiencies` by fg.
- **F49:** native import reads EwE `Type` when the column exists, before `assign_functional_groups()` (L661):

  | `Type` | fg |
  |---|---|
  | `2` | `"Detritus"` |
  | `1` | `"Phytoplankton"`, unless the name classifier returns Benthos (macroalgae or seagrass) |
  | `0 < Type < 1` (mixotroph) | name classifier only |
  | `0` | classifier with topology, but never Detritus or Phytoplankton via topology |

  Also widen the detritus regex to `detrit|^det\\b|debris`.
- **F52:**
  1. Open `examples/Coastal model EE 1.ewemdb` and `LT2022_0.5ST_final7.eweaccdb` and record whether `EcopathGroup` holds `Biomass` as habitat-area or total-area biomass. Also check whether a `BiomassAreaInput`-style column exists.
  2. Write the finding into the helper's roxygen.
  3. Add `ewe_group_biomass(group_table)` to `R/functions/ecopath/`. Both `ecopath_import_server.R:132-140` and `rpath_conversion.R:203` call it, so the two paths agree by construction.
- **F68:**
  - `dataeditor_inline_server.R:105` becomes `species_data_df[r, c] <- DT::coerceValue(info_edit$value, species_data_df[[c]])`. `c` is the column index as sent with `rownames = TRUE`.
  - Save (L116-120) also checks `is.numeric()` for `meanB`, `bodymasses` and `efficiencies`, and stops with a named-column error.
- **F51:** backend exists (`load_regional_euseamap()`), so wire, don't remove.
  - Replace `current_network()` with `input$sampling_longitude` and `input$sampling_latitude`, which L1770 already uses. When those are absent, use the imported model metadata bbox (`meta$min_lon`..`max_lat`, L1020).
  - Remove the hard-coded Baltic fallback. With no location, show `showNotification(..., type = "warning")` explaining that a sampling location is needed, and untick the box.
  - The path becomes `app_path("data/EUSeaMap_2025/EUSeaMap_2025.gdb")`.
- **F74:**
  - `trait_foodweb_to_igraph()` builds `vertices <- species_data[, c("species", setdiff(names(species_data), "species"))]`.
  - The construct observer (`foodweb_construction_server.R:367-397`) is wrapped in `tryCatch`. The error handler calls `warning()` plus `showNotification(type = "error")`.

## 5. Testing strategy

All tests are testthat files under `tests/testthat/`. Tests source through `helper-fixtures.R` (`source_app_dependencies()`) and never `if`-gate an `expect_*`; they use `skip_if()` or `skip_if_not_installed()` with a reason instead.

**A1: `test-edge-contract.R`**

1. `metaweb_to_igraph` on a 3-species template metaweb (cod eats herring eats *Calanus*): `assert_prey_to_predator(g, "Clupea harengus", "Gadus morhua")`. TL is cod 3, herring 2, *Calanus* 1.
2. For each of the five `METAWEB_PATHS` in `R/config.R:143-149`, load the `.rds`, build the graph, and check two things:
   - every vertex whose name matches `detrit|phyto|diatom|autotroph|macroalgae|microalgae` has in-degree 0 and TL 1;
   - mean TL of those vertices is below mean TL of the rest.
   This test fails on today's four swapped files after the flip, and it guards the migration.
3. `extract_local_network` on the asymmetric web (Phyto eaten by Zoo1 and Zoo2; Fish eats Zoo1): TL is Phyto 1, Zoo1 2, Zoo2 2, Fish 3. Before the fix, meanTL came out as 1.333 for Phyto.
4. `parse_ecopath_data` on a 4-group CSV (Phyto, Detritus, Zoo, Cod; Zoo eats Phyto and Detritus; Cod eats Zoo):
   - `assert_prey_to_predator(net, "Phyto", "Zoo")`;
   - TL is Phyto 1, Detritus 1, Zoo 2, Cod 3;
   - `"Detritus"` is retained (F55);
   - the Cod fg is not Phytoplankton.
5. The N1 round trip: a native-import-style prey->predator graph goes through the metaweb writer code path and back via `metaweb_to_igraph` and is identical (`igraph::identical_graphs` after sorting edges).
6. N3: `trait_foodweb_to_igraph` on the `"simple"` example dataset. Every edge's `from` has FS/MS consistent with being the resource, and the FS0 producer has in-degree 0.
7. Guard: `metaweb_core.R` and `spatial_analysis.R` contain no `c("predator_id", "prey_id")` edge-list construct. This is the grep pattern already used in `test-finalize-network.R:186-198`.

**A1: `test-mti-keystoneness.R`** (tolerance 1e-8)

1. Chain P->Z->F, B = (10, 5, 1), no QB. Expected `3 * MTI` in order P, Z, F:

   | Impactor | on P | on Z | on F |
   |---|---:|---:|---:|
   | P | -1 | 1 | 1 |
   | Z | -1 | -2 | 1 |
   | F | 1 | -1 | -1 |

   epsilon is `sqrt(2/9) = 0.4714045` for all three. KS is P -1.7328680, Z -1.1267321, F -0.8165772.
2. Producer impact is positive: `MTI["P","Z"] > 0` (the F65 regression).
3. Fan P -> {Z1, Z2}, B = (10, 3, 1), no QB: `MTI["Z1","Z2"] = -0.375` and `MTI["Z2","Z1"] = -0.125`. With `info$QB = c(0, 1, 9)` the values swap to -0.125 and -0.375, proving that Q/B x B is used when present.
4. `calculate_keystoneness` returns the columns `species, overall_effect, relative_biomass, keystoneness, keystone_status, ks_rank`. `keystone_status` is in {Keystone, Dominant, Other, Undefined}.
5. The singular case (a 2-cycle with no basal node) gives a `warning()` and a finite matrix.

**A1: `test-trophic-levels.R`**

1. Chain A->B->C gives TL 1, 2, 3 with no warning.
2. The graph `make_graph(c("P","A","A","B","B","A","C","C"))` gives P 1, A 4, B 5, C `NA`. `expect_warning(..., "no path from a basal")`.
3. A web with no basal node gives all `NA` and one warning.
4. `plotfw()` and `get_topological_indicators()` + `get_node_weighted_indicators()` run without error on the graph from case 2 (`expect_no_error`).

**A2: `test-rpath-tl.R` and edits to `test-rpath-diagnostics.R`**

1. `structure(list(Group = c("Phyto","Det","Zoo","Cod","Fleet"), type = c(1,2,0,0,3), TL = c(1,1,2,3,NA), Biomass = c(20,50,5,1,NA), PB = c(100,0,30,0.5,NA), NUM_GROUPS = 5), class = "Rpath")`:
   - `calculate_ecopath_diagnostics` is non-NULL with `n_groups == 3`, mean TL `(1+2+3)/3 = 2`, and `n_detritus == 1`;
   - `trophic_pyramid_bins` is non-NULL and excludes Det.
2. Update the existing expectations at `test-rpath-diagnostics.R:48-58` from `type < 3` to `type < 2`: mean TL becomes `mean(c(1.0, 2.1, 3.6)) = 2.233333`, `n_groups` becomes 3, and `total_biomass` becomes 26.
3. Structural: `rpath_balancing.R` no longer defines or calls `calculate_rpath_trophic_levels`, and `rpath_conversion.R` contains no `Removing cannibalism`.
4. `convert_ecopath_to_rpath` on a fixture with cod-on-cod 0.1 keeps `diet[Cod, Cod] == 0.1`, and the column sums to 1. `skip_if_not_installed("Rpath", ...)` applies only to the create-params step.
5. Live, behind `RUN_LIVE_TESTS` with `skip_if_not_installed("Rpath")`: balance the Phyto->Zoo->Fish toy and assert TL is 1, 2, 3 after `run_ecopath_balance`.

**A3: edits to `test-finalize-network.R`, plus new `test-import-attributes.R`**

1. F66: info without `fg` for `c("Gadus morhua","Calanus finmarchicus","Diatoma")` gives three different inferred fg values, not all the same.
2. F67: exporting the Baltic `.rds` via `metaweb_to_igraph` + `finalize_network(metaweb_species_to_info(...))`:
   - `info$species` equals the species names;
   - `info$meanB` equals the CSV biomass (not all 1);
   - `length(unique(info$fg)) > 1`.
3. F50: a converter fixture built from a recorded EcoBase list (no network) has `met.types` with no NA after `finalize_network`, and `bodymasses` is not equal to `meanB * 100`.
4. F49: a group table with `Type = c(1, 2, 0)` and names `c("Phyto","Rdetrit","Cod")` gives fg Phytoplankton, Detritus, Fish.
5. F52: `ewe_group_biomass()` known-answer on the two example files, using the values recorded in A3 step 1.
6. F68: the edit handler logic, extracted to `apply_cell_edit(df, row, col, value)`. Editing a `meanB` cell with `"3.5"` keeps `is.numeric(df$meanB)`.
7. F74: `trait_foodweb_to_igraph` on data whose columns are `MS, species, FS, ...` does not error, and `V(g)$name == species`.
8. F51: structural. The file does not contain `current_network(`, and the GDB path is wrapped in `app_path(`.

Every PR runs `testthat::test_dir("tests/testthat")` (the legacy `tests/run_all_tests.R` cannot run locally because it needs `dggridR`). No existing test may regress except the listed expectation updates.

## 6. Rollout

| PR | Contents | Commit style |
|---|---|---|
| (pre) | F1 hotfix, spec B | - |
| A1 | Contract doc, `assert_prey_to_predator`, F64, F56, F48, F55, N1-N3, MTI/KS (F65), F69 + callers, F85 tests, metaweb migration script **and** regenerated CSV/.rds | `fix(network)!: unify prey->predator edge contract` with a `BREAKING CHANGE:` footer |
| A2 | F40, F44, F41 | `fix(rpath): keep Rpath TL, accept Rpath object, keep cannibalism` |
| A3 | F66, F67, F50, F49, F52, F68, F51, F74 | `fix(import): ...` (split commits per finding) |

- **A1 is atomic.** Any subset leaves either the bundled metawebs or EwE-derived metawebs inverted. A2 is independent. A3 must merge after A1, because both touch `metaweb_to_igraph()` and `trait_foodweb.R`.
- **Release 1.5.0** after A3.
  - Bump `VERSION` (currently 1.4.4), `R/config.R:294` (currently `"1.4.2"`, stale) and the `app.R:46` header together.
  - Run `scripts/generate_changelog.R --version 1.5.0`. The generator overwrites the whole file, so insert the hand-written section below **after** regeneration, inside the 1.5.0 block.
- **CHANGELOG "Results changed" section:**
  - **MTI / keystoneness: every source.** Formula replaced (Libralato 2006). Producers can now have positive impacts. Earlier KS values and statuses are not comparable.
  - **Trophic levels changed for:**
    - CSV/Excel EwE imports: previously inverted; FG heuristics also shift;
    - the Kongsfjorden metaweb;
    - user metawebs built from the template or uploaded CSVs: previously inverted;
    - per-hexagon meanTL/maxTL from those metawebs;
    - Rpath TL after balancing: previously transposed.
  - **Unchanged TL:** the Baltic, Barents (x2) and North Sea bundled metawebs, and metawebs derived from `.ewemdb`. Two errors cancelled for these.
  - Nodes with no path from a basal species now show TL `NA` instead of about 101.
  - Earlier exports (CSV, RDS, GraphML) from the "changed" paths above are inverted and should be regenerated.
  - Rpath diagnostics now exclude detritus from "living" totals.
- **Deploy** follows the memory workflow:
  1. `cd deployment && Rscript pre-deploy-check.R`.
  2. `powershell ./deploy-windows.ps1 -SkipData -NoSudo`.
  3. `ssh razinka@laguna.ku.lt "cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt"`.
  4. Verify by grepping the deployed `metaweb_core.R` for `"prey_id","predator_id"`, then `curl` for HTTP 200.

  `metawebs/` is code-side, not `data/`, so `-SkipData` still ships the regenerated files. Confirm this in the staging listing before copying.

## 7. Risks & open questions

- **Detritus in MTI.** EwE routes detritus through `DetFate`, not predation. The approximation treats detritus as an ordinary prey with biomass-weighted pressure, so its impacts are indicative only. The UI help says so.
- **KS classification thresholds** (top-quartile rule) are a design choice; Libralato ranks without fixed cut-offs. Revisit if users want Libralato's plain ranking only.
- **F52 direction is unknown until the example DBs are inspected.** If EwE's `Biomass` is already per total area, the `x Area` in the network path is the bug. That would change network-tab biomasses and fluxes for models with `Area < 1`. Add it to "Results changed" if so.
- **N3** changes the direction in trait food-web RDS/GraphML exports. External users of those files must re-export. It is listed under "changed" in the CHANGELOG.
- **Visual change.** Metaweb graph arrows (N2) flip to prey->predator.
- **Legacy tests.** `tests/test_phase1_spatial.R` and `tests/run_all_tests.R` spatial cases may assert old per-hex TL values. Update the expected numbers there in A1 and justify each change in the PR.
- **`bindCache`** on `keystoneness_results` (`analysis_server.R:139`) is keyed on `(net, info)`. Cached values from before the upgrade die with the process restart, so no action is needed.

## 8. Acceptance criteria

- [ ] `network_finalize.R` header documents the edge contract. `assert_prey_to_predator()` exists in `helper-fixtures.R`.
- [ ] No code path builds predator->prey edges: `metaweb_to_igraph`, `extract_local_network`, `parse_ecopath_data`, the EwE->metaweb writer and `trait_foodweb_to_igraph` are all covered by tests.
- [ ] Four bundled metawebs are swapped in both CSV and `.rds`. Kongsfjorden is untouched. The orientation test passes for all five.
- [ ] `calculate_mti` reproduces the 3-chain matrix and fan values in section 5. The producer impact on its consumer is > 0. KS uses `log(eps * (1 - p))`.
- [ ] Keystoneness UI help documents the Q/B and diet-split approximations.
- [ ] Non-converged or unreachable TL returns `NA` plus `warning()`. `plotfw`, `create_foodweb_visnetwork`, `get_topological_indicators` and spatial metrics run on such webs.
- [ ] Rpath TL is never overwritten. Diagnostics and pyramid accept a class-`Rpath` list. Living means `type < 2`. Cannibalism is preserved.
- [ ] `finalize_network` infers fg per vertex. The Baltic export keeps real biomass and multiple fg values.
- [ ] EcoBase results carry `met.types` and have no `biomass * 100` body mass. EwE `Type` drives Detritus/producer assignment.
- [ ] Network and Rpath paths share one biomass helper, with its semantics documented.
- [ ] Data-editor edits keep numeric columns numeric. A CSV with species not first builds a network. EMODnet enrichment works with a sampling location and has no `current_network()`.
- [ ] All new handlers use `warning()`. No bare relative `source()`. Paths use `app_path()`.
- [ ] Versions read 1.5.0 in `VERSION`, `R/config.R` and `app.R`. CHANGELOG has the "Results changed" section. The deploy is verified on laguna.

## Appendix: excluded findings

None of the 18 assigned findings was NOT-REPRODUCED or ALREADY-FIXED. All are in scope.

Two findings are in scope with corrections to the report:

- **F56:** the report's fix ("use `mode = "out"`") is **wrong** under the unified contract. After :259 is flipped, `mode = "in"` returns prey and is correct. Applying both changes would re-invert per-hexagon TL.
- **F52:** PARTIAL. The biomass discrepancy between the paths is confirmed, but which path is correct is not established. A3 step 1 resolves it before any code change.

The report's F44 note "type < 3 counts detritus as living" is confirmed. The existing test (`test-rpath-diagnostics.R:48-58`) encodes that bug and is updated in A2.
