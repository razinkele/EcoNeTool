# Fix D: Spatial Session State and Conventions Sweep

- **Status:** Design with the user's binding decisions applied. Not yet implemented.
- **Date:** 2026-09-26
- **Source:** `docs/econetool-deep-analysis-2026-09-26.md`, remediation batches 10 (spatial state) and 12 (conventions sweep)
- **Prior art:** `docs/econetool-deep-analysis-2026-07-17.md` #9, #21, #22, #25, #31, #32, #33. These were thought fixed, but F57/F59/F60/F61/F73/F80 show they are not.
- **Baseline:** master @ `7a96789`, `VERSION=1.4.4`. Test suite: 719 pass / 0 fail / 30 skip.
- **Siblings:** A (F56 `spatial_analysis.R` edge direction, excluded here), B (CI offline testthat job), C (F12 SHARK, F78/F79/F32/F33 in `database_lookups.R`, `taxonomic_api_utils.R`)

## 1. Goal and context

Shiny Server OSS runs one R process for every session. Anything process-global, such as
`sf_use_s2()`, `future::plan()` or package-level caches, leaks from one user to all of them.
Inside a session, the spatial tab keeps eight `reactiveVal`s that nothing invalidates.
A new grid or study area therefore shows the previous run's habitat, networks and metrics.

The conventions in CLAUDE.md are: `warning()` rather than `message()` in error handlers, `<<-`
in error closures, `app_path()`, `skip_if()`, and live tests gated by `RUN_LIVE_TESTS`.
They were applied file by file, and roughly 60 handlers still swallow failures invisibly,
because production `preserve_logs` is off. D makes the conventions **mechanically
enforced** with AST guard tests, so they stop regressing.

## 2. Scope (all findings re-verified against master @ 7a96789)

| ID | Sev | Current file:line | Status | Defect today |
|---|---|---|---|---|
| F57 | high | `R/modules/spatial_server.R:774` (defaults 814-817, 840-858) | CONFIRMED | `euseamap_data()` loads only when NULL and is never reset or coverage-checked. With no area set, it caches a fixed 1x1 deg box `c(20,55,21,56)` |
| F58 | med | `spatial_server.R:602-640` (create grid), `:155`, `:325` (study area set) | CONFIRMED | Grid and study-area changes never reset networks, metrics or grid_with_habitat. hex_ids restart at `HEX_0001` (`spatial_analysis.R:100`), so the merge at `:1312` repaints stale metrics |
| F59 | med | `spatial_server.R:1268` | CONFIRMED | `metrics[metrics$S > 0, ]`: when S is unselected, `NULL > 0` gives `logical(0)`, so the table has 0 rows |
| F60 | med | `spatial_server.R:789-799`, BBT handler `:251,264,276,322,425` | CONFIRMED (wider) | `sf_use_s2(FALSE)` is not restored if `st_read`/`st_zm` throws, and the handler uses `cat()`. The BBT observer forces `TRUE` instead of restoring the saved value. `"data/BBT.geojson"` (`:251,:791`) and the gdb path (`:862`) are wd-relative |
| F61 | med | `spatial_server.R:1084-1094`, observer `:1119-1150` | CONFIRMED | Only column names are checked. Non-numeric lon/lat reach `addCircleMarkers` in an unguarded `observe()` and kill the session |
| F62 | med | `R/functions/spatial_analysis.R:184` | CONFIRMED | The `aggregate(biomass ~ ...)` formula drops rows with NA biomass (`na.omit`), so species vanish from the hexagon |
| F63 | low | `spatial_server.R:1024`; `R/functions/emodnet_habitat_utils.R:708` | CONFIRMED | The overlay overwrites `spatial_hex_grid`. A second overlay merges onto existing habitat columns, which gives `.x/.y` and "replacement has 0 rows" |
| F80 | med | `R/functions/trait_lookup/api_trait_databases.R:134,221,274,353,432`; `database_lookups.R:714-721,729-731` | CONFIRMED | Five handlers use `message()`, and the result lists have no `error` field. PTDB `switch` is case-sensitive; an unknown strategy becomes `primary_producer`/TL 1 |
| F16 | low | `api_trait_databases.R:186` (+ the 5 handlers above) | CONFIRMED | `taxon_id == ""` errors on NA. Same handler set as F80, merged into it |
| F39 | low | `R/functions/ml_trait_prediction.R:83,124,286` | CONFIRMED | wd-relative `models/trait_ml_models.rds`. Both handlers use `message()`, so ML switches off invisibly |
| F53 | low | `R/functions/ecopath/ecopath_windows.R:204,217,232,244,260,272,290,368`; `ecopath_unix.R:86,107,133` | CONFIRMED | Optional-table handlers use `message()`. unix:133 is a bare `NULL` |
| F15 | low | `R/functions/ices_lookups.R:388,396,403` | CONFIRMED | `result$error <<-` in the tryCatch **body** (the function frame) raises "object 'result' not found". The AST sweep found no other instance |
| F14 | med | `ices_lookups.R:279-330` (calls), `:349` (cache write) | CONFIRMED | `with_timeout(on_timeout = NULL)` is silent, partial frames are cached for the process lifetime, and a full timeout returns `"no DATRAS rows"`. `trait_research_server.R:583` classifies that string as legitimate absence |
| F31 | low | `app.R:92-97`, `:583-593`, `:103` | CONFIRMED | `init_parallel_lookup(workers = 4)` has no callers (`lookup_species_parallel` and `batch_lookup_parallel` are unreferenced). `parallel_lookup.R` calls `stop()` at top level when `future` is missing. `onSessionEnded` resets the process-wide plan |
| F23 | low | `scripts/populate_local_databases_to_sqlite.R:25,100,177` | CONFIRMED | It sources the removed `R/functions/trait_lookup.R`, and the error counters use `<-` in handlers. `harmonize_size_class` is still needed (`local_trait_databases.R:488,564`) |
| F23b | low | `scripts/create_regional_euseamap_files.R:168` | NEW (same class) | `validation_ok <- FALSE` inside the handler is lost, so validation always reports OK |
| F73 | med | `R/modules/trait_research_server.R:316-331`; `R/ui/trait_research_ui.R:143-156` | CONFIRMED | `input$trait_research_databases` only gates the biotic/maredat/ptdb file paths. `lookup_species_traits()` (`orchestrator.R:234`) has no `databases` argument. `shark` has no backend (`orchestrator.R:458,798`) |

## 3. Non-goals

- F56 (local-network edge direction, `spatial_analysis.R:~259`) belongs to sub-project A.
- F12 SHARK wrapper rewrite belongs to sub-project C. D only allow-lists `shark_api_utils.R` handlers, pointing to C.
- F51 (`ecopath_import_server.R:1588` undefined `current_network()`) belongs to batch 7. D provides `euseamap_covers()` for it to reuse but does not edit that observer.
- F72 (the process-global trait cache ignores config) belongs to B. The F73 cache interaction is limited to the bypass rule in D2.6.
- No TTL/eviction for `.ices_cache` beyond the completeness rule. No change to the `with_timeout()` mechanics.
- UI-visible render fallbacks, meaning `cat()` inside `renderPrint` and `plot.new(); text(...)` inside `renderPlot`, already show the error to the user and are not converted.
- CI wiring of the new guard tests. B's offline testthat job picks them up automatically.

## 4. Design

### D1. Spatial session state (batch 10)

**State model.** All eight `reactiveVal`s at `spatial_server.R:19-26` are per-session. They
are created inside `server()` and passed in. `euseamap_data` is also per-session
(`app.R:722`) but is **shared with `ecopath_import_server`**, so D never NULLs it. It is
coverage-checked and reloaded instead. The only process-global state touched here is
`sf_use_s2()`, which is handled by D1.3. The invalidation DAG:

```
study_area ──┬─> hex_grid ──┬─> local_networks ──> metrics_data
             │              └─> grid_with_habitat
             └─> habitat_clipped ──> grid_with_habitat
species_data  (independent input; never reset by upstream changes)
euseamap_data (cache; valid iff attr(., "bbox_filter") covers the needed extent)
```
The node sets for each trigger are: `study_area` clears {hex_grid, local_networks,
metrics_data, habitat_clipped, grid_with_habitat}. `grid` clears {local_networks,
metrics_data, grid_with_habitat}. `networks` clears {metrics_data}. `habitat` clears
{grid_with_habitat}.

Invalidating a node NULLs every node downstream of it and clears the matching leaflet
groups (`Grid`, `Habitat`, `Metrics`, plus `clearControls()` for the metric legend).

**D1.1 `reset_spatial_downstream(from, state, map = NULL)` (F58).** This is a local
function in `spatial_server`. `from` is one of `"study_area"`, `"grid"`, `"networks"` or
`"habitat"`. `state` is a named list of the reactiveVals. The function is also exported as a
pure helper, `spatial_invalidation_targets(from)`, in
`R/functions/spatial_state.R` (new file, sourced by `app.R` beside `spatial_analysis.R`).
That helper returns the character vector of nodes to NULL, so the DAG can be unit-tested
without Shiny. Call sites:
- The study-area upload (`:155`) and the BBT load (`:325`) call `reset("study_area")`, which clears hex_grid, networks, metrics, habitat_clipped and grid_with_habitat.
- `spatial_clear_study_area` (`:581-593`) replaces its three ad-hoc resets with `reset("study_area")`.
- `spatial_create_grid` (`:640`) calls `reset("grid")` before `spatial_hex_grid(hex_grid)`.
- `spatial_extract_networks` (`:1196`) calls `reset("networks")`, which clears metrics, before storing the new networks.

`output$spatial_metrics_table` (currently assigned inside the observer at `:1267`) and
`output$spatial_extraction_info` become top-level `renderDT`/`renderPrint` blocks that read
the reactiveVals. A reset therefore blanks them instead of leaving stale renders.

**D1.2 Habitat cache coverage (F57).** In `R/functions/spatial_state.R`:
- `euseamap_covers(euseamap, bbox)` returns `FALSE` when `euseamap` is NULL, when `attr(euseamap, "bbox_filter")` is missing, or when the stored bbox does not contain `bbox` (xmin/ymin/xmax/ymax comparison, tolerance 1e-9). `load_regional_euseamap()` already sets that attribute (`euseamap_regional_config.R:248,271`).
- `habitat_target_bbox(study_area_sf, hex_grid)` returns `habitat_load_bbox(st_bbox(study_area))` if a study area exists, else the grid bbox, else `NULL`. There is **no default box**.

In `spatial_server`, lines 779-873 become the local function
`load_habitat_for_extent(target_bbox)`, which wraps `load_regional_euseamap(custom_bbox =
target_bbox, path = app_path("data/EUSeaMap_2025/EUSeaMap_2025.gdb"))`. The rules:
- The checkbox observer (`:773`) triggers on enable. If `habitat_target_bbox()` is NULL, it shows a warning notification ("Define a study area or grid first"), calls `updateCheckboxInput(..., FALSE)` and returns. Otherwise it loads only if `!euseamap_covers(euseamap_data(), target)`.
- After `reset("study_area")`, the study-area handlers call the same check when `isTRUE(input$spatial_enable_habitat)`.
- `spatial_clip_habitat` (`:923`) re-checks coverage before clipping, which is a cheap guard against the ecopath tab having loaded a different extent.
- Delete the silent default (`:814-817`) and the unreachable region-test block (`:840-858`, including the bare-fallback handler at `:851`).

**D1.3 `with_s2_disabled(expr)` (F60).** Add it to `R/functions/spatial_state.R`:
```r
with_s2_disabled <- function(expr) {
  old <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old), add = TRUE)
  force(expr)
}
```
Add `read_bbt_polygon(name)` in the same file. It reads
`app_path("data/BBT.geojson")` inside `with_s2_disabled()`, filters by `Name`, calls
`st_zm()`, and returns the sf, or calls `stop()` with a clear message when the name is
absent. Uses:
- The BBT observer (`:251-322`) replaces its manual `sf_use_s2` toggles (`:264,276,322,425`) with `read_bbt_polygon()`. The simplify/validity steps also run inside `with_s2_disabled()`.
- The habitat observer's nested load (`:789-799`) becomes `study_area_sf <- tryCatch(read_bbt_polygon(bbt_name), error = function(e) { warning(...); NULL })`. Assigning the tryCatch value removes the `<-`-in-closure bug.
Since R is single-threaded, save/restore inside one synchronous call cannot interleave with another session.

**D1.4 Species upload validation (F61).** Add
`validate_species_occurrences(df, bbox = NULL)` in `spatial_state.R`. It returns
`list(data, dropped, messages)`. Steps:
1. If the file has 1 column containing `;`, re-read it with `read.csv2`.
2. Coerce `lon`/`lat` with `as.numeric(gsub(",", ".", x))`.
3. Drop rows that are non-finite, have lon outside [-180, 180] or lat outside [-90, 90], or have an empty species.
4. If more than 50% of the kept points fall outside `bbox` and the swapped (lat, lon) fall inside, add the message "lon/lat appear swapped".
5. Coerce `biomass` to numeric if present.
6. If zero rows survive, call `stop()`.

The upload handler (`:1084`) uses it, shows the messages as a warning notification and
calls `warning()` with the dropped count. The marker observer (`:1119`) is wrapped in
`tryCatch(..., error = function(e) { warning(...); showNotification(...) })`.

**D1.5 Metrics table (F59).** The display filter uses network size rather than the S column:
`nonempty <- names(local_networks)[vapply(local_networks, igraph::vcount, 0L) > 0]`, then
`metrics[metrics$hex_id %in% nonempty, ]`. This works whatever metrics are selected.

**D1.6 NA biomass (F62).** In `assign_species_to_hexagons` (`spatial_analysis.R:180-189`):
`aggregate(biomass ~ hex_id + species, data = joined_df, na.action = na.pass,
FUN = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE))`. If any biomass is
NA, it calls `warning("[assign_species_to_hexagons] %d occurrences have NA biomass; kept for
presence")`. Presence, and therefore species lists, never depends on biomass.

**D1.7 Idempotent overlay (F63).** There are two sides to this fix:
- `overlay_habitat_with_grid()` drops the existing habitat columns (the `habitat_cols` and `numeric_cols` names at `emodnet_habitat_utils.R:717-727`, plus `cell_id` if it is not the join key being created) from `grid_cells` before the merge at `:708`.
- `spatial_server` stops overwriting the base grid (delete `:1024`) and adds `active_grid <- reactive(spatial_grid_with_habitat() %||% spatial_hex_grid())`. It is used by the metric-map observer's habitat branch (`:1319`) and the RDS export (`:1476`). Network extraction keeps using the pristine `spatial_hex_grid()`.

### D2. Conventions sweep (batch 12)

**D2.1 Error-handler site table.** An AST sweep with `parse(keep.source = TRUE)` found every
`tryCatch(error = function(e) ...)` under `R/` and `app.R`. There are 230 handlers:
87 already call `warning()`/`stop()`, 32 notify the UI, and 111 do neither. Handler line
numbers are listed below. "Convert" means `warning(sprintf("[scope] ...: %s", label,
conditionMessage(e)), call. = FALSE)` followed by the existing return value. "Allow" means
adding a `# swallow-ok: <reason>` comment inside the handler.

| File | Handler lines | Action |
|---|---|---|
| `trait_lookup/api_trait_databases.R` | 134, 221, 274, 353, 432 | Convert. Add `error = NA_character_` to the 5 result inits (72, 158, 241, 296, 374) and set `result$error <- conditionMessage(e)` before returning `result` (a local copy is correct here because the handler returns it) |
| `trait_lookup/database_lookups.R` | 123, 209, 278, 355, 434, 532, 610, 770, 1181 | Add `warning()`. These already `<<-` into `result$error`. Rebase on C (it owns 245-282) |
| `trait_lookup/database_lookups.R` | 69, 74, 202, 988 | Convert. `with_timeout(on_timeout = NULL)` still returns NULL on timeout, so only non-timeout errors warn |
| `trait_lookup/database_lookups.R` | 798, 803, 910 | Convert. At 910, warn in the non-timeout branch only |
| `trait_lookup/csv_trait_databases.R` | 30 / 25 | Convert / Allow (normalizePath fallback) |
| `ml_trait_prediction.R` | 124, 286 | Convert. The missing-file branch at 86-87 also becomes `warning()`, and line 83 becomes `app_path("models/trait_ml_models.rds")` (F39) |
| `ecopath/ecopath_windows.R` | 204, 217, 232, 244, 260, 272, 290, 368 | Convert (F53) |
| `ecopath/ecopath_unix.R` | 86, 107, 133 | Convert (F53) |
| `taxonomic_api_utils.R` | 76, 165, 279, 310, 355, 480, 644, 746 / 566 | Convert / Allow (numeric coercion). Rebase on C |
| `rphylopars_imputation.R` | 56, 112 | Convert |
| `phylogenetic_imputation.R` | 240 | Convert. Warn once per lookup with a count of the skipped files, not one warning per file |
| `trait_foodweb.R` | 303 | Convert as an aggregate: count failures in the loop and emit one `warning()` after it |
| `ecobase_connection.R` | 266 / 696 | Convert / Allow (connection-test diagnostic; returns FALSE) |
| `ices_lookups.R` | 283, 303 | Replaced by D2.5 |
| `shark_api_utils.R` | 72, 149, 225, 346, 414, 457, 499 | Allow with `# swallow-ok: F12, owned by sub-project C`. C converts or deletes them |
| `error_logging.R` | 91, 99, 241 | Allow (the logger must not recurse into itself) |
| `ecopath/ecopath_diagnostics.R` | 47, 99 | Allow (CLI diagnostic; `cat` output is the product) |
| `emodnet_habitat_utils.R` | 778 | Allow (`test_emodnet_integration` CLI harness) |
| `feedback_store.R` | 87, 152, 186 | Allow (best-effort WAL checkpoint in `on.exit`) |
| `validation_utils.R` | 137 | Allow (`safe_get` default) |
| `api_rate_limiter.R`, `parallel_lookup.R` | 322; 98, 135 | Allow (latent code with no callers; F13 refuted-as-unreachable) |
| `R/modules/spatial_server.R` | 796, 851 | Removed by D1.2/D1.3 |
| `R/modules/dataeditor_inline_server.R` | 44 | Convert (observer `cat()` is invisible) |
| `app.R` | 590 | Removed by D2.7 |
| other `R/modules/*` handlers | `renderPrint`/`renderPlot`/`renderUI` fallbacks | Out of guard scope (UI-visible) |

The PTDB fix (F80) at `database_lookups.R:714-731` works like this:
`strategy <- tolower(trimws(...))`. Unrecognised values give `feeding_mode = NA`,
`trophic_level = NA` and a `warning()`. A **missing** strategy keeps
`primary_producer`/1.0, because PTDB is a phytoplankton database. The F16 guard at
`api_trait_databases.R:186` becomes `if (is.null(taxon_id) || is.na(taxon_id) || !nzchar(taxon_id))`.

**D2.2 Guard test: handlers must surface failures.** Add
`tests/testthat/test-conventions-guard.R`. It walks the AST of every
`R/functions/**/*.R` file (port of the sweep script) and, for each
`tryCatch(error = function(e) ...)`, checks two things:
- **Surfaced:** the handler calls `warning`, `stop`, `abort`, `warn` or `signalCondition`.
- **Allowed:** the handler's source lines (from the function literal's srcref) contain `# swallow-ok:` followed by at least 10 characters of reason.

The test fails on any handler that is neither. Offenders are reported as `file:line`. The
`# swallow-ok: F12` markers are removed by C when it rewrites or deletes those wrappers. If
D2 merges after C, the D2 PR does not add them. `R/modules` is excluded because a UI
fallback is legitimate there. The two module sites are pinned by named assertions in
D1/D2 tests instead.

**D2.3 Guard test: closure assignment direction (F15, F23).** Two more AST checks run in
the same file over `R/`, `app.R` and `scripts/`:
- (a) `<<-` inside a tryCatch **body** (first argument, not within a nested `function`) whose target base name is assigned with `<-` in the nearest enclosing function frame. Today's only hit is `ices_lookups.R` `fetch_sag_ssb`.
- (b) `<-` inside an error-handler body to a name that is a local of the enclosing frame, where that name is not the handler's return value. Names bound to reference objects (`output`, `rv`, `session`, `values`) are excluded. Today's hits are `populate_local_databases_to_sqlite.R:100,177`, `create_regional_euseamap_files.R:168`, `spatial_server.R:798` and `api_rate_limiter.R:322` (allow-listed with a `# swallow-ok:`-style `# closure-ok:` comment).

**D2.4 F15.** Replace `result$error <<-` with `<-` at `ices_lookups.R:388,396,403`. Keep `<<-` in the handler at `:407`.

**D2.5 DATRAS completeness (F14).** In `lookup_datras_indices`:
- Define `.timeout_sentinel <- structure(list(), class = "ices_timeout")` and pass it as `on_timeout` at the three calls (`:281,:301,:315`). Treat `inherits(x, "ices_timeout")` as a failure: increment `n_failed` and append `"<svy> <yr> Q<q>: timeout"` to `failures`. The `error = function(e)` handlers at `:283,:303` also count their errors as failures, with `warning()`.
- Add the result fields `complete` (`n_failed == 0`) and `failures` (character).
- Write `.ices_cache[[cache_key]] <- result` only when `result$success && result$complete`.
- With zero rows and `n_failed > 0`, return `success = FALSE` and `error = sprintf("DATRAS unavailable: %d request(s) failed or timed out", n_failed)`. This string must **not** start with `"no DATRAS rows"`.
- With some rows and failures, return `success = TRUE`, `complete = FALSE` and a `warning()` listing the failures.

Consumers:
- `rpath_server.R:1967` becomes `if (!isTRUE(res$success) || !isTRUE(res$complete))`, with the exclusion reason `"incomplete survey data (timeouts)"`.
- `trait_research_server.R:579-587` shows a warning notification "DATRAS: partial data (N requests failed)" when `!isTRUE(res$complete)`.

`fetch_sag_ssb` uses the same sentinel, so a timeout reports `"SAG timed out"` rather than `"no SAG assessment"`. That result is already uncached because only successes are cached.

**D2.6 Databases checkboxes (F73).** Wire the checkboxes where a backend exists.
- `lookup_species_traits(..., databases = NULL)`. After `route_flags` (`orchestrator.R:471`), if `databases` is non-NULL, AND each mapped flag with membership. The map is `fishbase`, `sealifebase`, `biotic`, `freshwater`, `maredat`, `ptdb` and `algaebase` to the same-named flag, and `worms` to `worms_attrs`. Unmapped flags (bvol, polytraits, obis, …) keep routing-only behaviour. `batch` (`:1792`) forwards `...` unchanged.
- UI (`trait_research_ui.R:143-156`): remove the `shark` choice, because it has no backend. Label WoRMS "WoRMS (taxonomy, always used)". The classification call is unconditional, and the checkbox gates only the WoRMS attribute lookup. The default `selected` becomes all 8 wired keys, which preserves today's effective behaviour.
- Server (`trait_research_server.R:316-405`): pass `databases = databases_to_check`. When the selection is not the full set, bypass both the module cache read (`:377-398`) and the orchestrator cache (`cache_dir = NULL`). A subset result must never be served to, or stored for, a full-set lookup.

**D2.7 Remove the startup future plan (F31).**
- Delete `app.R:91-98` (source and `init_parallel_lookup`) and the `onSessionEnded` block `:583-593`.
- Remove the banner line `:103` and the header bullet `:59`.
- Keep `R/functions/parallel_lookup.R`: tests read it (test-deep-analysis-fixes.R:331, 534, 545), and `tests/test_phase6_performance.R` sources it directly. Add a header comment: "Not loaded at startup; no callers (deep-analysis F31/F13)."

**D2.8 Populate scripts (F23, F23b).**
- `populate_local_databases_to_sqlite.R:25` becomes `source("R/functions/trait_lookup/load_all.R")`. The scripts run from the repo root, like `load_all.R`.
- Change `<-` to `<<-` at `:100` and `:177`.
- `create_regional_euseamap_files.R:168` becomes `validation_ok <<- FALSE`.

## 5. Testing strategy (TDD: every case is written red first)

| Test file | Cases |
|---|---|
| `tests/testthat/test-spatial-state.R` (new) | `spatial_invalidation_targets()` returns exactly the four node sets listed under D1 (`setequal`). No set ever contains `species_data`. `euseamap_covers()` returns TRUE for contained and FALSE for disjoint, NULL or missing-attr inputs. `habitat_target_bbox(NULL, NULL)` is NULL. `with_s2_disabled(stop("x"))` restores the prior value for both TRUE and FALSE starts, and `with_s2_disabled(1)` returns 1. `read_bbt_polygon("nope")` errors and leaves s2 unchanged. `validate_species_occurrences()` handles comma decimals, drops out-of-range rows with a count, flags swapped lon/lat, reads `;` separators, and errors on zero rows |
| same file, `shiny::testServer(function(input, output, session) spatial_server(input, output, session, reactiveVal(), reactiveVal(), reactiveVal(), reactiveVal(NULL)), {...})` | (1) With networks and metrics set, `session$setInputs(spatial_xmin=..., spatial_create_grid=1)` leaves `spatial_local_networks()` and `spatial_metrics_data()` NULL. (2) With metrics lacking S, the metrics-table render has more than 0 rows. (3) The habitat checkbox with no area or grid leaves `euseamap_data()` NULL, with no default box. (4) The BBT selector with a mocked `read_bbt_polygon` that throws leaves `sf::sf_use_s2()` equal to its prior value. (5) A bad-coordinates upload fixture (`tests/testthat/fixtures/spatial_bad_coords.csv`) does not error the session |
| `tests/testthat/test-spatial-analysis-fixes.R` (new) | `assign_species_to_hexagons` keeps (x,1) and (y,NA) in one cell, gives biomass NA for y, and warns. `overlay_habitat_with_grid(overlay_habitat_with_grid(g, h), h)` has no `.x/.y` columns and the same values as a single overlay (tiny synthetic sf fixtures, no GDB) |
| `tests/testthat/test-conventions-guard.R` (new) | D2.2 handler guard. D2.3 (a) and (b) closure guards. `app.R` contains no `init_parallel_lookup(` or `onSessionEnded(...shutdown_parallel_lookup`. The populate script does not reference `trait_lookup.R"`. `ml_trait_prediction.R` does not contain the literal `"models/trait_ml_models.rds"` outside `app_path(` |
| `tests/testthat/test-ices-lookups.R` (extend, offline) | Stub `with_timeout` in the local env to return the sentinel for one year and data for others. Expect `success`, `!complete`, `length(failures)==1`, the cache not written (`.ices_cache` has no key) and a warning. For all-timeout, expect `success=FALSE` and an error not matching `^no DATRAS rows`. For `fetch_sag_ssb` with `findAssessmentKey` stubbed to error, expect `res$error` equal to the stub message, not "object 'result' not found" |
| `tests/testthat/test-api-trait-handlers.R` (new) | With `httr::GET` mocked (`testthat::local_mocked_bindings`) to throw, each of the 5 lookups gives `expect_warning()` and `!is.na(res$error)`. PolyTraits with `taxonID = NA` produces no error. PTDB `"Autotroph"` maps to `primary_producer` and `"weird"` gives NA plus a warning. `load_ml_models()` with a corrupt RDS at a temp `econetool.app_root` warns |
| `tests/testthat/test-trait-routing.R` (extend) | `lookup_species_traits(..., databases = c("worms"))` for a fish never calls `lookup_fishbase_traits` (mocked binding counts calls). With `databases = NULL`, the routing is unchanged |
| `tests/testthat/test-trait-research-databases.R` (new, testServer) | A subset selection calls `lookup_species_traits` with `cache_dir = NULL` and `databases` equal to the selection. The UI choices do not contain `shark` |

Live behaviour stays behind `RUN_LIVE_TESTS` with `with_timeout()`, and no new live tests are added.
The run command is `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"` (the legacy `tests/run_all_tests.R` needs `dggridR` and cannot run locally).
Parse-check every edited `.R` file.

## 6. Rollout

| PR | Content | Version | Depends on |
|---|---|---|---|
| D1 | Batch 10: `spatial_state.R` + spatial_server/spatial_analysis/emodnet changes + tests | next patch at merge (1.4.x) | A merged first (A edits `spatial_analysis.R:~259`; D1 edits `:180-189`) |
| D2 | Batch 12: handler conversions, guards, F14/F15/F31/F23/F73 | next patch after D1 | C's PRs touching `database_lookups.R`/`taxonomic_api_utils.R` merged first. D2 rebases and re-runs the guard, which is the source of truth for line numbers |

Each PR bumps `VERSION` and adds a CHANGELOG entry per the existing `chore(release)` pattern,
then deploys via the MEMORY workflow (pre-deploy check from `deployment/`,
`-SkipData -NoSudo`, `cp -rT`, `touch restart.txt`). After the D2 deploy, check on laguna
that `models/trait_ml_models.rds` resolves (`app_path`), because a missing file now warns.
Also confirm that the app starts without `future` loaded.

## 7. Risks and open questions

- **leafletProxy under `MockShinySession`.** `testServer` cases assert only reactive state and outputs. If `leafletProxy` errors in the mock, wrap the proxy calls in a local `map_proxy()` that returns NULL when `session` inherits `MockShinySession`, rather than dropping the testServer cases.
- **Warning volume.** Converting ~45 handlers can flood the console during WoRMS/FishBase outages. The mitigation is the per-loop aggregation at 240/303 and scoped `[source]` prefixes. Nightly live tests will now see these warnings, which is intended.
- **Resetting the grid on a study-area change** discards a user's grid, which is a behaviour change. It is correct because the grid is clipped to the old area. It is announced in the notification text ("Grid cleared: study area changed").
- **F73 cache bypass** makes subset lookups slower. The default selection is the full set, so the default path keeps caching.
- **Shared `euseamap_data`.** Once F51 is fixed, the ecopath tab could load a smaller extent. D's coverage check in the clip handler makes the spatial tab reload rather than use it.
- **Merge order with A and C.** Line numbers in §2 and D2.1 are as of `7a96789`. The guard test, not this table, is authoritative after rebases.

## 8. Acceptance criteria

1. The full test suite passes with 0 fails, and the new test files are red before the fixes and green after.
2. `test-conventions-guard.R` passes: no unmarked swallowing handler in `R/functions`, and no F15/F23-class closure misuse in `R/`, `app.R` or `scripts/`.
3. `grep -rn "sf_use_s2(TRUE)\|sf_use_s2(FALSE)" R/modules` returns nothing. All toggles go through `with_s2_disabled()` or the existing save/restore sites.
4. `grep -n "c(20, 55, 21, 56)" R/modules/spatial_server.R` returns nothing.
5. Manual smoke on laguna: enable habitat, pick Bay_of_Gdansk, clip and overlay twice. Cells have real EUNIS codes and no error toast. Change the cell size and the metrics layer clears.
6. `app.R` startup log has no "Parallel processing enabled" line, and `future::plan()` is `sequential` in a live session.
7. Trait Research with only WoRMS ticked shows no FishBase source in results for *Gadus morhua*.

## Appendix: excluded findings

None of the 16 assigned findings was refuted or found already fixed. All reproduce at
`7a96789`. Scope adjustments:
- **F16** is merged into F80. It covers the same five handlers, plus the `taxon_id` NA guard.
- **F60** is widened to the BBT observer's forced `sf_use_s2(TRUE)` (`:276,322,425`) and the wd-relative `BBT.geojson`/gdb paths.
- **F23b** (`create_regional_euseamap_files.R:168`) is added. It is the same bug class and was found by the D2.3 sweep.
- **F13** (`api_rate_limiter.R:322` `delay` not propagated) was refuted as unreachable in the report. It is allow-listed, not fixed.
