# Fix E: Rpath Dynamics (Ecosim, Sensitivity, Fishing, Edit Loop, Fleet/Detritus Schema)

- **Status:** Design, ready for plan
- **Date:** 2026-09-26
- **Source:** `docs/econetool-deep-analysis-2026-09-26.md`, remediation batch 9 (F43, F45, F46, F47, F42)
- **Depends on:** Sub-project A (batch 2: F40 TL override, F44 `.require_balanced_model`, F41 cannibalism), specifically A2 merged first
- **Rpath reference:** NOAA-EDAB/Rpath v1.1.0 (GitHub default-branch HEAD `55fe84d`, 2025-09-05). **Rpath is not installed locally**; the API below was read from the package source (`R/param.R`, `R/ecopath.R`, `R/ecosim.R`, `R/Adjustments.R`) fetched from GitHub. Rpath is not on CRAN or r-universe.

## 1. Goal & context

The Rpath tab can balance a model, but everything downstream of balancing is broken. Ecosim fails on both paths. Sensitivity analysis always errors. The fishing scenario ignores the chosen fleet and shows a success toast with an empty plot. The parameter editor disappears after a balance. Fished EwE models also balance with F = 0, because catches, fleets and detritus fate never reach Rpath's schema. This sub-project makes the chain convert → edit → balance → simulate → scenario work end to end against the real Rpath API. It follows the binding user decisions:

- **TDD.** Every change starts with a failing test.
- **Dead UI.** Wire a control if a backend exists. Otherwise remove it.
- **No false success.** A scenario input is either applied, or the UI says plainly that it was not applied. Nothing is reported as "configured" unless it was.

Rpath conventions used below: the diet matrix is prey rows × predator columns. `Type` is 0 consumer, 1 producer, 2 detritus, 3 fleet. `create.rpath.params(group, type, stgroup = NA)` builds `model` with 10 fixed columns (`Group, Type, Biomass, PB, QB, EE, ProdCons, BioAcc, Unassim, DetInput`). It then adds one column per detritus group (detritus fate), one landings column per fleet (named after the fleet), and one `<fleet>.disc` discards column per fleet. `rpath()` reads these **positionally** (`model[, (10+1):(10+ndead)]`, then the landings and discards). `check.rpath.params()` requires `ncol(model) == 10 + n.dead + 2 * n.fleet`.

## 2. Scope

Line numbers are those on `master` at 7a96789. A2 edits `rpath_balancing.R:266-284` and `rpath_conversion.R:343-368`, so re-read line numbers after rebasing onto A.

| ID | Sev | Current location | Verification | Defect |
|---|---|---|---|---|
| F42 | high | `R/functions/rpath/rpath_conversion.R:150-170, 186-189, 227-255`; `R/functions/ecopath/ecopath_windows.R:222-247, 395-407`; `ecopath_unix.R` (returns group/diet/model only); `rpath_balancing.R:176-183` | **CONFIRMED** (upgraded from "plausible") | EwE stores fleets in `EcopathFleet`, never as Type-3 rows in `EcopathGroup`. So `has_fleets` is always FALSE and `DummyFleet` is always added. `EcopathCatch` (GroupID, FleetID, Landing, Discards) is never read. Fleet landing and `.disc` columns stay 0, so F = 0. `EcopathGroup` has no `DetritusFate` column, so `params$model$DetFate` is always 0. The real fate (`EcopathDietComp.DetritusFate`) is ignored and the per-detritus fate columns stay NA. The ad-hoc `DetFate/Catch/Immigration/Emigration` columns break the `check.rpath.params` column-count rule. Evidence: `examples/Coastal model EE 1.ewemdb` has 1 fleet, 13 non-zero landings and 2 non-zero discards. `LT2022_0.5ST_final7.eweaccdb` has 9 landings and 3 discards. Both would currently balance with no fishing. |
| F43 | high | `R/modules/rpath_server.R:1461-1522` (calls at 1498-1511); `R/functions/rpath/rpath_simulation.R:64-109, 111-142, 277-296, 322-370, 376-532` | **CONFIRMED** | Default path: the server passes `method=` (1510), which `setup_ecosim_scenario()` does not accept, so the call fails with "unused argument". The function calls `rsim.scenario(model, model, years = 50)` (86-88). The real signature is `rsim.scenario(Rpath, Rpath.params, years = 1:100)`: argument 2 must be the params (used by `rsim.stanzas`), and a scalar `years` stops. `rsim.run(..., method = "RK4")` (128) hard-codes RK4 and omits `years`, and its default `1:100` fails the row lookup on a 50-year scenario. Scenario path: `rsim.scenario(Rpath.params=, Rpath.sim=NULL)` (417-420) is an unused argument. The vulnerability loop (453-462) is a no-op. Group and fleet blocks (468-525) only print, yet print "✓ … configured" (464, 494, 523, 527). The plot and the extract functions melt or select a `time` column that `rsim.run` output lacks (289, 311, 349). The UI offers "Euler" (366), which `rsim.run` rejects. The viz controls `plot_type`, `selected_groups`, `show_legend` and `btn_update_plot` (379-388, 1549-1571) are never read by `ecosim_biomass_plot` (1535). |
| F45 | medium | `rpath_server.R:1756-1762`; `rpath_simulation.R:148-217` | **CONFIRMED** | The server passes `group=, range=, steps=`. The formals are `(rpath_model, parameter, variation, n_sims)`, so the call fails with "unused arguments". The body reads `rpath_model$params` (179), which the balanced object does not have. The UI value `"B"` is not a model column (`Biomass`). The plot (1788-1802) reads `param_values`, `total_biomass` and `baseline_value`, which are never returned. The error is swallowed as `warning()` + NULL (213-216). |
| F46 | medium | `rpath_simulation.R:223-271`; `rpath_server.R:1842-1849, 1868-1875` | **CONFIRMED** | `fleet_name` is never used. The server's `fleet=` partial-matches it, but line 254 scales `ForcedEffort` for every column. Errors become `warning()` + NULL (267-270), so the server still shows "Scenario complete!" (1849). The plot passes the `list(baseline, scenario, …)` to `plot_ecosim_results()`, which reads `$out_Biomass` and gets NULL. The builder inherits the F43 signature failures. |
| F47 | medium | `rpath_server.R:890-895` (reset), `723` (gate), `685-688, 845-881, 965-977, 986-991, 1246-1268` | **CONFIRMED** (gate). **PARTIAL** (reset: the defect is confirmed by reading the code, not by running it) | Reset stores `as.data.frame(...)` (captured at 702) into `params$model`. `rpath()` then runs data.table syntax (`model[, Type]`, `with = F`) on a data.frame and fails. The exact message ("object 'Type' not found") is unverified because Rpath is not installed. The table gate is `req(status == "converted")`, and balancing sets `"balanced"` (1261), so the editor goes blank. Edits and resets never clear `ecopath_model` or any downstream result, so stale results remain after edits. |

## 3. Non-goals

- F40 (TL override), F41 (cannibalism), F44 (`.require_balanced_model` and `type < 2`). These belong to sub-project A.
- Multi-stanza parameter import (`rpath_conversion.R:377-399` writes a non-Rpath stanza structure, but it is unreachable because `EcopathGroup` has no `StanzaID`). The pedigree block (405-413) is likewise unreachable. Both are recorded as follow-ups.
- Ecosense or Monte-Carlo sensitivity (`rsim.sense`). Sensitivity stays a one-at-a-time Ecopath re-balance.
- Environmental forcing (`adjust.forcing`) and time-series fitting.
- `.survey_trends_dict` wd-relative path (`rpath_server.R:1879`). This belongs to another batch.

## 4. Design

### E1: Params schema (fleets, catches, detritus fate)

**Importers**
- `ecopath_windows.R`: after the `EcopathDiscardFate` read (~237-247), add a `tryCatch` read of `EcopathCatch` into `catch_data`. The error handler calls `warning()`. Return `catch_data` in the result list (395-407).
- `ecopath_unix.R`: read `EcopathFleet`, `EcopathCatch` and `EcopathDiscardFate` via `Hmisc::mdb.get` in separate `tryCatch` blocks. Missing tables give NULL plus `warning()`. Return `fleet_data`, `catch_data` and `discard_fate`.
- The CSV importer has no fleet data. The converter falls back as described below.

**Converter** (`convert_ecopath_to_rpath`). Two new pure helpers go in `rpath_conversion.R`. Neither needs Rpath.
- `build_fleet_table(groups, fleet_data, catch_data)` returns `list(fleets = <chr>, landings = <matrix group × fleet>, discards = <matrix>)`. Fleet names come from `fleet_data$FleetName`, in `Sequence` order, with duplicates de-duplicated. `catch_data` rows map `GroupID → GroupName` and `FleetID → FleetName`. Values of −9999 or below become 0. Rows with unknown IDs are dropped with a `warning()` naming them.
- `build_detfate_table(groups, diet_data, discard_fate, fleets)` returns a matrix with rows `Group` (living + detritus + fleets) and columns = detritus groups. Source: `diet_data` rows where `PreyID` is a Type-2 GroupID. The value at `[PredID group, PreyID detritus]` is `DetritusFate`. This is the reverse pivot of the diet itself: the verified example DBs have `DetritusFate` summing to 1 per `PredID`. Fleet rows come from `discard_fate` (`GroupID` = detritus, `FleetID`). Fallbacks, each raising one `warning()` that lists the groups affected:
  - A living group whose fate sums to 0 sends fate 1 to the first detritus group.
  - A fleet with no discard fate sends 1 to the first detritus group.
  - Detritus rows default to 0, which Rpath permits.
- Group rows are `living_groups` (Types 0–2) followed by the fleet names as Type 3. `create.rpath.params(group, type)` then creates the real fate, landing and `.disc` columns. The fate block goes into `model[, det_names]`, landings into `model[, fleet]` and discards into `model[, paste0(fleet, ".disc")]`. Fleet rows keep NA in landing and discard columns, as Rpath expects.
- `DummyFleet` is added **only** when `build_fleet_table` returns zero fleets, because Rpath's `1:length(fleet.group)` loops break at zero. `DummyFleet` gets zero landings and a fate of 1 to the first detritus group. The conversion attaches `attr(params, "econetool_dummy_fleet") <- TRUE`.
- Delete the ad-hoc `DetFate`, `Catch`, `Immigration` and `Emigration` model columns (227-255). If any group has non-zero `Immigration` or `Emigration`, raise a `warning()` saying these are not represented in Rpath and were not applied. Store them in `params$econetool_extras` (a data.frame of Group, Catch, Immigration and Emigration) for display only. Extra list elements are ignored by `rpath()`.
- After building, call `Rpath::check.rpath.params(params)` inside `withCallingHandlers`. Collect its warnings into `attr(params, "check_warnings")` and re-raise them as `warning()`. Do not stop: balancing reports hard failures itself.

**Balancer** (`rpath_balancing.R:176-183`): delete the `DetFate` patch block. Its column no longer exists.

**UI** (`rpath_server.R:1812-1826`, fleet selector): list Type-3 groups except `DummyFleet`. If none remain, show `helpText("No fishing fleets in the imported model; fishing scenarios need EcopathFleet/EcopathCatch data")` and disable `btn_run_scenario` with `shinyjs::disable` (shinyjs is already loaded by app.R). The conversion status (1236-1243) lists the fleets and states "n landings / m discards imported". When the fallback fired, it says "no fleet data: model has no fishing".

### E2: Ecosim, sensitivity and fishing call signatures

New state: `rpath_values$balanced_params`. The balance handler (1256-1261) sets it to `rpath_values$params` at the moment of the successful balance. Every dynamic run uses `(ecopath_model, balanced_params)`, never the live-edited params.

`rpath_simulation.R`. Every function calls `check_rpath_installed()` and lets errors propagate. Every `warning()` + `return(NULL)` swallow (213-216, 267-270) is removed.

```r
setup_ecosim_scenario(rpath_model, rpath_params, years = 50)
  # yrs <- seq_len(years); Rpath::rsim.scenario(rpath_model, rpath_params, years = yrs)
  # drops fishing_effort/environmental_forcing args (never passed; dead)
run_ecosim_simulation(rsim_scenario, method = c("RK4", "AB"), years = NULL)
  # method <- match.arg(method); years defaults to rownames(rsim_scenario$fishing$ForcedFRate)
  # Rpath::rsim.run(rsim_scenario, method = method, years = as.numeric(years))
setup_ecosim_scenario_with_data(rpath_model, rpath_params, ecosim_data, scenario_id, years = 50)
  # returns list(scenario = <Rsim.scenario>, applied = <chr>, not_applied = <chr>)
run_sensitivity_analysis(rpath_params, group, parameter = c("Biomass", "PB", "QB"),
                         range_pct = 20, steps = 10)
  # returns list(param_values, total_biomass, balanced (lgl), baseline_value, group, parameter)
evaluate_fishing_scenario(rpath_model, rpath_params, fleet_name, effort_multiplier = 1, years = 50)
  # returns list(baseline = <Rsim.output>, scenario = <Rsim.output>, fleet, effort_multiplier)
plot_ecosim_results(rsim_results, groups = NULL, type = c("biomass", "catch", "relative"),
                    baseline = NULL, show_legend = TRUE)
```

- **Output shape.** `ecosim_long(rsim_output, type)` (new, internal) uses `annual_Biomass` or `annual_Catch`. Rows are years (`as.numeric(rownames)`) and columns are groups; the `"Outside"` column is dropped. It returns long `data.frame(year, group, value)`. `extract_ecosim_biomass` and `extract_ecosim_catch` use it (fixing lines 289/311). `type = "relative"` divides by `baseline` when one is given, otherwise by the first year.
- **Imported scenario mapping** (replaces 404-531). Every row either calls `adjust.scenario` or is listed in `not_applied`.

| EwE field | Rpath target | Rule |
|---|---|---|
| `EcosimScenarioForcingMatrix.vulnerability` (PredID, PreyID) | `adjust.scenario(s, "VV", group = prey, groupto = pred, value = v)` | IDs map `EcosimScenarioGroup.GroupID → EcopathGroupID → GroupName` (the example DB has scenario GroupID 46 ↔ EcopathGroupID 1). Links that cannot be mapped go to `not_applied`. EwE and Rpath share the vulnerability scale (2 = mixed; Rpath default `mscramble = 2`). |
| `EcosimScenarioGroup.FtimeAdjust` | `adjust.scenario(s, "FtimeAdj", group, value)` | Applied only where not NA and not ≤ −9000. |
| `StepSize`, `SystemRecovery`, `Pbmaxs`, `FtimeMax`, `RecruitmentCV`, `RStockRatio`, fleet `MaxEffort`, `QuotaType`, `Epower` | none | Listed in `not_applied` as `"<field>: no Rpath equivalent"`. |

  All `message("✓ … configured")` lines are deleted. The server shows `applied` in the success toast. When `not_applied` is non-empty, it shows a separate `type = "warning"` notification: "Not applied (no Rpath equivalent): …". The status panel (1525-1532) lists both. `TotalTime` is not used; the user's `sim_years` governs.
- **Sensitivity.** Map the parameter code with `c(B = "Biomass", PB = "PB", QB = "QB")[input]`. If the chosen group's value is NA (Ecopath estimates it), `stop()` with "<param> for <group> is estimated by Ecopath; choose an entered parameter". Build the grid with `seq(-range_pct, range_pct, length.out = steps) / 100`. For each step, copy the params with `data.table::copy`, scale the cell, run `Rpath::rpath()` inside `tryCatch`, and record `total_biomass = sum(Biomass[type < 2])`. A failed step records NA with `balanced = FALSE` and raises `warning()` (`<<-` is not needed because the value is returned from the closure). `baseline_value` is the unscaled value. The server call (1756) becomes `run_sensitivity_analysis(rpath_values$balanced_params, input$sens_group, input$sens_parameter, input$sensitivity_range, input$sensitivity_steps)`. The plot (1779-1805) plots points only where `balanced` is TRUE and states "k of n steps failed to balance" in the subtitle.
- **Fishing.** Build `base <- setup_ecosim_scenario(...)`, then `scen <- Rpath::adjust.fishing(base, "ForcedEffort", group = fleet_name, sim.year = seq_len(years), value = effort_multiplier)`. Run both. `stop()` if `fleet_name` is not in `base$params$spname` or is `DummyFleet`. The server plot (1873) calls `plot_ecosim_results(fs$scenario, type = "relative", baseline = fs$baseline, groups = input$selected_groups)`.
- **Server** (`rpath_server.R`):
  - 1498-1511 pass `rpath_values$balanced_params` and drop `method` from setup.
  - 1515 calls `run_ecosim_simulation(scenario, method = input$sim_method)`.
  - The UI (366) removes `"Euler"`.
  - `rpath_values$ecosim_meta <- list(years, method, scenario_name, applied, not_applied)` feeds `ecosim_status`, replacing the live `input$sim_years` read at 1529.
  - Dead viz controls are **wired**, because `plot_ecosim_results` supports groups, type and legend. `ecosim_biomass_plot` depends on `rpath_values$plot_trigger` and reads `isolate(input$plot_type / selected_groups / show_legend)`.
  - The group selector (1554) uses `Type < 2` (living). Detritus has no dynamics worth plotting by default.
  - All three `observeEvent` error handlers (1519, 1766, 1851) call `warning(sprintf("[rpath] … failed: %s", conditionMessage(e)), call. = FALSE)` before `showNotification`. Success toasts fire only after a non-NULL result is assigned.

### E3: Edit and re-balance loop

- A new local helper in `rpath_server.R`, `invalidate_downstream()`, sets `ecopath_model`, `balanced_params`, `mti`, `ecosim`, `ecosim_meta`, `diagnostics`, `sensitivity`, `fishing_scenario` and `survey_trends` to NULL, and sets `status <- "converted"` if params exist.
- Call it:
  - in convert (replacing 685-688, before the new params are assigned),
  - after a successful group cell edit (878),
  - after a diet cell edit (975),
  - in both resets (892, 988).
- Reset groups (892): `rpath_values$params$model <- data.table::as.data.table(original_params()$model)`. Also store `original_params` with `data.table::copy()` instead of `as.data.frame` (701-704), so both resets restore the exact class. Keep `as.data.table` on read as a defensive measure.
- Gate (723): replace `req(rpath_values$status == "converted")` with `req(!is.null(rpath_values$params$model))`. Status then only drives display text.
- Error handling: the edit observers already validate their input. Invalidation is unconditional after a successful write, so a failed validation (`return()` before the write) keeps the downstream results.

## 5. Testing strategy

TDD order: write each test, watch it fail on current master (after A2), then implement. Rpath-dependent tests use `skip_if_not_installed("Rpath")`. The fixture is Rpath's bundled `Rpath::REco.params`, confirmed present in the package `data/`: 3 fleets (Trawlers, Midwater, Dredgers), 2 detritus pools, and known landings.

- `tests/testthat/test-rpath-params-schema.R` (E1)
  - Pure, no Rpath needed: `build_fleet_table` on a 4-group + 2-fleet synthetic frame maps IDs to names, turns −9999 into 0, and drops an unknown FleetID with a warning (`expect_warning`).
  - Pure: `build_detfate_table` pivots `PreyID` = detritus into the `[PredID, det]` cells, and rows sum to 1. A living group with no fate falls back to the first detritus group with a warning. The fleet row is filled from `discard_fate`.
  - Pure: a guard test on the source text of `convert_ecopath_to_rpath` confirms it no longer writes `params$model$DetFate`, `$Catch`, `$Immigration` or `$Emigration`.
  - Rpath: converting the synthetic fished model gives `ncol(model) == 10 + n.dead + 2 * n.fleet`. `check.rpath.params` raises no column-count warning. The fleet landing column equals the input. No `DummyFleet` is present.
  - Rpath: the zero-fleet model gets `DummyFleet` and `attr(, "econetool_dummy_fleet")`.
  - Rpath: a fished model balanced with `rpath()` has non-zero `Landings` for the caught groups (`rowSums(model$Landings) > 0`).
  - Windows-only integration: `skip_on_ci()`, `skip_if_not(.Platform$OS.type == "windows")`, `skip_if_not_installed("RODBC")`, and `skip_if_not_installed("Rpath")`. Importing `examples/Coastal model EE 1.ewemdb` gives `catch_data` with 41 rows. After conversion, `sum(model$Fishery, na.rm = TRUE)` equals `sum(EcopathCatch$Landing)`.
- `tests/testthat/test-rpath-ecosim.R` (E2, all Rpath-gated, with `m <- Rpath::rpath(REco.params)`)
  - `setup_ecosim_scenario(m, REco.params, years = 5)` returns class `Rsim.scenario` with 5 year rows. `run_ecosim_simulation(s, "AB")` runs and returns `annual_Biomass` with 5 rows.
  - `run_ecosim_simulation(s, "Euler")` errors (match.arg).
  - `ecosim_long()` returns columns `year, group, value` with no `Outside`. `plot_ecosim_results()` returns a ggplot for `biomass`, `catch` and `relative`.
  - A synthetic `ecosim_data` with one forcing row that maps to a REco link, plus `FtimeAdjust` and `MaxEffort`, gives a scenario with `VV` changed at that link. `applied` contains the vulnerability and `not_applied` contains `"MaxEffort: no Rpath equivalent"`. A source-text guard confirms that no `"configured"` string remains in the function.
  - `run_sensitivity_analysis(REco.params, g, "QB", 20, 5)` (with `g` = first Type-0 group with non-NA QB, chosen at runtime) returns 5 `param_values` centred on the baseline, with `baseline_value == REco.params$model[Group == g, QB]`. An NA-parameter group errors with the "estimated by Ecopath" message.
  - `evaluate_fishing_scenario(m, REco.params, "Trawlers", 0, 5)`: in the scenario's `ForcedEffort`, only the Trawlers column is 0 and Midwater stays 1. Baseline and scenario `annual_Catch` differ. An unknown fleet causes `expect_error`.
- `tests/testthat/test-rpath-server-edit-loop.R` (E3, `shiny::testServer(rpathModuleServer, args = list(ecopath_import_reactive = reactive(<REco-derived import>)))`, Rpath-gated)
  - After convert → balance → Reset groups, `data.table::is.data.table(rpath_values$params$model)` holds and a second balance succeeds.
  - After a balance, `output$group_params_table` renders (no `req` silent-stop). Use `expect_no_error(output$group_params_table)`.
  - A cell edit after the balance sets `ecopath_model`, `ecosim` and `fishing_scenario` to NULL.
- Convention checks: every precondition uses `skip_if` and never `if (...) expect_*`. `tests/testthat/test-deep-analysis-fixes.R` guards still pass. Parse-check and lint every edited file.
- **Rpath for dev and CI.** Rpath is GitHub-only and compiles Rcpp (Rtools on Windows). Dev install: `remotes::install_github("NOAA-EDAB/Rpath@55fe84d")`. CI gets a new non-blocking job `rpath-tests` in `.github/workflows/nightly-live-tests.yml`. It installs Rpath at the pinned SHA and runs the three files above. The default CI keeps working because every Rpath test skips.

## 6. Rollout

- **Prerequisite.** A2 merged. E rebases on it: A touches `rpath_balancing.R` and `rpath_conversion.R`, and E's diagnostics checks rely on A's class-`Rpath` acceptance.
- **PR E1** (schema + importers + fleet selector). This is behaviour-changing: fished models now balance with real F, so EE and balance output change. Ship as a **minor** bump (1.7.0; A owns 1.5.0 and C owns 1.6.0, see the overview spec) with a CHANGELOG note "imported EwE fisheries now reach Rpath; balanced EE values for fished groups will differ".
- **PR E2** (Ecosim, sensitivity, fishing, viz wiring). Patch bump on top of E1. It needs E1 for real fleets in the fishing tests on imported models (the REco tests do not need it).
- **PR E3** (edit loop). Independent of E1 and E2, so it can merge first as a patch.
- Deploy per the MEMORY workflow. Check that `Rpath` is installed on laguna (`Rscript -e 'packageVersion("Rpath")'`) before announcing; if it is missing, install it at the pinned SHA into the server library.

## 7. Risks & open questions

- **Rpath not installed locally or in CI.** All Rpath behaviour here was taken from source, not executed. Mitigation: pin the SHA and run the nightly job. The first implementation step installs Rpath locally and confirms the REco fixture assumptions (fleet and detritus names, a Type-0 group with entered QB, multi-stanza groups present).
- **Detritus fate pivot.** This was verified on two local EwE DBs only. EwE 5 exports or CSV imports lack `DetritusFate`, so the fallback (all to the first detritus group) applies and raises a warning.
- **Mixed-type groups (0 < Type < 1).** Rpath treats these as mixotrophs. `build_detfate_table` must use `Type < 2` for "living", not `Type %in% c(0, 1)`.
- **Vulnerability ID mapping.** The mapping `EcosimScenarioGroup.GroupID → EcopathGroupID` is inferred from the example schema. The example DB's forcing matrix is empty, so the mapping is tested only on synthetic data. Unmapped links are reported, never silently dropped.
- **Other EwE Ecosim fields** (`Pbmaxs`, `FtimeMax`, recruitment, quotas). These are deliberately reported as not applied. Mapping `FtimeMax` or `Pbmaxs` onto Rpath's `FtimeQBOpt` or `PBopt` needs domain review and is a follow-up.
- **Sensitivity cost.** Each step is one `rpath()` call, and 30 steps on a 50-group model take under a few seconds. No async work is needed.

## 8. Acceptance criteria

1. Importing `Coastal model EE 1.ewemdb` and converting it gives a real `Fishery` fleet with landings equal to `EcopathCatch` totals, detritus-fate rows that sum to 1, and no `DummyFleet`. `check.rpath.params` raises no column-count warning.
2. Run Simulation succeeds for RK4 and AB, with and without an imported scenario. The plot shows annual biomass. Every scenario field is shown as either applied or not applied, and the text "configured" does not appear.
3. Sensitivity analysis produces a plot of total living biomass against the parameter value, with a baseline marker. An estimated parameter gives an explanatory error.
4. A fishing scenario at 0× on one fleet changes only that fleet's effort, and the plot shows scenario/baseline relative biomass. Any failure shows an error toast and never "Scenario complete!".
5. The parameter editor stays visible after a balance. Reset followed by balance works. Any edit clears stale downstream results.
6. The three new test files pass locally with Rpath installed and skip cleanly without it. The existing suite has no new failures.

## Appendix: Excluded findings

None. All five batch-9 findings reproduce on current code (F47's data.frame-reset message is PARTIAL only because it was not executed). Related items found during verification but out of scope: unreachable stanza and pedigree blocks (`rpath_conversion.R:377-413`), `.survey_trends_dict` wd-relative path (`rpath_server.R:1879`), and the cannibalism, TL and diagnostics fixes owned by sub-project A.
