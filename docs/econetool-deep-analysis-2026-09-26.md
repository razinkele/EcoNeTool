# EcoNeTool — Deep Analysis Report

_Generated 2026-09-26 by a 12-area multi-agent workflow. Finders 12/12 completed. 93 raw findings, 86 unique after dedup, 84 survived two-lens adversarial verification (reachability + novelty), 2 refuted, 0 left unverified. Final severities: 5 critical, 21 high, 43 medium, 15 low. A few survivors describe the same defect from different areas (F1/F76, F35/F77, F25/F34, F29/F34, F16/F80). They are kept separate below and cross-referenced._

## Architecture delta since 2026-07-17

The overall shape is the same as in the 2026-07-17 report: one bs4Dash process, a shared `net_reactive`/`info_reactive` spine, and three ingest fronts (EwE import, Trait Research, Spatial) that all converge on the (net, info) pair. The changes since July:

- **`finalize_network()`** (R/functions/network_finalize.R) is the new single alignment helper for the #18/#27 remediation. Metaweb export and other paths now call it. It stops the crash, but it introduces two new defects (F66, F67).
- **Admin gate.** R/functions/admin_auth.R adds an opt-in bcrypt_pbkdf gate for the API-key modal (plugin_server.R). The primitives are sound. The gate is still applied only to API keys, not to other process-wide writes (F2, F19). It also fails open when `.Renviron` is lost (F4).
- **Survey-trend validation** (survey_validation.R plus an rpath_server block, with ICES DATRAS/SAG helpers in ices_lookups.R) is new. The survey-trend code itself is clean. Its helpers have a cache-completeness defect (F14) and a `<<-` defect (F15).
- **Deploy.** Commit f88095a hardened `deployment/deploy.sh` (it preserves r-libs, cache, models and dotfiles). `deploy-windows.ps1` and the data/config handling were not hardened (F4, F5, F81). The production reload now uses `touch restart.txt`.
- **Tests.** `test-deploy-preserve.R`, `test-rpath-diagnostics.R` and the layer2a live-gate fix (a5250fb) were added. CI workflows have not changed since 2026-04-11 (F82).
- **Rpath diagnostics** (the #12/#13 fix) moved into `rpath_workflows.R` with a `.require_balanced_model` guard. That guard rejects the real Rpath object (F44).
- **Retired `config/api_keys.R`** (7a96789). Deploys still ship it back (F5).

## Overall health

Plumbing-level hygiene has improved. There are no bare wd-relative `source()` calls. The `get_harm_config()` accessor, the `<<-` error closures, parameterised SQL and `on.exit` disconnects are applied consistently. The ICES and feedback-store code is careful.

The scientific core is in worse shape than the July report suggested. Five critical findings all have the same root cause: **edge and matrix orientation is inverted somewhere between ingest and metric**. Every route below produces an upside-down web or wrong trophic structure, and every one reports success:

- the CSV EwE import (F48)
- metaweb ingest (F64)
- per-hex spatial TL (F56)
- the Rpath TL "correction" (F40)
- MTI/keystoneness (F65)

None of these functions has a known-answer test (F85).

The second major result: **at least 20 of the 44 July findings recorded as "closed" in project memory are unfixed or only partially fixed in code.** These are #9, #11, #12/#13, #14 (class), #16, #17, #19, #21, #22, #23, #24, #25, #26, #27, #28, #31, #32, #33, #36 and #37, plus the deploy paths behind #1/#2. Prior #19 turns out to be a process-wide hang, not a slider revert (F1/F76).

The Rpath dynamic features (Ecosim, sensitivity, fishing scenarios) do not work end to end.

## Cross-cutting themes

### 1. Edge/matrix orientation is inverted at ingest and metric boundaries
The app's contract is prey→predator edges and prey×predator DC matrices. The following code violates that contract:
- Metaweb ingest builds predator→prey edges (F64), and so does the spatial copy of that code (F56).
- The CSV parser transposes the diet matrix (F48).
- The Rpath TL override reads DC transposed (F40).
- MTI normalises along the wrong axis and is sign-locked (F65).

The bundled metawebs disagree among themselves about column meaning (F64). F69 (non-converged TL returned as ~101) and F55 (CSV drops Detritus) are adjacent defects. F85 records that none of these estimators has a test.
**Findings:** F40, F48, F56, F64, F65, F55, F69, F85.

### 2. "Closed" prior findings that are not fixed in code
The remediation record overstates what shipped. Several fixes covered one call site of a multi-site finding (#16, #26, #33, deploy #1/#2). Several were never applied (#17, #19, #21–#25, #28, #31, #32, #36, #37). Several fixed the crash but not the underlying data error (#9, #12/#13, #27).
**Findings:** F1, F76 (#19); F33 (#26); F35, F77 (#17); F18, F36 (#16); F44 (#12/#13); F51 (#11); F57 (#9); F59 (#21); F60 (#22); F61 (#31); F67 (#27); F68 (#28); F73 (#25); F78 (#23); F79 (#24); F80, F16, F39, F53 (#32/#33); F82 (#36/#37); F4, F81 (#1/#2 deploy); F27 (#14 class).

### 3. Trait vocabulary, provenance and confidence are silently corrupted
MB and EP codes mean different things in the config, the offline writer, the live harmonizers, the UI and the food-web model (F17, F18, F36). Regex cascades misfire on common inputs (F35, F38). Unit handling is wrong by a factor of 10 or 1000 (F32, F78). The validator rejects codes the harmonizer itself emits (F71). Confidence and provenance are stamped wrong or computed before imputation (F20, F25, F26, F28, F37). Imputed values are fed back into later imputations as if observed (F29, F34). The cache ignores the per-session config (F72). The WoRMS and FishBase classification paths discard good results and keep bad ones (F9, F10, F11).
**Findings:** F9, F10, F11, F17, F18, F20, F22, F25, F26, F27, F28, F29, F32, F34, F35, F36, F37, F38, F70, F71, F72, F77, F78.

### 4. The Rpath pipeline is wired to the wrong Rpath API and schema
The DC orientation is wrong (F40). Cannibalism is deleted (F41). Fleets and detritus fate never reach Rpath's schema (F42). Ecosim, sensitivity and fishing scenarios fail on signature mismatches or ignore their inputs (F43, F45, F46). Diagnostics reject the real `Rpath` object (F44). Reset and the status gate break the edit-and-rebalance loop (F47). The #12/#13 regression test passes only because it uses a hand-built data.frame fixture.
**Findings:** F40, F41, F42, F43, F44, F45, F46, F47.

### 5. Process-global and anonymous mutation on a shared single-process server
Examples:
- One slider move hangs the R process for every session (F1/F76).
- Any visitor can rewrite the server-wide harmonization config (F2) or delete and rebuild the offline DB (F19).
- The trait cache leaks one session's thresholds into other sessions (F72).
- DATRAS partial results are cached for the life of the process (F14).
- s2 is left disabled process-wide (F60).
- The habitat cache and grid-derived state are never invalidated (F57, F58).
- Startup spawns idle future workers (F31).

**Findings:** F1, F76, F2, F14, F19, F31, F57, F58, F60, F72.

### 6. Deploy and CI do not protect production state or catch regressions
- Deploy scripts delete `data/` CSVs (F81) and `.Renviron`/`models/` (F4).
- They ship dev `config/` and wipe server-saved keys (F5).
- Backups containing secrets land in a directory-indexed site_dir (F7).
- CI runs no testthat and does not parse `trait_lookup/` (F82).
- The pre-deploy gate parses only app.R (F83).
- Live API tests leak into the offline suite (F84).
- The deploy guard tests pass on comments alone (F86).

**Findings:** F4, F5, F7, F81, F82, F83, F84, F86.

### 7. Unescaped HTML sinks and dead or misleading UI
Third-party and uploaded metadata go into `HTML()` without escaping (F3, F54). Several controls call functions that do not exist or have no handler (F8, F12, F45, F51, F75). Some controls are ignored while the modal claims otherwise (F73). A save handler erases a stored secret (F6). The data editor corrupts column types (F68), and a reordered upload CSV crashes the session (F74).
**Findings:** F3, F6, F8, F12, F45, F51, F54, F68, F73, F74, F75.

## Ranked findings

Ordering is by final severity, then by blast radius. "V:" summarises the two verifier lenses (R = reachability, N = novelty) with their adjusted severity and confidence.

| # | ID | Sev | File:line | Title |
|---|----|-----|-----------|-------|
| 1 | F40 | critical | R/functions/rpath/rpath_balancing.R:81 | TL "correction" reads DC transposed and overwrites Rpath's correct TL |
| 2 | F64 | critical | R/functions/metaweb_core.R:138 | metaweb_to_igraph builds predator→prey edges; analyses assume prey→predator |
| 3 | F48 | critical | R/functions/ecopath/ecopath_csv.R:110 | CSV/Excel EwE import reverses every trophic link |
| 4 | F56 | critical | R/functions/spatial_analysis.R:414 | Per-hexagon TL computed from predators, not prey |
| 5 | F65 | critical | R/functions/keystoneness.R:47 | MTI can never be positive; DC normalised along wrong axis |
| 6 | F1 | high | R/modules/harmonization_settings_server.R:18 | Slider move hangs R process in infinite reactive loop (prior #19) |
| 7 | F76 | high | R/modules/harmonization_settings_server.R:38 | Same as F1 (pattern-sweep view; 201 reloads reproduced) |
| 8 | F81 | high | deployment/deploy.sh:198 | Server-side deploy deletes data/ and restores it without CSVs |
| 9 | F9 | high | R/functions/taxonomic_api_utils.R:1087 | WoRMS classifications always 'low' → discarded by Ecopath import |
| 10 | F17 | high | R/functions/trait_foodweb.R:51 | MB codes mean different things in classifier, offline writer, food-web model |
| 11 | F18 | high | scripts/initialization/build_offline_trait_db.R:379 | BIOTIC Living_habit→EP sends burrowers to EP3, attached fauna to EP2 |
| 12 | F32 | high | R/functions/trait_lookup/database_lookups.R:1033 | WoRMS body size ignores Unit child; 10× error for invertebrates |
| 13 | F33 | high | R/functions/trait_lookup/harmonization.R:933 | Zero-length WoRMS class crashes harmonize_protection and the batch (prior #26) |
| 14 | F67 | high | R/functions/network_finalize.R:21 | Metaweb export drops every species attribute (#27 incomplete) |
| 15 | F66 | high | R/functions/network_finalize.R:102 | finalize_network assigns one fg to every vertex when info lacks `fg` |
| 16 | F71 | high | R/functions/trait_foodweb.R:389 | Validation rejects PR1/PR4 emitted by harmonizer; handoff fails |
| 17 | F50 | high | R/functions/ecobase_connection.R:649 | EcoBase imports lack met.types; body mass = biomass × 100 |
| 18 | F35 | high | R/functions/trait_lookup/harmonization.R:384 | 'benthopelagic' → EP1; 'subtidal' → EP4 (prior #17) |
| 19 | F77 | high | R/functions/trait_lookup/harmonization.R:393 | Prior #17 unfixed: subtidal/sublittoral → EP4 |
| 20 | F41 | high | R/functions/rpath/rpath_conversion.R:358 | Cannibalism silently deleted from diet without renormalising |
| 21 | F42 | high | R/functions/rpath/rpath_conversion.R:244 | Catches, fleets, detritus fate never reach Rpath schema |
| 22 | F44 | high | R/functions/rpath/rpath_workflows.R:319 | #12/#13 fix rejects the real Rpath object |
| 23 | F43 | high | R/modules/rpath_server.R:1510 | Ecosim cannot run on either path; scenario params ignored |
| 24 | F57 | high | R/modules/spatial_server.R:774 | Habitat cache never invalidated (#9 incomplete) |
| 25 | F49 | high | R/modules/ecopath_import_server.R:661 | Native import ignores EwE Type column; detritus gets consumer params |
| 26 | F19 | high | R/modules/trait_research_server.R:1170 | Anonymous user can delete/rebuild production offline DB; races |
| 27 | F10 | medium | R/functions/taxonomic_api_utils.R:426 | FishBase common-name first substring match labelled 'high' |
| 28 | F20 | medium | R/functions/trait_lookup/orchestrator.R:419 | Offline complete-row return drops confidences, reports 'high' |
| 29 | F25 | medium | R/functions/trait_lookup/orchestrator.R:1450 | Confidence scored before phylo imputation |
| 30 | F34 | medium | R/functions/phylogenetic_imputation.R:196 | Imputed values cached as ground truth, reused by imputation and ML training |
| 31 | F29 | medium | R/functions/trait_lookup/orchestrator.R:1678 | Cache 'harmonized' stores imputed traits without provenance (self-match) |
| 32 | F26 | medium | R/functions/trait_lookup/orchestrator.R:1301 | PR fabricated as PR0 with no data/taxonomy |
| 33 | F36 | medium | R/functions/trait_lookup/harmonization.R:715 | Live EP/PR cascades ignore config patterns (prior #16) |
| 34 | F72 | medium | R/modules/trait_research_server.R:384 | Global trait cache ignores per-session harmonization config |
| 35 | F70 | medium | R/functions/trait_foodweb.R:229 | MS1 prey capped at 0.05 floor; no threshold gives a sensible web |
| 36 | F52 | medium | R/modules/ecopath_import_server.R:137 | Same EwE file → different biomasses in food-web vs Rpath tab |
| 37 | F55 | medium | R/functions/ecopath/ecopath_csv.R:71 | CSV path deletes the 'Detritus' group |
| 38 | F62 | medium | R/functions/spatial_analysis.R:184 | NA-biomass occurrences silently dropped from hex species |
| 39 | F58 | medium | R/modules/spatial_server.R:640 | Re-created grid keeps old metrics, painted via recycled hex_ids |
| 40 | F2 | medium | R/modules/harmonization_settings_server.R:56 | Anonymous visitor can overwrite process-wide harmonization config |
| 41 | F4 | medium | deploy-windows.ps1:489 | Default deploy deletes .Renviron and models/, admin gate fails open |
| 42 | F5 | medium | deploy.sh:69 | Deploys ship dev config/, delete server api_keys.json, revive api_keys.R |
| 43 | F7 | medium | deployment/shiny-server.conf:27 | Backups with secrets placed inside a directory-indexed site_dir |
| 44 | F3 | medium | R/modules/ecobase_server.R:168 | Stored XSS from EcoBase metadata in HTML() |
| 45 | F54 | medium | R/modules/ecopath_import_server.R:1035 | Model preview renders .ewemdb metadata as raw HTML |
| 46 | F28 | medium | R/functions/trait_lookup/orchestrator.R:1105 | MS_source inferred from wrong source; BIOTIC/MAREDAT/PTDB sizes unused |
| 47 | F27 | medium | R/functions/trait_lookup/orchestrator.R:1160 | Fuzzy ontology overwrites offline-prefilled FS/MB/EP |
| 48 | F37 | medium | R/functions/uncertainty_quantification.R:159 | Boundary sizes get MS confidence 0 → overall confidence 0 |
| 49 | F38 | medium | R/functions/trait_lookup/harmonization.R:683 | All jellyfish MB1 sessile; all echinoderms PR5 soft shell |
| 50 | F78 | medium | R/functions/trait_lookup/database_lookups.R:88 | FishBase max_weight_g still × 1000 (prior #23) |
| 51 | F79 | medium | R/functions/trait_lookup/database_lookups.R:253 | AlgaeBase fallback always fails (prior #24) |
| 52 | F11 | medium | R/functions/taxonomic_api_utils.R:337 | Singularisation truncates scientific names |
| 53 | F14 | medium | R/functions/ices_lookups.R:349 | Timeout-partial DATRAS results cached for process lifetime |
| 54 | F30 | medium | R/functions/trait_lookup/orchestrator.R:1671 | Degraded results from transient failures cached 30 days |
| 55 | F68 | medium | R/modules/dataeditor_inline_server.R:105 | Data-editor edits turn numeric columns to character (prior #28) |
| 56 | F46 | medium | R/functions/rpath/rpath_simulation.R:254 | Fishing scenario ignores fleet; result unplottable |
| 57 | F45 | medium | R/modules/rpath_server.R:1760 | Sensitivity analysis always errors |
| 58 | F47 | medium | R/modules/rpath_server.R:892 | Reset stores data.frame in params$model; editor vanishes after balance |
| 59 | F12 | medium | R/functions/shark_api_utils.R:68 | SHARK wrappers call non-existent SHARK4R functions |
| 60 | F51 | medium | R/modules/ecopath_import_server.R:1598 | current_network() still undefined (prior #11) |
| 61 | F59 | medium | R/modules/spatial_server.R:1268 | Deselecting 'S' empties metrics table (prior #21) |
| 62 | F60 | medium | R/modules/spatial_server.R:790 | s2 left off process-wide on BBT load error (prior #22) |
| 63 | F61 | medium | R/modules/spatial_server.R:1084 | Species coordinates unvalidated; bad file kills session (prior #31) |
| 64 | F73 | medium | R/modules/trait_research_server.R:316 | Databases checkboxes ignored (prior #25) |
| 65 | F74 | medium | R/functions/trait_foodweb.R:347 | CSV with species not first crashes construction |
| 66 | F80 | medium | R/functions/trait_lookup/api_trait_databases.R:134 | message() in error handlers; PTDB switch default (prior #32/#33) |
| 67 | F82 | medium | .github/workflows/ci.yml:151 | CI runs no testthat; skips trait_lookup parse (prior #36/#37) |
| 68 | F83 | medium | deployment/pre-deploy-check.R:240 | Pre-deploy gate parses only app.R/run_app.R |
| 69 | F84 | medium | tests/testthat/test-layer2c-api-databases.R:14 | Live API calls in offline suite not gated |
| 70 | F53 | low | R/functions/ecopath/ecopath_windows.R:205 | ecopath_windows/unix handlers still message() (prior #33) |
| 71 | F16 | low | R/functions/trait_lookup/api_trait_databases.R:222 | API trait-source handlers use message() |
| 72 | F39 | low | R/functions/ml_trait_prediction.R:124 | ML error handlers message(); wd-relative models path |
| 73 | F63 | low | R/functions/emodnet_habitat_utils.R:708 | Habitat overlay can run only once per grid |
| 74 | F85 | low | R/functions/keystoneness.R:1 | MTI/keystoneness, TL, metaweb editing have zero tests |
| 75 | F22 | low | R/functions/trait_lookup/orchestrator.R:88 | RS/TT/ST offline round-trip dead at both ends |
| 76 | F24 | low | R/functions/trait_lookup/orchestrator.R:47 | Offline DB path wd-relative |
| 77 | F69 | low | R/functions/trophic_levels.R:76 | Non-converged TL returned (~101) with only a warning |
| 78 | F6 | low | R/modules/plugin_server.R:267 | Saving API key modal erases stored AlgaeBase password |
| 79 | F8 | low | R/modules/harmonization_settings_server.R:149 | JSON import/export call non-existent functions |
| 80 | F75 | low | R/ui/harmonization_settings_ui.R:80 | Rule checkboxes / FS pattern inputs have no handler |
| 81 | F15 | low | R/functions/ices_lookups.R:388 | fetch_sag_ssb `<<-` in tryCatch body → 'object result not found' |
| 82 | F31 | low | app.R:96 | Unused 4-worker future plan at startup; missing pkg aborts startup |
| 83 | F23 | low | scripts/populate_local_databases_to_sqlite.R:25 | Script sources a deleted file; `<-` in error closures |
| 84 | F86 | low | tests/testthat/test-deploy-preserve.R:51 | Deploy guard tests pass on comments alone |


### Detail

#### F40 [critical] TL "correction" reads the DC matrix transposed and overwrites Rpath's correct trophic levels
- **Where:** R/functions/rpath/rpath_balancing.R:81 (override at L273-281)
- **Claim:** Rpath's DC is prey (rows) × predator (columns), and the conversion builds it that way (rpath_conversion.R:263-287). `calculate_rpath_trophic_levels` treats `DC[i, j]` as "prey j in the diet of predator i", so each consumer's TL is computed from its predators. `run_ecopath_balance` replaces all of `model$TL` whenever any group differs from Rpath's value by more than 0.5, which happens in practically every chain. The wrong TL reaches the results table, the CSV export, the diagnostics and the pyramid. The "circular loop" rationale is false: Rpath solves TL linearly.
- **Failure scenario:** Toy Phyto→Zoo→Fish gives Phyto=1, Zoo=3, Fish=2. Top predators always come out at TL 2.0. A Baltic model reports cod at TL 2, and this goes to the CSV with a "Mass balance complete!" toast.
- **Fix:** Delete the override and keep Rpath's TL. If it must stay, use `TL[pred] = 1 + Σ DC[prey, pred]·TL[prey]`. Add a toy-chain test.
- **V:** R critical/high: reproduced with Rscript; reachable after every balance, provided Rpath is installed. N critical/high: novel; dates from 7aabf1d.

#### F64 [critical] metaweb_to_igraph builds predator→prey edges while every analysis assumes prey→predator
- **Where:** R/functions/metaweb_core.R:138 (same construct at spatial_analysis.R:259)
- **Claim:** The edges are `c(predator_id, prey_id)`. trophic_levels.R, visualization basal detection, fluxing and MTI all assume prey→predator. The template, the README, `add_trophic_link()` and the Kongsfjorden metaweb come out inverted. Three bundled metawebs (Baltic Kortsch, Barents, North Sea) look right only because their CSVs store the columns reversed.
- **Failure scenario:** kongsfjorden_farage2021.rds puts beluga and Trophon at TL 1 and Halicryptus at the top (TL 7.26). Every user metaweb built from the template is inverted after "Export to active network", which reports SUCCESS.
- **Fix:** Build edges as `c(prey_id, predator_id)` here and in spatial_analysis.R:259. Swap the columns in the mislabelled bundled files. Add a regression test that cod gets a higher TL than herring.
- **V:** R critical/high: reproduced; reachable via metaweb_manager_server.R:448. N critical/high: not in the July report.

#### F48 [critical] CSV/Excel EwE import reverses every trophic link
- **Where:** R/functions/ecopath/ecopath_csv.R:110
- **Claim:** The diet matrix is read with prey in rows (L86-87, UI help text), then `t(diet_matrix > 0)` makes the edges predator→prey. The native and EcoBase paths do not transpose. The in/out-degrees feeding the FG heuristics are reversed too.
- **Failure scenario:** A 4-group CSV gives Phyto TL 3.5 and Cod TL 1.0, and the import shows "✓ SUCCESS".
- **Fix:** Drop the transpose. Add a test that a producer gets TL 1 after `parse_ecopath_data()`.
- **V:** R critical/high: reachable via ecopath_import_server.R:839. N critical/high: #6 fixed only the row mask, and no test checks edge direction.

#### F56 [critical] Per-hexagon trophic levels are computed from predators, not prey
- **Where:** R/functions/spatial_analysis.R:414
- **Claim:** `extract_local_network` builds predator→prey edges, and `neighbors(mode="in")` then returns predators. TL becomes 1 + the mean TL of the node's consumers.
- **Failure scenario:** Phyto eaten by Zoo1 and Zoo2 gives meanTL 1.333 (true value 1.667). On a linear chain the result looks plausible, which hides the bug. meanTL/maxTL are wrong in the table, the choropleth and the CSV/RDS exports.
- **Fix:** Use `mode = "out"`, or reuse trophic_levels.R once edge direction is unified (F64). Add a test on an asymmetric web.
- **V:** R high/high; N critical/high (silent wrong science in exports). Final: critical.

#### F65 [critical] calculate_mti can never produce a positive impact; DC normalised along the wrong axis
- **Where:** R/functions/keystoneness.R:47 (normalisation L16-24, KS L127)
- **Claim:** MTI = −(I−DC)⁻¹DC with DC ≥ 0, so every entry is ≤ 0. Rows are prey, not predators as the comment says, so the row normalisation divides by predator count rather than diet share. The KS formula is not Libralato's.
- **Failure scenario:** Phyto→Zoo→Fish plus Phyto→Zoo2: Phyto has zero impact on everything and Fish is ranked "Keystone". Producers never register as influential.
- **Fix:** Use a predator-by-prey DC, Q = DC − t(FC), MTI = solve(I−Q) − I, and KS = log(ε_i(1−p_i)). Add a test that a producer has a positive MTI on its consumer.
- **V:** R high/high (sign and axis confirmed; exact ranks not reproduced); N critical/high. Final: critical.

#### F1 [high] Harmonization slider move hangs the R process in an infinite reactive loop (prior #19 never fixed)
- **Where:** R/modules/harmonization_settings_server.R:18
- **Claim:** The INITIALIZE `observe()` reloads the JSON and reads `rv$config` without `isolate()`. The UPDATE observer's `rv$config$…$X <- input$…` also reads `rv$config`. Once a saved config exists, the two observers invalidate each other forever.
- **Failure scenario:** Reproduced with testServer: the call never returns and exits 124 at the timeout. In production, one visitor moving a threshold pins the single R process and freezes every session. config/harmonization_custom.json ships with deploy-windows.ps1 and is created by any Save.
- **Fix:** Load the file once at session start outside any reactive, and use `isolate(rv$config)` in the update observer.
- **V:** R critical/high (reproduced; production file existence could not be read); N high/high (DoS, not data loss). Duplicate of F76.

#### F76 [high] Prior #19 not fixed and worse than reported: endless observer loop
- **Where:** R/modules/harmonization_settings_server.R:38
- **Claim:** Same mechanism as F1, seen from the UPDATE observer. The flush loop never returns and the shared R process stops serving.
- **Failure scenario:** testServer with a reload counter: after `setInputs(harm_thresh_MS3_MS4 = 9L)`, `load_harmonization_config` ran 201 times before hitting the cap. The uncapped run timed out at 120 s.
- **Fix:** As F1.
- **V:** R critical/high; N high/high (not in any recorded remediation batch). Fix together with F1.

#### F81 [high] Server-side deploy deletes data/ and restores it with every CSV excluded
- **Where:** deployment/deploy.sh:198
- **Claim:** PRESERVE_ITEMS contains only r-libs, cache and restart.txt, so `find` deletes data/ and config/. The re-copy rsyncs data/ with `--exclude='*.csv' --exclude='*.zip'`, so the tracked biotic/maredat/ptdb/test CSVs never come back. Server-only data/EUSeaMap_2025 has no source to restore from. The "match root deploy.sh" comment is false. The f88095a fix is incomplete.
- **Failure scenario:** BIOTIC, MAREDAT and PTDB silently drop out of trait lookups, and habitat enrichment loses EUSeaMap. The script prints success.
- **Fix:** Preserve data and config, or copy over the tree (`cp -rT`). Drop the `*.csv` exclude.
- **V:** R high/high; N high/high. Downgraded from critical because this is not the documented deploy path and a tar backup is taken first.

#### F9 [high] WoRMS classifications always have confidence 'low', so the Ecopath import discards them
- **Where:** R/functions/taxonomic_api_utils.R:1087
- **Claim:** `confidence` is initialised to "low" (L1005), and the WoRMS branch sets "medium" only when it `is.null`, which never happens. Both consumers accept only high or medium (ecopath_import_server.R:599, :1230). The progress modal still shows the WoRMS classification.
- **Failure scenario:** 'Acartia spp.' shows "Zooplankton" in the modal but is stored as the pattern default 'Fish'.
- **Fix:** Set "medium" unconditionally before the override checks. Map it through `confidence_to_num/label`.
- **V:** R high/high; N high/high. Novel. Plugin-gated path.

#### F17 [high] MB mobility codes mean different things in the classifier, the offline DB writer and the food-web model
- **Where:** R/functions/trait_foodweb.R:51 (also harmonization_config.R:66-68, build_offline_trait_db.R:592, local_trait_databases.R:511, trait_research_ui.R:503-505, harmonization.R:630/686)
- **Claim:** The config uses MB2 = burrower and MB4 = drifter. The model (MB_MB, TRAIT_DEFINITIONS) uses MB2 = passive floater and MB4 = facultative swimmer. The offline writer mixes the two, and the UI has a fourth mapping. There is no `mobility_labels` list.
- **Failure scenario:** Abra alba (a burrower) is stored as MB2 and priced as a passive floater. The same diatom is MB2 offline and MB4 live, so link probabilities depend on cache state.
- **Fix:** Pick one vocabulary (the model's), define `mobility_labels` in the config, re-key all producers and the UI, and rebuild the DB. Add a test that the config labels match `names(TRAIT_DEFINITIONS$MB)`.
- **V:** R high/high; N high/high. Novel; reaches link probabilities via foodweb_construction_server.R:372/381.

#### F18 [high] BIOTIC Living_habit→EP mapping sends burrowers to EP3 and attached/tube fauna to EP2
- **Where:** scripts/initialization/build_offline_trait_db.R:379
- **Claim:** The mapping uses inline regexes, not the config: burrow→EP3 and tube|attached|free→EP2 (benthopelagic). Prior #16 was incomplete. Only the SpeciesEnriched block uses the config, and the live `harmonize_environmental_position` still hard-codes its EP regexes.
- **Failure scenario:** The live DB has 453/679 BIOTIC rows at EP2 (170 of them sessile) and 0 at EP4. Infauna are priced with the benthopelagic EP_MS row.
- **Fix:** Map through the config's `environmental_patterns` (burrow/infauna→EP4, tube/attached→EP3) and rebuild the DB.
- **V:** R high/high (DB query confirmed); N high/high (incomplete fix of #16).

#### F32 [high] WoRMS body size ignores the 'Unit' child measurement; 10× error for invertebrates
- **Where:** R/functions/trait_lookup/database_lookups.R:1033
- **Claim:** `children` is never read. When there is no qualitative size row, every non-vertebrate class is assumed to report in mm and the value is divided by 10. The orchestrator seeds size_cm from WoRMS first (orchestrator.R:377-379), and MS_source then credits SeaLifeBase.
- **Failure scenario:** Mytilus edulis: WoRMS gives 20 cm, the code produces 2.0 cm, and the size class is MS3 instead of MS5. Confirmed live.
- **Fix:** Read the unit from each row's children and convert per row. Keep the class heuristic only as a warned last resort.
- **V:** R high/high (live wm_attr_data confirmed); N high/high.

#### F33 [high] Zero-length WoRMS class crashes harmonize_protection and aborts the whole batch (prior #26 never fixed)
- **Where:** R/functions/trait_lookup/harmonization.R:933 (also :653, :764; database_lookups.R:959/1032)
- **Claim:** `class` is `character(0)` when a taxon has no Class rank, so `!is.null(x) && grepl(...)` inside `if` errors. harmonize_protection is called unconditionally with no tryCatch, and the trait_research_server.R:361 tryCatch wraps the entire batch loop.
- **Failure scenario:** `harmonize_protection(NULL, list(phylum="Nematoda", class=character(0)))` gives "missing value where TRUE/FALSE needed". One such taxon empties the whole Trait Research batch.
- **Fix:** Normalise taxonomy fields to length 1 or NULL in `lookup_worms_traits`, and use `isTRUE()` in the guards.
- **V:** R high/high (the mobility sub-claim is overstated: it returned MB4); N high/high (#26 incomplete).

#### F67 [high] Metaweb export drops every species attribute (#27 fix incomplete)
- **Where:** R/functions/network_finalize.R:21
- **Claim:** Vertices are named by `species_id` ("SP001"), but `.SPECIES_KEY_CANDIDATES` has no species_id. The code picks species_name, gets an all-NA match, and replaces every attribute with defaults. The metaweb columns functional_group/biomass/body_mass/metabolic_type/efficiency are also never mapped to fg/meanB/bodymasses/met.types/efficiencies.
- **Failure scenario:** Baltic export: species = SP001…, meanB = 1 for every species, the real biomass is lost, and fg = Fish throughout. The status says SUCCESS.
- **Fix:** Name vertices by species_name (or add species_id as a key and relabel). Map the metaweb columns before calling `finalize_network`.
- **V:** R high/high (reproduced); N high/high.

#### F66 [high] finalize_network gives every vertex the first species' functional group when info has no fg column
- **Where:** R/functions/network_finalize.R:102
- **Claim:** `fg_char` is a length-1 NA, so `fg_char[needs_fg] <- inferred` keeps only the first value, which is then recycled. bodymasses, met.types and efficiencies all inherit the uniform fg.
- **Failure scenario:** All 34 Baltic species become "Fish" (reproduced). The same happens for any uploaded RData whose info lacks fg.
- **Fix:** Initialise with `rep(NA_character_, length(vertex_names))`. Add a test.
- **V:** R high/high; N high/high. Distinct from F67.

#### F71 [high] Validation rejects PR1/PR4, which the harmonizer emits, so the Trait Research → Food Web handoff fails
- **Where:** R/functions/trait_foodweb.R:389
- **Claim:** `valid_PR` lists PR0, 2, 3, 5, 6, 7, 8. harmonize_protection emits PR1 (L835) and PR4 (L853/L904), and PR_MS itself supports PR4. Construction aborts when validation fails.
- **Failure scenario:** Any dataset containing copepods or crustaceans gets "Please fix validation errors" (reproduced).
- **Fix:** Derive the valid sets from TRAIT_DEFINITIONS or the config labels. Add PR1 to PR_MS.
- **V:** R high/high; N high/high.

#### F50 [high] EcoBase imports have no met.types (Energy Fluxes always fails); body mass = biomass × 100
- **Where:** R/functions/ecobase_connection.R:649 (and ~463)
- **Claim:** Both converters omit met.types and set `bodymasses = biomass_values * 100`. ecobase_server.R:249 adds only colfg and never calls `finalize_network()`.
- **Failure scenario:** Model 403 gives "missing required columns: 'met.types'" in fluxweb, and a basking shark body mass of 3.4 g.
- **Fix:** Route through `finalize_network()` or the `estimate_*_by_fg` helpers. Remove the biomass proxy.
- **V:** R high/high; N high/high.

#### F35 [high] Fuzzy EP cascade codes 'benthopelagic' as EP1; prior #17 still reproduces
- **Where:** R/functions/trait_lookup/harmonization.R:384
- **Claim:** The EP1 'pelagic' test comes before the EP2 'benthopel' test. The EP3 negative guard contains an unanchored 'tidal'.
- **Failure scenario:** `harmonize_fuzzy_habitat(... "benthopelagic")` gives EP1, and "subtidal" gives EP4. data/ontology_traits.csv has 43 benthopelagic rows.
- **Fix:** Test EP2 first and use word boundaries. Add regression tests.
- **V:** R medium/high (ontology fallback only); N high/high. Overlaps F77.

#### F77 [high] Prior #17 not fixed: 'subtidal'/'sublittoral' still coded as infaunal EP4
- **Where:** R/functions/trait_lookup/harmonization.R:393
- **Claim:** The negative guard `!grepl("intertidal|tidal|littoral|…")` matches substrings of subtidal and sublittoral, so control falls through to EP4.
- **Failure scenario:** A subtidal epibenthic species is coded as buried infauna.
- **Fix:** Use `\\b` boundaries. Add tests for "subtidal" and "sublittoral".
- **V:** R high/high; N high/high. Fix together with F35.

#### F41 [high] Cannibalism is silently deleted from the diet matrix without renormalisation
- **Where:** R/functions/rpath/rpath_conversion.R:358
- **Claim:** Every self-diet cell is set to 0 and the column is not rescaled. The user is told only via `message()`. The "circular TL" rationale does not apply to Rpath.
- **Failure scenario:** 10% cod-on-cod leaves the diet column summing to 0.9, so EE and B differ from the published EwE balance.
- **Fix:** Keep cannibalism as entered. Rpath handles it.
- **V:** R high/high; N high/high.

#### F42 [high] Catches, fleets and detritus fate never reach Rpath's parameter schema (plausible)
- **Where:** R/functions/rpath/rpath_conversion.R:244
- **Claim:** The conversion writes ad-hoc Catch/Immigration/Emigration/DetFate columns. Nothing writes the per-fleet landing/`.disc` columns or the per-detritus fate columns. The importer never reads the catch table, and 'DummyFleet' is always added.
- **Failure scenario:** A fished model balances with F = 0. The fishing scenario lists only DummyFleet, and every multiplier leaves the result unchanged.
- **Fix:** Read the catch and detritus-fate tables, create real fleets via `create.rpath.params`, and fill the proper columns.
- **V:** R high/medium; N high/medium. Rpath is not installed locally, so the schema is taken from documentation.

#### F44 [high] #12/#13 fix is incomplete: diagnostics and the trophic pyramid reject the real Rpath object
- **Where:** R/functions/rpath/rpath_workflows.R:319
- **Claim:** `Rpath::rpath()` returns a list of class 'Rpath', and `.require_balanced_model` requires `is.data.frame`. The test passes only on a hand-built data.frame fixture. `type < 3` also counts detritus as living.
- **Failure scenario:** After a successful balance, Diagnostics and the Pyramid always report "unavailable".
- **Fix:** Accept `inherits(model, "Rpath")`, build a data.frame from its vectors, and use `type < 2` for living groups. Add a fixture with class 'Rpath'.
- **V:** R high/high; N high/high (incomplete fix, 0c59474).

#### F43 [high] Ecosim cannot run on either path; imported scenario parameters are ignored but reported as configured
- **Where:** R/modules/rpath_server.R:1510 (rpath_simulation.R:64-67, 86-88, 128, 417, 451-464)
- **Claim:** The default path passes `method=`, which is not in the signature, so it fails with "unused argument". The scenario path calls `rsim.scenario(Rpath.params=, Rpath.sim=NULL)`, which is the wrong signature. `years` is a scalar, RK4 is hard-coded, and the plot melts on a nonexistent 'time' column. The vulnerability, group and fleet loops apply nothing.
- **Failure scenario:** Every Run Simulation click gives "Ecosim error: unused argument" (reproduced).
- **Fix:** Forward method, call `rsim.scenario(model, params, years = 1:years)`, apply the scenario vulnerabilities or remove the messages, and fix the plot.
- **V:** R high/high; N high/high (visible failure, so not critical).

#### F57 [high] #9 fix incomplete: the habitat cache is never invalidated
- **Where:** R/modules/spatial_server.R:774 (default bbox at 814-817)
- **Claim:** `euseamap_data()` is loaded only when NULL and is never reset. With no study area it caches a fixed 1×1° box, c(20,55,21,56).
- **Failure scenario:** Enable habitat first, then pick Bay_of_Gdansk: every cell comes out "No data" with diversity 0 and a success toast. Switching study area keeps the stale extent.
- **Fix:** Store the loaded bbox and reload when it no longer covers the study area. NULL the caches on study-area change. Remove the silent default.
- **V:** R high/high; N high/high (new angle on #9).

#### F49 [high] Native import ignores EwE's Type column, so detritus pools get consumer groups and parameters
- **Where:** R/modules/ecopath_import_server.R:661
- **Claim:** `$Type` is never read. The detritus regex misses abbreviations such as 'Detritu' and 'Rdetrit', which fall through to the topology heuristics.
- **Failure scenario:** MarMenor: 'Rdetrit' (the largest-biomass pool) is classed as Fish (ectotherm vertebrates, efficiency 0.85), and 84/86 consumers come out as Benthos.
- **Fix:** Use Type first (1→producer, 2→Detritus, 3→drop) and classify only Type 0 groups.
- **V:** R medium/medium (only abbreviated names are affected; MarMenor numbers not re-run); N high/medium.

#### F19 [high] Any anonymous user can delete and rebuild the production offline DB; concurrent rebuilds race
- **Where:** R/modules/trait_research_server.R:1170
- **Claim:** There is no `admin_authorized()` gate, and the in-progress guard is a per-session reactiveVal. The build script calls `file.remove(db_path)` and then may `stop()` on ontology drift, leaving an empty DB.
- **Failure scenario:** Two visitors rebuild at the same time and race on the same path. During the build every session falls back to slow API lookups. On drift, the DB stays empty.
- **Fix:** Add the admin gate and a process-wide lock file, build to `.tmp`, and `file.rename` on success.
- **V:** R high/medium (race not reproduced); N high/high.

#### F10 [medium] FishBase common-name lookup takes the first substring match and labels it 'high'
- **Where:** R/functions/taxonomic_api_utils.R:426 (confidence L1033, cache key L980)
- **Claim:** rfishbase 5.0.3 `common_to_sci` does substring grep. With no region, or no region match, the code takes `Species[1]` and marks it high confidence. The cache key omits the region (30-day TTL).
- **Failure scenario:** 'Cod' may resolve to Arctic cod or a cod icefish, whose weight and TL are imported as "high". A later import from a different region gets the same cached pick.
- **Fix:** Prefer exact matches, downgrade confidence and warn when there are multiple candidates, and put the region in the cache key.
- **V:** R medium/high; N medium/medium (a progress warning is emitted; plugin-gated).

#### F20 [medium] Offline complete-row early return drops the stored per-trait confidences and reports 'high'
- **Where:** R/functions/trait_lookup/orchestrator.R:419
- **Claim:** `*_confidence` columns are SELECTed but never copied, and `confidence` is hard-coded to "high". The prefill path ignores them too.
- **Failure scenario:** 981 complete rows (470 PTDB, 367 BIOTIC, 144 MAREDAT) are cached as "high" with NA per-trait confidences, including BVOL defaults stored at 0.2-0.4.
- **Fix:** Copy `offline$<T>_confidence` and derive the label with `confidence_to_label()` on the geometric mean.
- **V:** R medium/high; N medium/high (metadata only; codes are correct).

#### F25 [medium] Confidence is scored before phylogenetic imputation
- **Where:** R/functions/trait_lookup/orchestrator.R:1450 (phylo at 1581)
- **Claim:** Phylo-filled traits keep NA confidence, and overall confidence ignores them. `imputation_method` stays "observed". The inline 0.7/0.5 bands disagree with `confidence_to_label`.
- **Failure scenario:** A fish with an imputed EP shows "high" confidence and "observed".
- **Fix:** Move scoring after phylo imputation, score phylo traits, set `imputation_method`, and use `confidence_to_label()`.
- **V:** R medium/high; N medium/high. Overlaps F34.

#### F34 [medium] ML- and phylo-imputed values are cached as ground truth, then reused by later imputations and ML training
- **Where:** R/functions/phylogenetic_imputation.R:196 (orchestrator.R:1680-1687; scripts/train_trait_models.R:91-95)
- **Claim:** `harmonized` stores the final codes with no provenance. `find_closest_relatives` reads every cache file, including the target's own file (distance 0, weight 5). Training uses these codes as labels. Phylo runs after UQ.
- **Failure scenario:** An ML guess of EP1 for species A spreads to its congeners as "Phylogenetic", and retraining then learns the guess as a label.
- **Fix:** Store `*_source` or an observed flag, filter on it in phylo and training, skip the target's own file, and score phylo traits.
- **V:** R medium/medium; N medium/medium. Overlaps F29 and F25.

#### F29 [medium] Cache 'harmonized' block stores imputed traits without provenance and returns them as phylo evidence (self-match)
- **Where:** R/functions/trait_lookup/orchestrator.R:1678
- **Claim:** Same mechanism as F34, seen from the writer side.
- **Failure scenario:** After the 30-day TTL, a species' stale own file votes for its own imputed EP2 at distance 0.
- **Fix:** Write only observed values, or add a source field and have relatives skip non-observed values and the target itself.
- **V:** R medium/high; N medium/medium. Fix together with F34.

#### F26 [medium] PR fabricated as PR0 "Unprotected" when there is no protection data and no taxonomy
- **Where:** R/functions/trait_lookup/orchestrator.R:1301
- **Claim:** `harmonize_protection` ends with an unconditional `return("PR0")`, even when worms is NULL. PR is then never NA, is excluded from ML and phylo fill, and is labelled "Taxonomy".
- **Failure scenario:** After a WoRMS timeout, Mytilus edulis is cached as PR0 (unprotected).
- **Fix:** Call it only when there is protection info or taxonomy, otherwise leave NA. Label rule defaults "Rule-based".
- **V:** R medium/high; N medium/high ("blocks ML/phylo" applies only when WoRMS succeeds).

#### F36 [medium] Live EP/PR cascades still ignore config patterns (prior #16 unfixed) and misclassify common inputs
- **Where:** R/functions/trait_lookup/harmonization.R:715
- **Claim:** The regexes are hard-coded. 'surface' swallows 'benthic surface' and 'Subsurface'. PR0's 'soft' fires before PR5's 'soft.*shell'. The depth rule precedes the taxonomic pelagic rules.
- **Failure scenario:** "benthic surface" gives EP1, "soft shell" gives PR0, and a copepod at 0-20 m gives EP3 (all reproduced).
- **Fix:** Route through `get_config_pattern(..., 'environmental'/'protection')` and apply the taxonomic pelagic rules before depth.
- **V:** R medium/high; N medium/high (incomplete fix of #16).

#### F72 [medium] Process-global trait cache ignores the per-session harmonization config
- **Where:** R/modules/trait_research_server.R:384 (orchestrator.R:243-246)
- **Claim:** Harmonized codes are cached per species only, for 30 days, and shared across sessions.
- **Failure scenario:** Session A's MS thresholds leak into session B, and slider changes have no effect on species already cached.
- **Fix:** Cache raw source data and re-harmonize on read, or key the cache on a config hash.
- **V:** R high/high; N medium/medium (matters only with non-default sliders).

#### F70 [medium] MS1 prey capped at the 0.05 floor; no threshold gives a sensible web
- **Where:** R/functions/trait_foodweb.R:229 (>= at L315/L338)
- **Claim:** EP_MS has no MS1/MS6 column, so the code hard-codes 0.05 and takes the min. Links to producers therefore sit at the implausibility floor. The default threshold of 0.05 with `>=` keeps implausible links, anything above it drops all producer links, and 0 makes every pair an edge, including self-loops.
- **Failure scenario:** In the 'simple' example, Benthic_filter_feeder eats Predatory_fish at p = 0.05. At a threshold of 0.06 phytoplankton has no consumers.
- **Fix:** Add proper MS1/MS6 columns, use a strict `>`, and always zero the diagonal and excluded pairs.
- **V:** R high/high; N medium/medium.

#### F52 [medium] Same EwE file gives different biomasses in the food-web tabs and the Rpath tab
- **Where:** R/modules/ecopath_import_server.R:137
- **Claim:** The network import multiplies Biomass by Area. `convert_ecopath_to_rpath` (rpath_conversion.R:204) and EcoBase do not.
- **Failure scenario:** MarMenor 'Detritu': 0.75 vs 13.22. LTCoastal Polychaetes: 0.485 vs 4.85.
- **Fix:** Check EwE's semantics and apply one shared conversion helper in both paths.
- **V:** R medium/medium; N medium/medium.

#### F55 [medium] CSV path deletes the 'Detritus' group
- **Where:** R/functions/ecopath/ecopath_csv.R:71
- **Claim:** `^detritus$` is in the summary-row filter, so every detritivory link is lost. The native path keeps detritus.
- **Failure scenario:** Deposit feeders become basal, and connectance differs from the .ewemdb import of the same model.
- **Fix:** Remove `^detritus$` from the filter.
- **V:** R low/high; N medium/medium.

#### F62 [medium] Occurrences with missing biomass are silently dropped from their hexagon's species list
- **Where:** R/functions/spatial_analysis.R:184
- **Claim:** The formula `aggregate()` defaults to `na.action = na.omit`.
- **Failure scenario:** A cell with rows (x, 1) and (y, NA) keeps only x (reproduced). Richness is roughly halved for mixed files.
- **Fix:** Compute presence separately, use `na.pass` with `sum(na.rm=TRUE)`, and warn.
- **V:** R medium/high; N medium/high.

#### F58 [medium] Re-creating the grid keeps the old networks and metrics, painted onto new cells via recycled hex_ids
- **Where:** R/modules/spatial_server.R:640 (merge at 1312)
- **Claim:** `create_grid` never resets local networks, metrics or grid_with_habitat. hex_ids restart at HEX_0001 for every grid.
- **Failure scenario:** Changing the cell size shows the old richness on unrelated hexes. The RDS export mixes the new grid with the old networks.
- **Fix:** NULL the downstream reactiveVals in the create-grid and study-area handlers.
- **V:** R medium/high; N medium/high.

#### F2 [medium] Any anonymous visitor can overwrite the process-wide harmonization config
- **Where:** R/modules/harmonization_settings_server.R:56
- **Claim:** `harm_save_config` writes the JSON with no admin gate and no validation. Every new session loads it.
- **Failure scenario:** User A's thresholds, or arbitrary values sent via `setInputValue`, silently become every user's defaults.
- **Fix:** Gate save and reset with `admin_authorized()`, validate that thresholds are finite and monotonic, and relabel the button.
- **V:** R medium/medium (currently masked by F1 when the file exists); N medium/medium.

#### F4 [medium] Default (non -NoSudo) deploy deletes .Renviron and models/, silently switching the admin gate off
- **Where:** deploy-windows.ps1:489
- **Claim:** `$preserve` keeps only data, cache and r-libs, and `find -mindepth 1` also removes dotfiles. models/ is not in DEPLOY_ITEMS. This is the incomplete deploy-windows part of the #1/#2 fix.
- **Failure scenario:** ECONETOOL_ADMIN_PASSWORD_HASH is unset and the gate fails open. FEEDBACK_ADMIN_KEY is gone.
- **Fix:** Add `! -name '.*' ! -name models`. Consider making the gate fail closed in production.
- **V:** R low/medium (needs NOPASSWD sudo; the documented workflow is -NoSudo); N medium/high (the feedback-loss part is overstated).

#### F5 [medium] Deploys ship the developer's local config/, delete server-saved API keys, and revive the retired api_keys.R
- **Where:** deploy.sh:69 (also deploy-windows.ps1 DEPLOY_ITEMS, deployment/deploy.sh)
- **Claim:** config/ is shipped with no excludes for api_keys.json, api_keys.R or harmonization_custom.json. `rsync --delete` would remove the server's api_keys.json.
- **Failure scenario:** The actual workflow (ps1 + `cp -rT`) puts api_keys.R back on laguna, which triggers the deprecation warning again and overwrites the server's harmonization JSON. Key deletion needs the rsync path, which is unavailable on the server.
- **Fix:** Exclude those three files in all three scripts and ship only the template.
- **V:** R medium/medium; N medium/medium (partly overstated).

#### F7 [medium] Full-app backups containing .Renviron and api_keys.json are placed inside a directory-indexed site_dir
- **Where:** deployment/shiny-server.conf:27 (deploy.sh:39, deployment/deploy.sh:175, deploy-windows.ps1:50)
- **Claim:** Backups go to /srv/shiny-server/backups inside `location /` with `directory_index on`. Old `cp -r` backups also run as live apps.
- **Failure scenario:** If `/` or :3838 is reachable, anyone can download FEEDBACK_ADMIN_KEY and the AlgaeBase credentials. The nginx config is not in the repo, so this could not be checked.
- **Fix:** Move BACKUP_DIR outside site_dir, chmod 600 the archives, and set `directory_index off`.
- **V:** R medium/medium; N medium/medium (the -NoSudo path writes backups to /home and is safe).

#### F3 [medium] Stored XSS: EcoBase model metadata interpolated unescaped into HTML()
- **Where:** R/modules/ecobase_server.R:168 (also :72, :157, :173-192; ecopath_import_server.R:966/1055)
- **Claim:** `fmt()` is only `as.character()`. description, author, doi (inside href), institution and model_name go in raw.
- **Failure scenario:** A contributed EcoBase model with an `onerror` payload runs script for every user who selects it.
- **Fix:** Use `tags$…` or `htmlEscape()`, and validate the DOI.
- **V:** R medium/medium; N medium/medium (requires control of an upstream record).

#### F54 [medium] Model preview renders .ewemdb metadata and the parser error as raw HTML
- **Where:** R/modules/ecopath_import_server.R:1035 (also 968, 1037, 1047, 1056, 1069)
- **Claim:** PublicationURI (inside href), Description, Author, filename and the error text are unescaped. The preview runs on file select.
- **Failure scenario:** A shared .ewemdb with a Description payload runs script before Import is clicked. A `javascript:` URI runs on click.
- **Fix:** Use `tags$*` or `htmlEscape()`, and allow only http(s) hrefs.
- **V:** R medium/high (Description is also filled on Linux); N medium/high.

#### F28 [medium] MS_source is inferred from whichever raw source has a size field; BIOTIC/MAREDAT/PTDB sizes are never used
- **Where:** R/functions/trait_lookup/orchestrator.R:1105
- **Claim:** The ladder checks field presence rather than which source actually set `size_cm`. BIOTIC, MAREDAT and PTDB `max_length_cm` never feed `size_cm`. FS_source takes the last entry of `sources_used`.
- **Failure scenario:** A WoRMS size is labelled SeaLifeBase (weight 0.95 instead of 0.60). A MAREDAT ESD is ignored.
- **Fix:** Record the source at assignment time and wire in the biotic, maredat and ptdb sizes.
- **V:** R medium/high (scenario (c) is wrong: the PTDB branch is dead because the field is cell_volume_um3); N medium/medium.

#### F27 [medium] Fuzzy ontology results overwrite offline-prefilled FS/MB/EP
- **Where:** R/functions/trait_lookup/orchestrator.R:1160 (also 1210, 1257)
- **Claim:** The fuzzy branches assign the value before checking `offline_prefilled`. This is the #14 bug class, not carried over to these branches.
- **Failure scenario:** A curated offline FS5 is replaced by the ontology fuzzy class.
- **Fix:** Wrap each branch in `if (!"X" %in% offline_prefilled)`.
- **V:** R low/high (narrow trigger); N medium/high.

#### F37 [medium] Sizes exactly on a class boundary get MS confidence 0, which forces overall confidence to 0
- **Where:** R/functions/uncertainty_quantification.R:159
- **Claim:** A size on a boundary has distance 0, so the factor is 0 and the geometric mean is 0. Latent: raw rather than profile-adjusted sizes can produce a negative or NaN value, and `if (overall >= 0.7)` then errors (only reachable via a hand-edited config).
- **Failure scenario:** FishBase lengths of 5, 20, 50 or 150 cm give "low" overall confidence.
- **Fix:** Floor and clamp the factor, compute it on the adjusted size, and guard with `isTRUE()`.
- **V:** R medium/high; N medium/high.

#### F38 [medium] Default rules code all jellyfish as sessile (MB1) and all echinoderms as 'soft shell' (PR5)
- **Where:** R/functions/trait_lookup/harmonization.R:683 (also 910-913)
- **Claim:** `cnidarians_sessile` and `echinoderms_calcium_plates` are on by default. The fallback returns MB2 (burrower) under a "passive floaters" comment. PR5 is labelled "Soft shell", while the config puts sea urchins in PR7.
- **Failure scenario:** Aurelia gets MB1, and sea urchins are coded as soft-shelled prey.
- **Fix:** Branch on class (medusae MB3/MB4, Anthozoa MB1) and map echinoderms to PR7/PR6.
- **V:** R medium/high; N medium/medium.

#### F78 [medium] Prior #23 not fixed: FishBase max_weight_g still multiplied by 1000
- **Where:** R/functions/trait_lookup/database_lookups.R:88
- **Claim:** rfishbase Weight is already in grams. Other paths in the codebase treat it as grams.
- **Failure scenario:** Cod gets 9.6e7 g in the cached raw traits and the trait panels.
- **Fix:** Drop the `* 1000`.
- **V:** R medium/high; N medium/high (no harmonization consumer found).

#### F79 [medium] Prior #24 not fixed: AlgaeBase fallback indexes a data.frame column as a record
- **Where:** R/functions/trait_lookup/database_lookups.R:253
- **Claim:** `worms_data[[1]]$phylum` on a tibble errors. The `length()` guard counts columns, not rows.
- **Failure scenario:** Every phytoplankton lookup silently gets no AlgaeBase data.
- **Fix:** Use `worms_data[1, ]` and `nrow()`.
- **V:** R high/high; N medium/high (silent coverage loss).

#### F11 [medium] Plural-to-singular step truncates scientific names, so FishBase misses common species
- **Where:** R/functions/taxonomic_api_utils.R:337
- **Claim:** The singularisation runs on binomials too. 'Pollachius virens' becomes '…viren' and 'Ammodytes' becomes 'Ammodyte'.
- **Failure scenario:** No FishBase traits for these groups, and with F9 the WoRMS result is discarded as well.
- **Fix:** Skip Latin-looking names, or try the original string first.
- **V:** R medium/medium; N medium/high.

#### F14 [medium] Timeout-partial DATRAS results cached as success for the process lifetime; full timeouts shown as "no data"
- **Where:** R/functions/ices_lookups.R:349
- **Claim:** `with_timeout(on_timeout = NULL)` is silent. Partial frames are stored in the process-wide `.ices_cache`, which has no eviction. When everything times out, the result is treated as legitimate absence.
- **Failure scenario:** 3 of 10 years time out, the survey-trend verdict is fitted on 7 years, and the partial series is reused for all users.
- **Fix:** Track failures, cache only complete results, flag `incomplete`, and return a distinct timeout error.
- **V:** R medium/medium (only timeouts are fully silent; other errors warn); N medium/medium.

#### F30 [medium] Results degraded by transient API failures are cached for 30 days
- **Where:** R/functions/trait_lookup/orchestrator.R:1671
- **Claim:** The cache write is unconditional, with no degraded marker.
- **Failure scenario:** A 5-minute WoRMS/FishBase outage produces 40 degraded species rows that are served for 30 days.
- **Fix:** Skip the write or use a short TTL when `!worms_ok` or a source errored, or store `degraded = TRUE`.
- **V:** R low/medium; N medium/medium (amplifies F26).

#### F68 [medium] Prior #28 not fixed: data-editor cell edits turn numeric columns into character
- **Where:** R/modules/dataeditor_inline_server.R:105
- **Claim:** The DT string is assigned uncoerced. Save checks only column names.
- **Failure scenario:** Editing one meanB cell makes Flux and Keystoneness error with generic messages.
- **Fix:** Use `DT::coerceValue()`, and validate `is.numeric` on save.
- **V:** R medium/high; N medium/high.

#### F46 [medium] Fishing scenario ignores the selected fleet; the result cannot be plotted
- **Where:** R/functions/rpath/rpath_simulation.R:254
- **Claim:** `fleet_name` is unused and ForcedEffort is scaled for all fleets. The returned list is passed to `plot_ecosim_results`, which reads `$out_Biomass` and gets NULL. Errors become warning + NULL, so the server shows "Scenario complete!".
- **Failure scenario:** Choosing Trawl at 0.5× halves both fleets. The user sees a success toast and an empty plot.
- **Fix:** Scale only the chosen fleet column, plot `$scenario`, and let errors propagate.
- **V:** R medium/high; N medium/high.

#### F45 [medium] Sensitivity analysis always errors
- **Where:** R/modules/rpath_server.R:1760
- **Claim:** The call passes group, range and steps, but the signature is (rpath_model, parameter, variation, n_sims). The body reads `rpath_model$params`, which does not exist, and the plot reads fields that are never returned.
- **Failure scenario:** Every click gives "unused arguments" (reproduced).
- **Fix:** Pass params, vary one group over `seq(-range, range, length.out = steps)`, and return the plotted fields.
- **V:** R medium/high; N medium/high.

#### F47 [medium] Group-parameter Reset stores a data.frame in params$model; the editor disappears after balancing
- **Where:** R/modules/rpath_server.R:892 (gate at 723)
- **Claim:** The model reset is not converted back to data.table, unlike the diet reset. The table requires status == 'converted', but a balance sets it to 'balanced'.
- **Failure scenario:** Reset then balance fails with "object 'Type' not found" (plausible). After a balance, the parameter table goes blank.
- **Fix:** Wrap the reset in `as.data.table`, gate on `!is.null(params)`, and clear downstream results on edit.
- **V:** R medium/medium; N medium/medium (the data.table half is unverified because Rpath is absent).

#### F12 [medium] All SHARK wrappers call SHARK4R functions that do not exist
- **Where:** R/functions/shark_api_utils.R:68
- **Claim:** Nine `sharkdata_*` calls do not exist (per the upstream NAMESPACE). Errors are swallowed via `message()` and NULL.
- **Failure scenario:** Every query shows "No results found".
- **Fix:** Rewrite against the real API (`get_dyntaxa_records`, `match_worms_taxa`, `get_shark_data`, …), use `warning()`, or hide the tab.
- **V:** R medium/medium; N medium/medium (SHARK4R not installed locally).

#### F51 [medium] Prior #11 not fixed: EMODnet habitat enrichment still calls the undefined current_network()
- **Where:** R/modules/ecopath_import_server.R:1598
- **Claim:** The function is not a parameter and is defined nowhere. The error is shown as a misleading "ensure GDB exists" message. The fallback bbox is hard-coded and the GDB path is wd-relative.
- **Failure scenario:** The feature cannot be enabled.
- **Fix:** Use `current_metaweb()` or the sampling inputs, wrap the path in `app_path()`, and add a test.
- **V:** R medium/high; N medium/high.

#### F59 [medium] Prior #21 not fixed: deselecting 'S' empties the metrics table
- **Where:** R/modules/spatial_server.R:1268
- **Claim:** `metrics[metrics$S > 0, ]` gets NULL when S is not computed.
- **Failure scenario:** The table shows 0 rows while the toast reports N hexagons.
- **Fix:** Guard on `"S" %in% names(metrics)`, or always compute S.
- **V:** R medium/high; N medium/high.

#### F60 [medium] Prior #22 not fixed: s2 left off process-wide when the direct BBT load throws
- **Where:** R/modules/spatial_server.R:790
- **Claim:** There is no `on.exit` or restore in the handler. It uses a plain `<-` in the closure and `cat()` for logging.
- **Failure scenario:** Every session silently switches to planar lon/lat geometry.
- **Fix:** Save and restore with `on.exit`, use `warning()`, and use `app_path()`.
- **V:** R medium/high (rare trigger); N medium/high.

#### F61 [medium] Prior #31 not fixed: uploaded species coordinates unvalidated; a bad file kills the session
- **Where:** R/modules/spatial_server.R:1084 (observer 1119-1150)
- **Claim:** Only column presence is checked. `addCircleMarkers` runs in an unguarded `observe()`.
- **Failure scenario:** Comma decimals give a session disconnect. Swapped lon/lat silently match no hexes.
- **Fix:** Coerce and validate, report bad rows, warn on out-of-bbox points, and use tryCatch.
- **V:** R medium/high; N medium/high.

#### F73 [medium] Prior #25 still present: the Databases checkboxes are ignored
- **Where:** R/modules/trait_research_server.R:316
- **Claim:** `lookup_species_traits` has no databases parameter. Only biotic, maredat and ptdb are gated.
- **Failure scenario:** Unticking FishBase still queries FishBase. The modal names databases that are not used.
- **Fix:** Thread a databases argument through routing, or remove the inert checkboxes.
- **V:** R medium/high; N medium/high.

#### F74 [medium] An uploaded CSV whose species column is not first crashes network construction
- **Where:** R/functions/trait_foodweb.R:347
- **Claim:** `graph_from_data_frame(vertices = species_data)` takes the first column as vertex names. There is no tryCatch in the observer.
- **Failure scenario:** A header of "MS,species,…" gives "duplicated vertex names" and a session disconnect (reproduced).
- **Fix:** Reorder so species comes first, and wrap in tryCatch.
- **V:** R medium/high; N medium/medium.

#### F80 [medium] Prior #33/#32 not fully fixed: message() still used in error handlers that swallow live-source failures
- **Where:** R/functions/trait_lookup/api_trait_databases.R:134 (also 222/275/354/433; ecopath_windows.R 8 sites; ml_trait_prediction.R:125; database_lookups.R:714-728)
- **Claim:** Handlers call `message()` only, and results carry no error field. The PTDB switch is case-sensitive and defaults to primary_producer at TL 1.0.
- **Failure scenario:** An OBIS/TraitBank HTTP 500 is indistinguishable from "no data".
- **Fix:** Use `warning()` plus `result$error`, apply `tolower()` in the PTDB switch with an NA default, and add a lint guard.
- **V:** R low/medium; N medium/medium. Overlaps F16, F39, F53.

#### F82 [medium] Prior #36/#37 never landed: CI runs no testthat on push/PR and still skips R/functions/trait_lookup
- **Where:** .github/workflows/ci.yml:151 (r-check.yml:11/97)
- **Claim:** The workflows have not changed since 3309e8b (2026-04-11). The parse list uses `recursive = FALSE` and omits trait_lookup. No offline `test_dir` runs.
- **Failure scenario:** A syntax error or logic regression in orchestrator.R passes CI.
- **Fix:** Parse recursively and add an offline `test_dir(stop_on_failure=TRUE)` job.
- **V:** R medium/high; N medium/high.

#### F83 [medium] Mandatory pre-deploy gate syntax-checks only app.R/run_app.R
- **Where:** deployment/pre-deploy-check.R:240
- **Claim:** No file under R/ is parsed.
- **Failure scenario:** An unbalanced brace in rpath_server.R passes the gate and takes the whole app down on laguna.
- **Fix:** Parse every `R/**/*.R` file and count failures as errors.
- **V:** R medium/high; N medium/high.

#### F84 [medium] Live WoRMS/Polytraits/OBIS/TraitBank calls in the offline suite are not gated by RUN_LIVE_TESTS
- **Where:** tests/testthat/test-layer2c-api-databases.R:14
- **Claim:** A local `skip_if_offline` shadows the helper. The "structure" tests make real HTTP calls with 1-2 s timeouts via `setTimeLimit`. This is the same class a5250fb fixed in layer2a.
- **Failure scenario:** On a slow day, an elapsed-time error aborts the remaining files while the summary shows 0 failures.
- **Fix:** Use `skip_if_no_live_tests()` plus the helper's `skip_if_offline()`, and stub or fixture the structure tests.
- **V:** R medium/high; N medium/medium.

#### F53 [low] Prior #33 incomplete: ecopath_windows.R error handlers still use message()
- **Where:** R/functions/ecopath/ecopath_windows.R:205 (also 218, 233, 245, 261, 273, 291, 369; ecopath_unix.R:87/108/133)
- **Claim:** All optional-table catches call `message()`. ecopath_unix.R:133 is a bare `NULL` swallow.
- **Failure scenario:** On the Linux server only the metadata swallow at unix:133 is silent, which degrades the location label.
- **Fix:** Use `warning()`, including in ecopath_unix.R.
- **V:** R low/medium; N low/high.

#### F16 [low] API trait-source error handlers use message()
- **Where:** R/functions/trait_lookup/api_trait_databases.R:222 (also 135, 275, 354, 433)
- **Claim:** Five outer handlers swallow errors silently. `taxon_id == ""` errors on NA.
- **Failure scenario:** A PolyTraits HTML maintenance page silently loses traits, and nightly tests cannot see it.
- **Fix:** Use `warning()` and guard `taxon_id`.
- **V:** R low/high; N low/high. Duplicate scope of F80.

#### F39 [low] ML error handlers use message(); ML silently switches off in production
- **Where:** R/functions/ml_trait_prediction.R:124 (also 286-289; models_file L83)
- **Claim:** A failed readRDS or predict leaves no trace. The models path is wd-relative.
- **Failure scenario:** An incompatible randomForest RDS disables the ML tier invisibly.
- **Fix:** Use `warning()` and `app_path("models/trait_ml_models.rds")`.
- **V:** R low/high; N low/high.

#### F63 [low] Habitat overlay can run only once per grid
- **Where:** R/functions/emodnet_habitat_utils.R:708 (spatial_server.R:1024)
- **Claim:** The overlay overwrites `spatial_hex_grid`. A second merge produces `.x`/`.y` columns and the fill loop errors.
- **Failure scenario:** "replacement has 0 rows" and the "Error overlaying habitat" toast. The only way out is to rebuild the grid.
- **Fix:** Keep the base grid separate, or drop the habitat columns before merging.
- **V:** R medium/high; N low/medium.

#### F85 [low] Core scientific outputs (MTI/keystoneness, trophic levels, metaweb editing) have zero tests
- **Where:** R/functions/keystoneness.R:1 (trophic_levels.R:33, metaweb_core.R)
- **Claim:** No test references `calculate_mti`, `calculate_keystoneness`, the trophic-level functions, or 10 of the 11 metaweb_core functions. Spatial is covered only by an unrun legacy test.
- **Failure scenario:** Regressions in TL or MTI ship unnoticed. This is how F40, F56, F64 and F65 went undetected.
- **Fix:** Add known-answer testthat cases (a 3-species chain with TL 1/2/3, a known MTI matrix) and metaweb round-trip tests.
- **V:** R medium/high; N low/medium.

#### F22 [low] RS/TT/ST offline round-trip is dead at both ends
- **Where:** R/functions/trait_lookup/orchestrator.R:88 (build:772-777)
- **Claim:** The columns are selected but never assigned. The writer's `INSERT OR IGNORE` cannot attach values to existing species, and the counter overcounts.
- **Failure scenario:** Populated external CSVs have no effect. This is latent today because the CSVs are stubs by design.
- **Fix:** Use UPDATE…COALESCE or INSERT in the build, and `assign_trait_if_resolved()` in the reader.
- **V:** R low/high; N low/high.

#### F24 [low] Offline DB path relative to the working directory
- **Where:** R/functions/trait_lookup/orchestrator.R:47 (trait_research_server.R:1053/1216)
- **Claim:** It returns NULL silently when wd is not the repo root.
- **Failure scenario:** testthat never exercises the offline prefill or early-return path, so F20 and F22 were uncatchable. Production is unaffected.
- **Fix:** Use `app_path("cache/offline_traits.db")`.
- **V:** R low/medium; N low/medium.

#### F69 [low] Non-converged trophic levels returned (~101) with only a warning
- **Where:** R/functions/trophic_levels.R:76
- **Claim:** A self-loop-only node or a basal-less cycle diverges, and the vector is returned anyway.
- **Failure scenario:** `make_graph(c('P','A','A','B','B','A','C','C'))` gives C = 101, which skews mean TL and the layout.
- **Fix:** Return NA for nodes with no path from a basal node, or solve the linear system.
- **V:** R low/high; N low/medium.

#### F6 [low] Saving the API key modal erases the stored AlgaeBase password unless retyped
- **Where:** R/modules/plugin_server.R:267 (also 169, 284)
- **Claim:** The empty `passwordInput` is written unconditionally. The freshwaterecology key is shown in plain text to the admin.
- **Failure scenario:** An admin changes only the freshwater key, and AlgaeBase breaks silently.
- **Fix:** Treat an empty field as "unchanged", and show the key masked.
- **V:** R medium/high; N low/high (admin-gated).

#### F8 [low] Harmonization JSON import/export buttons call functions that do not exist
- **Where:** R/modules/harmonization_settings_server.R:149 (export at 141)
- **Claim:** `export_config_json` and `import_config_json` were never defined in any commit.
- **Failure scenario:** Both buttons fail with "could not find function".
- **Fix:** Implement them with jsonlite plus validation, or remove the tab.
- **V:** R medium/high; N low/high (visible failure).

#### F75 [low] Taxonomic-rule checkboxes and FS pattern inputs have no server handler
- **Where:** R/ui/harmonization_settings_ui.R:80
- **Claim:** `harm_rule_*`, `harm_pattern_FS*` and `harm_cancel` are never read.
- **Failure scenario:** Unticking "Arthropods exoskeleton" still yields PR4/PR8.
- **Fix:** Wire them into `rv$config` (with `isolate`), or remove them.
- **V:** R low/high; N low/high.

#### F15 [low] fetch_sag_ssb uses `<<-` in the tryCatch body, so expected failures raise "object 'result' not found"
- **Where:** R/functions/ices_lookups.R:388 (also 396, 403)
- **Claim:** The body is evaluated in the function frame, so `<<-` searches the enclosing environment.
- **Failure scenario:** The diagnostic is replaced by "object 'result' not found" plus a spurious warning. The functional outcome is unchanged.
- **Fix:** Use a plain `<-` in the body.
- **V:** R low/high (reproduced); N low/high (new since b386685).

#### F31 [low] Startup spawns an unused 4-worker multisession future plan; a missing 'future' aborts startup
- **Where:** app.R:96
- **Claim:** No live code uses the plan. parallel_lookup.R calls `stop()` at top level if its packages are missing. The banner advertises faster lookups.
- **Failure scenario:** Idle worker RSS until the first session ends (`shutdown_parallel_lookup` resets the plan). Startup fails on hosts missing future.apply.
- **Fix:** Remove `init_parallel_lookup()` or make it lazy, tryCatch the source, and fix the banner.
- **V:** R low/high (the "sustained 1.2 GB" claim is overstated); N low/medium.

#### F23 [low] Populate script sources a file that no longer exists and aborts immediately
- **Where:** scripts/populate_local_databases_to_sqlite.R:25
- **Claim:** R/functions/trait_lookup.R was split. The error counters use `<-` in closures, so they always report 0.
- **Failure scenario:** "cannot open file".
- **Fix:** Source `trait_lookup/load_all.R` and use `<<-`.
- **V:** R low/high; N low/high (offline tooling).

#### F86 [low] Deploy guard tests can pass on comments alone; the other two deploy scripts are unguarded
- **Where:** tests/testthat/test-deploy-preserve.R:51
- **Claim:** The dotfile and preserve greps include comments. There are no guards for deploy.sh or deploy-windows.ps1. The if-gated `expect_*` calls in test-trait-lookup-unit.R break the project convention.
- **Failure scenario:** Dropping `! -name '.*'` keeps the test green because '.Renviron' appears in a comment.
- **Fix:** Strip comments and assert on the actual lines. Add guards for the other scripts. Convert to `skip_if()`.
- **V:** R low/medium (deleting the whole PRESERVE_ITEMS line would still be caught); N low/high.

## Refuted findings

- **F13** (R/functions/api_rate_limiter.R:107, RateLimiter crash and no back-off): the defects are real but unreachable. RateLimiter is used only by `lookup_species_parallel`/`batch_lookup_parallel`, which have no callers. It is latent dead code; fix it before wiring it in.
- **F21** (R/functions/cache_sqlite.R:783, offline DB written on every lookup with no busy_timeout/WAL): unreachable contention. OSS shiny-server runs one R process per app, the parallel workers are never used, and the write is a microsecond autocommit. The only concurrent writer is the rebuild covered by F19.

## Coverage gaps

- **Lost finders:** none (12/12 completed).
- **Unverified findings:** 0. No verifier was lost, and all 86 unique findings received both lenses.
- **Environment limits recorded by verifiers:**
  - Rpath is not installed locally. F42, F43, F46 and F47 check Rpath's schema and API against repo contracts and documentation, not by execution.
  - SHARK4R is not installed (F12).
  - The production file system on laguna could not be read. Whether config/harmonization_custom.json exists there (F1/F76) and the nginx exposure behind F7 are both unverified.
- **Remediation audit gap:** the memory record's batch list covers only about 23 of the 44 July finding numbers. This run confirmed at least 20 of them as unfixed or partly fixed (theme 2). The remaining numbers not explicitly re-checked should be re-audited against code, not against the memory record.

## Suggested remediation batches

Each batch is sized for one PR. Write the failing test first, then the fix. Batches are ordered by severity.

1. **Edge-direction unification** (F64, F56, F48, F55, F65, F69, plus F85 tests). Start by adding known-answer tests: a 3-species chain with TL 1/2/3, an asymmetric hex web, a CSV producer at TL 1, and MTI with a positive producer impact. Then flip `metaweb_to_igraph` and spatial_analysis:259, swap the mislabelled bundled metaweb columns, drop the CSV transpose, rewrite MTI/KS, and make non-converged TL return NA.
2. **Rpath TL and diagnostics** (F40, F44, F41). Tests: a toy chain keeps Rpath's TL, a fixture with class 'Rpath' is accepted, cannibalism is preserved. Fix: delete the TL override, accept the Rpath list, stop zeroing the diagonal.
3. **Harmonization settings safety** (F1/F76, F2, F8, F75, F72). Tests: a testServer slider change settles, and save requires the admin gate. Fix: load once and use `isolate`, gate and validate saves, implement or remove the JSON import/export, wire or remove the rule inputs, key the trait cache on a config hash.
4. **Deploy and CI hardening** (F81, F4, F5, F7, F83, F82, F84, F86). Strengthen the guard tests first (strip comments, cover all three scripts). Then preserve data/config/dotfiles/models, exclude runtime config files, move backups out of site_dir, parse all of R/ in pre-deploy and CI, add an offline testthat job, and gate the layer2c live calls.
5. **Trait vocabulary consistency** (F17, F18, F36, F35, F77, F38, F71, F70). Tests: config labels match TRAIT_DEFINITIONS, plus regex regressions for benthopelagic, subtidal, "benthic surface" and "soft shell", and PR1/PR4 pass validation. Fix: define `mobility_labels`, route EP/PR through config patterns, fix the BIOTIC mapping and rebuild the DB, fix the MS1 column and the threshold semantics.
6. **Trait lookup correctness** (F33, F32, F78, F79, F9, F10, F11, F12). Tests: a zero-length class does not crash, the WoRMS unit child is honoured, FishBase grams are kept, the AlgaeBase tibble is indexed correctly, WoRMS results get medium confidence, a binomial is not singularised.
7. **Network finalize and import** (F67, F66, F50, F49, F52, F68, F51, F74). Tests: a Baltic export keeps biomass and fg, info without fg keeps per-species groups, an EcoBase result has met.types, EwE Type 2 maps to Detritus, a data-editor edit stays numeric.
8. **Orchestrator confidence and provenance** (F20, F25, F26, F27, F28, F29, F34, F37, F30, F22, F24). Tests use an `app_path` DB fixture: offline confidences propagate, phylo-imputed traits are scored, PR stays NA without evidence, the prefilled guard holds, the self-file is excluded from relatives, boundary confidence is above 0.
9. **Rpath dynamics** (F43, F45, F46, F47, F42). Requires Rpath in CI or a dev environment. Fix the signatures, apply scenario inputs or remove the "configured" messages, add real fleet and detritus columns, fix the Reset type and the status gate.
10. **Spatial state** (F57, F58, F59, F60, F61, F62, F63). Tests: cache invalidation on study-area change, grid reset clears metrics, the S-less table is not empty, s2 is restored on error, bad coordinates are rejected, NA biomass is kept, overlay is idempotent.
11. **Security and admin gating** (F19, F3, F54, F6). Add the admin gate, a lock file and atomic rename for the rebuild, htmlEscape/tags for the EcoBase and preview panels, and keep the password when the field is empty.
12. **Conventions sweep** (F80, F16, F39, F53, F15, F14, F31, F23, F73). Convert `message()`→`warning()` in error handlers and add a lint or guard test, fix the `<<-` misuse, make the DATRAS cache track completeness, remove the unused future plan, fix the populate script, and honour or remove the Databases checkboxes.
