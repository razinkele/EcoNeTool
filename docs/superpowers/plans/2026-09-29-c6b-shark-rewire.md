# C-6b: SHARK Data Tab Rewired to SHARK4R 1.2.0 (C2.8, F12) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The SHARK Data tab works again on SHARK4R 1.2.0. SHARK4R 1.2.0 no longer exports any of the 12 functions the tab called, so every action on the tab fails in production today. Each old call is rewired to its 1.2.0 equivalent, or its wrapper is removed where nothing replaces it or nothing uses it. Taxonomy uses WoRMS; Dyntaxa and AlgaeBase are offered only when their subscription keys are set. Environmental and species data come from `get_shark_data()`. Quality control uses SHARK4R's field and outlier checks, plus local completeness and coordinate checks. All handlers warn instead of `message()` (F12). Without a usable SHARK4R the tab shows installation help instead of failing. A guard test pins every `SHARK4R::` call to the package's export list, so the next API change fails CI instead of production. The branch ships as patch release 1.6.3 and is deployed; no offline-DB rebuild is needed.

**Architecture:** `R/functions/shark_api_utils.R` is rewritten; it is the only file that talks to SHARK4R for the tab.
- `SHARK4R_FUNCTIONS` lists every SHARK4R function the app calls.
- `shark4r_installed()` checks the installed version without loading the package. `check_shark4r_available()` also checks the exports.
- Taxonomy wrappers return a status list: `found`, `not_found`, `no_key` or `error`.
- Data wrappers return `list(status, data, message, total)`: `ok`, `empty` or `error`.
- The QC checks are separate small functions, composed by `run_shark_qc()`.

`R/modules/shark_server.R` gains pure render helpers (`shark_taxonomy_card()`, `shark_query_status()`, `shark_occurrence_popup()`) so the escaping is unit-tested. The module only wires inputs to wrappers. `R/ui/shark_ui.R` uses the exact SHARK parameter names, offers only the configured taxonomy sources, adds a QC data-type select, and falls back to `shark_unavailable_ui()`. `app.R` and the plugin system are unchanged. All offline tests mock SHARK4R with `local_mocked_bindings(.package = "SHARK4R")`. The live checks are behind `RUN_LIVE_TESTS=true`.

**Tech Stack:** R 4.4.1, SHARK4R 1.2.0 (laguna; CRAN now has 1.2.1), shiny 1.11.1 (`testServer`), testthat 3.3.2 (`local_mocked_bindings`), withr 3.0.2, tibble, htmltools, leaflet, DT, bs4Dash. Windows dev box (Git Bash / PowerShell), Linux deploy target (laguna.ku.lt, shiny-server, R 4.6.1).

**Spec:** `docs/superpowers/specs/2026-09-26-fix-c-trait-pipeline-correctness-design.md`. This plan covers:
- section 2 row F12;
- section 4 C2.8 ("dead-UI policy", wire-or-remove);
- the SHARK row of the section 5 test table;
- section 6 Rollout item 3 ("PR C-6b (SHARK): C2.8. Record the export list in the PR");
- the section 7 risk "SHARK4R API drift".

It also implements the user's decision of 2026-09-29 to **rewire the tab, not remove it**. It follows `docs/superpowers/specs/2026-09-26-fix-overview.md` for the shared rules ("dead UI: wire if backend exists else remove"). It does not touch the trait pipeline: `lookup_shark_traits()` is unrouted (Deviations, item 17).

> **Execution rulings (plan-time facts and the user's decisions of 2026-09-29; the controller confirms 2 at release time). These override the tasks below where they conflict:**
> 1. **Release flow:** the fix PR (Tasks 0-4) is squash-merged WITHOUT a version bump or a CHANGELOG regeneration. The release then runs as its own PR, cut from the updated master (Task 5):
>    - `scripts/version_bump.R`;
>    - CRLF -> LF;
>    - `GIT_BRANCH=master`;
>    - the `R/config.R` fallback;
>    - `scripts/generate_changelog.R`;
>    - re-insert every `docs/releases/<ver>-*.md` under its header (`1.5.0-results-changed.md`, `1.5.2-notes.md`, `1.5.3-notes.md`, `1.6.0-notes.md`, `1.6.1-notes.md`, `1.6.2-notes.md`, and the new `1.6.3-notes.md`);
>    - exactly one trailing newline;
>    - merge, then tag `v1.6.3`.
> 2. **Version:** the next PATCH, **1.6.3** / `v1.6.3`, name "SHARK Rewire". Master is 1.6.2 (tags `v1.6.0`, `v1.6.1`, `v1.6.2`). If another release lands first, the controller rules on the number.
> 3. **No offline-DB rebuild.** C-6b touches no file that `scripts/initialization/build_offline_trait_db.R` sources (`harmonization_config.R`, `validation_utils.R`, `harmonization.R`, `offline_db_rebuild.R`), and no trait lookup. `cache/offline_traits.db` is not written anywhere in this plan.
> 4. **SHARK4R versions:** production (laguna) has SHARK4R 1.2.0 in `/usr/local/lib/R/site-library` (verified 2026-09-29), which the `shiny` user can read. CRAN now serves 1.2.1, which is what CI installs (Task 3). Every function in `SHARK4R_FUNCTIONS` exists in 1.2.0; the export guard runs in CI against 1.2.1. The dev box needs SHARK4R >= 1.2.0 (Task 0); the plan was verified against 1.2.0.
> 5. **Subscription keys (User decision 1, pending; the plan implements option (a)):** laguna has neither `DYNTAXA_KEY` nor `ALGAEBASE_KEY` (checked 2026-09-29 in the app `.Renviron`, `~razinka/.Renviron` and `~shiny/.Renviron`; only key names were grepped, no values read). With option (a), production shows WoRMS only, plus a "not configured" note. Provisioning a key is an optional STOP step in Task 6.
> 6. Every outward step is **STOP - ask the user**: push, PR, merge, tag, and anything on laguna. Production `data/` must never be deleted.
> 7. **Network steps** (Task 3 Step 5) call the live SHARK and WoRMS APIs from the dev box. They are marked **NETWORK**, not STOP. The offline suite never touches the network.
> 8. **Local test runs:** the full suite is too heavy for the 16 GB dev box (it gets killed); CI runs it on the PR. Implementers run the touched test files only, one R process at a time.

## Global Constraints

- **Branch:** `fix/c6b-shark-rewire`, cut from `master` at `64a4bca` (v1.6.2) or later. Every commit message ends with these two lines:

  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
  ```
- **Spec C2.8:** "Wrappers `sharkdata_dyntaxa_search`, `sharkdata_worms_search` and `sharkdata_get_biological` are rewired to `SHARK4R::get_dyntaxa_records`, `SHARK4R::match_worms_taxa` and `SHARK4R::get_shark_data`." "The other six wrappers (algaebase_search, get_parameters, get_physical_chemical, validate, quality_check, list_datasets) are rewired only if a same-purpose function is in `getNamespaceExports("SHARK4R")` at PR time. The implementer installs SHARK4R and records the export list in the PR description. Otherwise each wrapper and its UI control/output in `shark_ui.R`/`shark_server.R` is **removed**." "All handlers use `warning()`."
- **Spec section 3 (non-goal):** "Rewriting the SHARK tab UX beyond the wire-or-remove rule in C2.8."
- **B3 XSS rule:** dynamic text reaches the page only through tag builders (`tags$td(x)` escapes `x`), never `HTML(paste0(...))`. Leaflet popups are HTML strings, so every field goes through `htmltools::htmlEscape()`. Only integer AphiaIDs become links.
- **C-6a patterns:**
  - scalar fields are read with `.scalar_chr()` (`R/functions/validation_utils.R`: NA for NULL, zero-length, NA or "");
  - per-item loops cannot abort on one item (the QC outlier loop catches per parameter; each taxonomy source is its own `tryCatch`).
- **CLAUDE.md conventions:**
  - `warning()`, not `message()`, in error handlers;
  - `<<-` in error closures;
  - `app_path()` for runtime paths;
  - `skip_if()` / `skip_if_not_installed()`, never `if (cond) expect_*()`;
  - `<-`, 120-char lines, no tabs or trailing whitespace;
  - live tests behind `RUN_LIVE_TESTS=true`, with `with_timeout()` around raw network calls.
- **Parse-check** every edited `.R` file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='<path>'); cat('OK\n')"`.
- **Tests (one file):** `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_file('tests/testthat/<file>', reporter = 'silent', stop_on_failure = FALSE)); cat('tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n')"`. The "package ... was built under R version 4.4.3" warnings are noise.
  - Do not run the full suite locally (Execution ruling 8). `tests/run_all_tests.R` needs `dggridR`; do not use it.
- **Writing files:**
  - Write R files and multi-line replacements with the Write/Edit tools only, never shell heredocs.
  - In R source every regex backslash is doubled (`"\\b"`). The code blocks below are exactly what goes into the files.
  - New R code is ASCII, except the `—` escape (an em dash rendered in the cards).
- **Staging and forbidden files:**
  - `git add` explicit paths only.
  - Never stage `WBGIFSV5ISSUE70.pdf`, anything under `config/` or `cache/`, or any `*safeBackup*` file.
  - Never create, read, copy, move or delete `R/config-laguna-safeBackup-0001-CONTAINS-freshwaterecology-API-KEY.R.bak`, `R/functions/admin_auth-laguna-safeBackup-0001.R` (gitignored, local only; the export-guard scan skips it by name), or any other `*.bak` / `*safeBackup*` file.
  - Never write `cache/offline_traits.db`. Every test that could write a cache uses `use_cache = FALSE`, `withr::local_tempdir()`, or a mocked `app_path()`.
- **Verification basis:**
  - Every code block below was applied to a scratch copy of master (`64a4bca`). The copy held explicit files only, via `git archive master -- <file list>`, never `R/` wholesale. SHARK4R 1.2.0 was installed locally.
  - **`test-shark-rewire.R`:** on master all 31 tests fail (6 passing expectations). After Task 1, its 25 tests pass (127 expectations). After Task 2, all 31 pass (152 expectations).
  - **`test-p0p1-fixes.R`:** 7 tests, 0 failing, 2 empty (reported as skipped) on master; 5 tests, 0 failing, 9 passing after Task 1.
  - **`test-ci-workflows.R`:** 5 tests, 0 failing, 21 passing on master (run in the real tree). The new test fails before the workflow edit. After Task 3: 6 tests, 0 failing, 23 passing.
  - **`test-shark-live.R`:** skips all 3 tests offline. With `RUN_LIVE_TESTS=true` on 2026-09-29, all 3 passed (8 expectations).
  - **Lint:** 0 line-length, trailing-whitespace or assignment lints in every new or edited R file.
  - **Not run in planning:** the full suite, which CI runs (Execution ruling 8).
- **Live API calls during planning (2026-09-29, read-only public APIs; no fixture files were written, and the test data is typed in from these responses):**
  - `match_worms_taxa()` on "Gadus morhua" and "Xx yy";
  - `get_shark_options()`;
  - `get_shark_table_counts()` four times (Kattegat June 2024 CTD; Aurelia aurita 2023; Temora longicornis, Macoma balthica and Gadus morhua 2022);
  - `get_shark_data()` four times (Kattegat June 2024 CTD: 96 rows in 25 s; Aurelia aurita 2023 and "Xx yy": 0 rows, 43 s; Macoma balthica 2022: 928 rows);
  - `check_fields()`, `check_datatype()`, `check_zero_positions()` and `check_outliers()` on the Macoma sample (these checks are offline);
  - the three `test-shark-live.R` tests.
  - Laguna was inspected read-only: the SHARK4R version and library path, and whether the key names exist.

## The SHARK4R export map (spec rollout item 3; this table goes into the PR description)

Every function below was absent from `getNamespaceExports("SHARK4R")` for SHARK4R 1.2.0 (160 exports; verified locally and on laguna).

| Old `SHARK4R::` call | Old wrapper | SHARK4R 1.2.0 replacement | Outcome |
|---|---|---|---|
| `sharkdata_worms_search`, `worms_search` | `query_shark_worms()` | `match_worms_taxa()` | **rewired** |
| `sharkdata_dyntaxa_search`, `dyntaxa_search` | `query_dyntaxa()` | `match_dyntaxa_taxa()`, needs `DYNTAXA_KEY` | **rewired, key-gated** (deviation 1) |
| `sharkdata_algaebase_search`, `algaebase_search` | `query_algaebase()` | `parse_scientific_names()` + `match_algaebase_taxa()`, needs `ALGAEBASE_KEY` | **rewired, key-gated** |
| `sharkdata_get_physical_chemical` | `get_shark_environmental_data()` | `get_shark_data(dataTypes = "Physical and Chemical")` | **rewired** |
| `sharkdata_get_biological` | `get_shark_species_occurrence()` | `get_shark_data(taxonName = )` | **rewired** |
| `sharkdata_validate` | `validate_shark_data()` | `check_fields()` + `translate_shark_datatype()` | **rewired** (new data-type select) |
| `sharkdata_get_parameters` | `get_available_shark_parameters()` | `get_shark_options()$parameters` exists | **wrapper removed** (nothing called it); the UI uses the exact names, pinned by a live test (deviation 3) |
| `sharkdata_list_datasets` | `get_shark_datasets()` | `get_shark_options()$datasets` exists | **wrapper removed** (nothing called it; it shadowed `SHARK4R::get_shark_datasets()`) (deviation 4) |
| `sharkdata_quality_check` | `check_data_quality()` | none | **kept as a local completeness check** (deviation 5) |
| none: the "Outlier detection" checkbox had no handler | none | `check_outliers()` | **wired** (User decision 2) |
| none: the "Coordinate validation" checkbox had no handler | none | none needed | **wired locally** (User decision 2) |

After this PR, `SHARK4R_FUNCTIONS` is `match_worms_taxa`, `match_dyntaxa_taxa`, `parse_scientific_names`, `match_algaebase_taxa`, `get_shark_data`, `translate_shark_datatype`, `check_fields` and `check_outliers`. It includes `database_lookups.R`'s existing `get_shark_data` call.

## Deviations from the spec (and why)

1. **Dyntaxa uses `match_dyntaxa_taxa()`, not `get_dyntaxa_records()`** (spec C2.8 names the latter).
   - `get_dyntaxa_records(taxon_ids)` takes taxon IDs, not names. The name search is `match_dyntaxa_taxa()`; `match_taxon_name()` is its deprecated alias, which warns.
   - It returns `search_pattern, taxon_id, best_match, author, valid_name` (from the 1.2.0 source; not called live, because there is no key). So the Dyntaxa card shows matched name, recommended scientific name, taxon ID and author.
   - The old Swedish-name, Class and Family rows are dropped. They would need a second keyed `get_dyntaxa_records()` call whose response shape could not be verified (YAGNI).
   - Fuzzy maps to `multiple_options = FALSE` (first accepted hit). Exact maps to `multiple_options = TRUE`, which keeps only case-insensitive exact names.
2. **Dyntaxa and AlgaeBase need subscription keys** (`DYNTAXA_KEY`, `ALGAEBASE_KEY`). SHARK4R aborts without them. The Trait Research AlgaeBase username/password (`API_KEYS$algaebase_*`) is a different credential. See User decision 1.
3. **Parameters are a static list of exact SHARK names** (`SHARK_ENV_PARAMETERS`), not a runtime `get_shark_options()` call.
   - The old choices were slugs ("temperature") that SHARK does not know.
   - Calling SHARK while the UI is built would put the network on the start-up path.
   - The live test pins the eight names against `get_shark_options()$parameters`.
4. **`get_shark_datasets()` (app wrapper) is removed.** No UI or server code called it. Its name shadowed `SHARK4R::get_shark_datasets()`, which in 1.2.0 downloads zip archives, a different purpose. Its `test-p0p1-fixes.R` test was an `if (precondition) expect_*` empty test and goes too; so does the one for `get_available_shark_parameters()`.
5. **Completeness stays, computed locally** (`check_data_quality()`, no SHARK4R). SHARK4R has no same-purpose function. The old wrapper already fell back to this computation whenever its SHARK4R call failed, which it always did, so this is what production users saw.
6. **Format validation needs a data type:** `check_fields()` validates against the field definitions of one delivery data type. The new `shark_qc_datatype` select defaults to "Auto", which uses the file's `delivery_datatype` column when it holds exactly one known value.
   - A SHARK web export is not a delivery file; `check_fields()` reports its missing delivery fields (977 errors on the Macoma sample).
   - So the report lists the first 20 unique messages and a count, not every row.
7. **The two dead QC checkboxes are wired** (User decision 2):
   - **Outliers:** `check_outliers()` runs per parameter against SHARK4R's bundled thresholds (offline; 12 data types, none for Physical and Chemical). A parameter without a threshold is listed as unchecked, and its SHARK4R warning is muffled.
   - **Coordinates:** a local count of zero, out-of-range and missing positions. `check_zero_positions()` covers zeros only, so it is not used.
8. **`get_shark_data()` has no date or record-cap arguments:**
   - the date range maps to `fromYear`/`toYear` and is then filtered on `sample_date`;
   - "Max Records" truncates after download, and the status says "Showing the first N of M records";
   - there is no pre-count: `get_shark_table_counts()` took 18 s and has no `bounds` argument;
   - the timeout is 90 s (`SHARK_QUERY_TIMEOUT_S`), because observed latencies were 25-43 s. The trait lookup's 20 s would fail most queries.
9. **Bounding box:** SHARK wants `bounds = c(lon_min, lat_min, lon_max, lat_max)`. A blank `numericInput` is `NA`, and the old `!is.null()` guard let NAs through. Now all four blank means no filter; a partly filled or inverted box is refused with a message, and SHARK is not called.
10. **Result contracts:** the taxonomy wrappers return a status list instead of `NULL`, so the card says "not found", "needs a key" or "lookup failed". The data wrappers return `list(status, data, message, total)`, so the tab tells "no records" apart from "SHARK failed"; the old tab showed "No data retrieved yet" after a failure.
11. **Species records are long format:** one row per measured parameter ("# counted", "Abundance", "Wet weight"). The table shows Parameter / Value / Unit instead of an invented "Abundance" column, and the tab labels say "records". The map popup escapes every field (B3).
12. **UI defaults:**
    - The environmental date range defaults to 3 years, not 1. SHARK's newest year was 2025 on 2026-09-29, so a 1-year default returned nothing.
    - Placeholders name taxa SHARK holds. Gadus morhua has 0 SHARK rows for 2022.
    - The broken "torsk"/"sill" tip now applies to Dyntaxa only.
13. **Graceful degradation lives in the tab, not the plugin system.** `AVAILABLE_PLUGINS` package checks only drive the settings modal, and the sidebar `menuItem`s in `app.R` are static.
    - `shark_ui()` shows `shark_unavailable_ui()` when `shark4r_installed()` is FALSE, and `shark_server()` then registers nothing.
    - `shark4r_installed()` reads the installed DESCRIPTION only, so start-up does not load SHARK4R and its sf/terra imports.
    - `app.R` is unchanged.
14. **All `message()` calls are gone from `shark_api_utils.R`**, not only the six error handlers. The progress chatter is invisible in production anyway. Failures warn with a `[shark]` prefix.
15. **Cache:** the default `cache_dir` is `app_path("cache", "shark")` (CLAUDE.md runtime paths; it was wd-relative). Only `found` results are cached, for 30 days. An old cache file lacks `status` and is ignored; none can exist, since the old calls always errored before the cache write.
16. **Server-side check (Task 6)** sources the four SHARK files and builds `shark_ui()` on laguna, then makes one WoRMS and one SHARK call without cache. It does not source the whole `app.R`: that runs data loading and can create razinka-owned directories inside the live tree, which the `shiny` user could not write.
17. **`lookup_shark_traits()` (`database_lookups.R`) is not changed** (finding, out of scope).
    - It is unrouted: SHARK was removed from trait routing, and `test-layer1-quality.R` pins that.
    - It passes `dataTypes = "PhysicalChemical"`, which is not a SHARK data-type name (SHARK uses "Physical and Chemical"), and physical-chemical rows carry no taxon. With its 20 s timeout it could never return data.
    - Its `get_shark_data` call is covered by `SHARK4R_FUNCTIONS` and the export guard. The fix belongs to whoever re-routes SHARK traits.

## User decisions (pending; the controller asks before Task 1)

1. **Dyntaxa / AlgaeBase keys.**
   - **(a) Recommended, implemented below:** wire both, read the keys from `DYNTAXA_KEY` / `ALGAEBASE_KEY` (SHARK4R's own defaults), and offer a source only when its key is set. Without a key the tab says so.
   - **(b)** Remove Dyntaxa and AlgaeBase and keep WoRMS only. This drops `query_dyntaxa()`, `query_algaebase()`, their tests, and `match_dyntaxa_taxa` / `parse_scientific_names` / `match_algaebase_taxa` from `SHARK4R_FUNCTIONS`.
   - **(c)** Option (a), plus provisioning keys on laguna now (Task 6, Step 6).
     - A Dyntaxa key is free from the SLU Artdatabanken developer portal.
     - AlgaeBase keys are a paid subscription.
2. **The two dead QC checkboxes.**
   - **Recommended, implemented:** wire them (deviation 7). The rule is "wire if backend exists", and `check_outliers()` exists.
   - Alternative: remove "Outlier detection" and "Coordinate validation", and drop `check_shark_outliers()`, `check_shark_coordinates()`, their tests and `check_outliers` from `SHARK4R_FUNCTIONS`.
3. **SHARK4R in CI.**
   - **Recommended, implemented (Task 3):** add `any::SHARK4R` to both test jobs, with a guard test. Without it the export guard and every mocked test skip in CI, which is how the tab broke unnoticed. `terra` is a new transitive dependency; its system libraries (GDAL/GEOS/PROJ) are already in the apt list.
   - Alternative: keep CI unchanged, and accept that the SHARK tests run only on machines that have SHARK4R.

## Review Focus

- **SHARK is slow or does not answer** (25-43 s is normal): the query ends after 90 s with a "narrow the query" status and a `[shark]` warning, not a hung session or a silent empty table. Pinned by Task 1 test "a failing or timed-out SHARK query warns and returns status error (F12)".
- **Blank or partly filled bounding box** (`numericInput` gives NA, not NULL): blank sends no `bounds`; partial or inverted is refused before any network call. Pinned by Task 1 test "a blank bounding box sends no bounds; a partial or inverted one is refused".
- **A name WoRMS does not know** (`match_worms_taxa()` returns a row with `AphiaID` NA and status "no content", not zero rows): the card says "No results found", and nothing is cached. Pinned by Task 1 tests "a name WoRMS does not know is not_found, not a result (F12)" and "only found WoRMS results are cached".
- **A server without subscription keys** (production today): Dyntaxa and AlgaeBase are not offered, the tab says why, and a stale selection yields a "needs a key" card, not an error. Pinned by Task 1 tests "Dyntaxa without DYNTAXA_KEY is no_key and makes no call" and "the taxonomy sources offered follow the subscription keys", and by Task 2 test "the SHARK UI offers exact SHARK parameters, the key-gated sources and a QC data type".
- **Third-party text** (SHARK taxon names, WoRMS authorities, error messages) in cards, status lines and leaflet popups: rendered as text, never markup; only integer AphiaIDs become links. Pinned by Task 2 tests "taxonomy cards escape third-party text ...", "occurrence popups escape every field" and "query status lines distinguish idle, ok, empty and error".
- **SHARK4R missing, too old, or changed again:** the tab shows installation help, and a missing export fails CI. Pinned by Task 2 test "without SHARK4R >= 1.2.0 the tab shows installation help and the server does nothing", by Task 1's two export tests, and by Task 3's CI guard.

## Interfaces produced (later tasks and future PRs rely on these)

| Name | Signature / value | Where |
|---|---|---|
| `SHARK4R_MIN_VERSION` | `"1.2.0"` | `R/functions/shark_api_utils.R` |
| `SHARK4R_FUNCTIONS` | character vector of every `SHARK4R::` function called in `R/` and `app.R` | same |
| `SHARK_QUERY_TIMEOUT_S` | `90` | same |
| `SHARK_ENV_PARAMETERS` | named character (label = exact SHARK parameter name), 8 entries | same |
| `SHARK_QC_DATATYPES` | 15 SHARK data-type names that `check_fields()` knows after `translate_shark_datatype()` | same |
| `SHARK_DISPLAY_COLUMNS` | `list(environmental =, occurrence =)` of display name -> SHARK column | same |
| `shark4r_installed()` | `-> logical(1)`, no namespace load | same |
| `check_shark4r_available()` | `-> logical(1)`; warns when exports are missing | same |
| `shark_subscription_key(source)` | `"dyntaxa"`/`"algaebase"` `-> character(1)` key or NA | same |
| `shark_taxonomy_source_choices()` | `-> named character` of "worms" plus configured sources | same |
| `query_shark_worms(species_name, fuzzy = TRUE, use_cache = TRUE, cache_dir = app_path("cache", "shark"))` | `-> list(source, query, status, message?, scientific_name, aphia_id (integer), authority, taxon_status, kingdom, phylum, class, order, family, genus, rank)` | same |
| `query_dyntaxa(species_name, fuzzy = TRUE, use_cache = TRUE, cache_dir = ...)` | `-> list(source, query, status, message?, matched_name, scientific_name, taxon_id, author)` | same |
| `query_algaebase(species_name, use_cache = TRUE, cache_dir = ...)` | `-> list(source, query, status, message?, scientific_name, algaebase_id, authority, taxon_status, phylum, class)` | same |
| `get_shark_environmental_data(parameters, start_date, end_date, bbox = NULL, max_records = 10000)` | `-> list(status = "ok"/"empty"/"error", data, message, total)` | same |
| `get_shark_species_occurrence(species_name, start_date, end_date, bbox = NULL, max_records = 5000)` | same shape | same |
| `format_shark_results(raw_data, result_type = c("environmental", "occurrence"))` | `-> data.frame` of display columns | same |
| `read_shark_qc_file(path, name = path)` | `-> data.frame` or NULL + warning | same |
| `resolve_shark_qc_datatype(data_frame, selected = "auto")` | `-> character(1)` data type or NA | same |
| `validate_shark_data(data_frame, datatype)` | `-> list(valid, message, errors, warnings, n_errors, n_warnings)` | same |
| `check_data_quality(data_frame)` | `-> list(completeness, missing_values, record_count, column_count, message)` | same |
| `check_shark_outliers(data_frame, datatype)` | `-> list(message, outliers, checked, unchecked)` | same |
| `check_shark_coordinates(data_frame)` | `-> list(message, zero, out_of_range, missing)` | same |
| `run_shark_qc(data_frame, checks, datatype)` | `-> list(error = FALSE, datatype, validation, quality, outliers, coordinates, data_summary)` | same |
| `format_shark_qc_report(qc, max_items = 20L)` | `-> character` lines | same |
| `shark_taxonomy_card(source_name, result)`, `shark_query_status(res, idle_text)`, `shark_occurrence_popup(d)` | render helpers -> tag / tag / character | `R/modules/shark_server.R` |
| `shark_ui()`, `shark_requirements_box(collapsed = TRUE)`, `shark_unavailable_ui()` | tab UI | `R/ui/shark_ui.R` |
| input `shark_qc_datatype` | `"auto"` or a `SHARK_QC_DATATYPES` value | `R/ui/shark_ui.R` |

Removed: `get_available_shark_parameters()`, `get_shark_datasets()` (the app wrapper; `SHARK4R::get_shark_datasets()` is untouched).

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `R/functions/shark_api_utils.R` | Rewrite (Task 1) | SHARK4R availability and export list; key-gated taxonomy wrappers; SHARK data wrappers; QC checks and report |
| `tests/testthat/test-p0p1-fixes.R` | Modify (Task 1) | Drop the two empty tests of removed wrappers and the now-unneeded `source()` |
| `R/modules/shark_server.R` | Rewrite (Task 2) | Render helpers; wiring of the four sub-tabs to the wrappers |
| `R/ui/shark_ui.R` | Rewrite (Task 2) | Exact parameters, configured sources, QC data type, install-help fallback |
| `tests/testthat/test-shark-rewire.R` | Create (Task 1), append (Task 2) | All C-6b offline tests |
| `.github/workflows/ci.yml`, `.github/workflows/nightly-live-tests.yml` | Modify (Task 3) | Install SHARK4R in both test jobs |
| `tests/testthat/test-ci-workflows.R` | Modify (Task 3) | Guard: both jobs install SHARK4R |
| `tests/testthat/test-shark-live.R` | Create (Task 3) | Live checks: parameter names, WoRMS, one SHARK query |
| `CONTRIBUTING.md` | Modify (Task 4) | Convention: every `SHARK4R::` call is listed in `SHARK4R_FUNCTIONS` |
| `VERSION`, `R/config.R`, `app.R`, `README.md`, `CHANGELOG.md`, `docs/releases/1.6.3-notes.md` | Modify / Create (Task 5, release PR) | 1.6.3 |

Not touched (noted for reviewers):
- `app.R` (the tab's `source()`, `menuItem`, `shark_ui()` and `shark_server()` calls stay; the tab handles a missing SHARK4R itself);
- `R/config/plugins.R` (deviation 13);
- `R/functions/trait_lookup/database_lookups.R` (deviation 17);
- `R/functions/api_rate_limiter.R` (`get_shark_limiter()` is unused by the tab);
- `R/ui/dashboard_ui.R` (static release text mentioning SHARK).

---

### Task 0: Branch, preconditions and baseline

**Files:** none modified.

**Interfaces:**
- Consumes: master at v1.6.2 or later; `.scalar_chr()`, `with_timeout()`, `app_path()` in `R/functions/validation_utils.R`.
- Produces: branch `fix/c6b-shark-rewire`, SHARK4R >= 1.2.0 in the local R library, and recorded baselines for the touched test files.

- [ ] **Step 1: Confirm preconditions and create the branch**

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git checkout master && git pull --ff-only
grep -n "^VERSION=" VERSION
git tag -l "v1.6.*"
grep -n "^\.scalar_chr <- function\|^with_timeout <- function\|^app_path <- function" R/functions/validation_utils.R
grep -o "SHARK4R::sharkdata_[a-z_]*" R/functions/shark_api_utils.R | sort -u | wc -l
git checkout -b fix/c6b-shark-rewire
```

Expected:
- `VERSION=1.6.2` (or later) and tags `v1.6.0`, `v1.6.1`, `v1.6.2`;
- the three definitions printed;
- the count `9`: the nine distinct dead `sharkdata_*` functions are still called (on 10 lines; verified on master).

If a definition is missing or the count is not 9, **STOP** and tell the user.

- [ ] **Step 2: Make sure SHARK4R >= 1.2.0 is installed locally**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "cat(if (nzchar(system.file(package = 'SHARK4R'))) as.character(packageVersion('SHARK4R')) else 'missing', '\n')"
```

If it prints `missing` or a version below 1.2.0, install the version production runs (a download from CRAN; this is the R library, not micromamba):

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "install.packages('https://cran.r-project.org/src/contrib/Archive/SHARK4R/SHARK4R_1.2.0.tar.gz', repos = NULL, type = 'source')"
```

Expected: `1.2.0` (1.2.1 is also acceptable). All of SHARK4R's imports (sf, terra, worrms, ...) were already present on this machine at plan time.

- [ ] **Step 3: Record the baselines**

Run the one-file command (Global Constraints) for `test-p0p1-fixes.R`, then for `test-ci-workflows.R`.

Expected:
- `tests 7 failing 0 skipped 2 passed 9`: the two SHARK tests are empty on a machine with SHARK4R;
- `tests 5 failing 0 skipped 0 passed 21`.

Write both lines into your notes. If either fails, stop and report.

---

### Task 1: The SHARK4R API layer (F12; export map)

**Files:**
- Rewrite: `R/functions/shark_api_utils.R` (whole file).
- Modify: `tests/testthat/test-p0p1-fixes.R`: drop the `shark_api_utils.R` `source()` and the two SHARK tests.
- Create: `tests/testthat/test-shark-rewire.R`

**Interfaces:**
- Consumes: `.scalar_chr()`, `with_timeout(expr, timeout, on_timeout)`, `app_path(...)`, `get_app_root()` (helper-fixtures). SHARK4R 1.2.0: `match_worms_taxa`, `match_dyntaxa_taxa`, `parse_scientific_names`, `match_algaebase_taxa`, `get_shark_data`, `translate_shark_datatype`, `check_fields`, `check_outliers`.
- Produces: everything in the Interfaces table that lives in `shark_api_utils.R`. Task 2 uses `shark4r_installed()`, `shark_taxonomy_source_choices()`, `SHARK_ENV_PARAMETERS`, `SHARK_QC_DATATYPES`, the `query_*` / `get_shark_*` wrappers, `format_shark_results()`, `read_shark_qc_file()`, `resolve_shark_qc_datatype()`, `run_shark_qc()` and `format_shark_qc_report()`.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-shark-rewire.R` with exactly:

```r
# C-6b (spec C2.8, F12): the SHARK Data tab called 12 SHARK4R functions that
# SHARK4R 1.2.0 does not export, so every SHARK action failed in production.
# These tests pin the rewire to the 1.2.0 API. SHARK4R is mocked with
# local_mocked_bindings(.package = "SHARK4R"); no test touches the network
# (test-shark-live.R holds the live checks).

app_root <- get_app_root()

source_shark <- function(env = parent.frame()) {
  # The UI and server call shiny, DT, leaflet and bs4Dash unqualified, as app.R
  # attaches them. Attach them only for the calling test.
  for (pkg in c("shiny", "DT", "leaflet", "bs4Dash")) withr::local_package(pkg, .local_envir = env)
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  source(file.path(app_root, "R/modules/shark_server.R"), local = FALSE)
  source(file.path(app_root, "R/ui/shark_ui.R"), local = FALSE)
}

# Assign `fn` to `nm` in globalenv for the calling test, restoring (or
# removing) whatever was there before.
local_global_mock <- function(nm, fn, env = parent.frame()) {
  had <- exists(nm, envir = globalenv(), inherits = FALSE)
  old <- if (had) get(nm, envir = globalenv()) else NULL
  assign(nm, fn, envir = globalenv())
  withr::defer(
    if (had) assign(nm, old, envir = globalenv()) else rm(list = nm, envir = globalenv()),
    envir = env
  )
}

html_of <- function(x) paste(as.character(htmltools::renderTags(x)$html), collapse = "\n")

# One match_worms_taxa() row, columns as returned by SHARK4R 1.2.0
# (live, 2026-09-29, "Gadus morhua"; trimmed to the columns the app reads).
worms_row <- function(name = "Gadus morhua") {
  tibble::tibble(
    name = name, AphiaID = 126436L, scientificname = "Gadus morhua", authority = "Linnaeus, 1758",
    status = "accepted", rank = "Species", kingdom = "Animalia", phylum = "Chordata",
    class = "Teleostei", order = "Gadiformes", family = "Gadidae", genus = "Gadus"
  )
}

# What match_worms_taxa() returns for a name WoRMS does not know ("Xx yy", live).
worms_no_content <- function(name = "Xx yy") {
  tibble::tibble(name = name, AphiaID = NA_integer_, scientificname = NA_character_,
                 status = "no content", rank = NA_character_)
}

# get_shark_data() rows (internal_key headers; values from the live
# Kattegat June 2024 query), dates spanning two years.
shark_rows <- function(dates = as.Date(c("2023-12-31", "2024-01-15", "2024-06-06", "2024-12-31", "2025-01-01"))) {
  n <- length(dates)
  tibble::tibble(
    delivery_datatype = "Physical and Chemical", station_name = "FLADEN", sample_date = dates,
    sample_latitude_dd = 57.19267, sample_longitude_dd = 11.658, sample_min_depth_m = 0,
    sample_max_depth_m = 0, scientific_name = NA_character_, parameter = "Temperature CTD",
    value = seq_len(n) + 10, unit = "C", quality_flag = NA_character_
  )
}

# ---------------------------------------------------------------------------
# The export list (spec rollout item 3)
# ---------------------------------------------------------------------------

shark4r_calls_in_app <- function() {
  files <- c(list.files(file.path(app_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
             file.path(app_root, "app.R"))
  files <- files[!grepl("safeBackup", basename(files), fixed = TRUE)]
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- code[!startsWith(trimws(code), "#")]
  unique(sub("^SHARK4R::", "", unlist(regmatches(code, gregexpr("SHARK4R::[A-Za-z0-9_.]+", code)))))
}

test_that("every SHARK4R:: call in the app is listed in SHARK4R_FUNCTIONS (F12)", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  used <- shark4r_calls_in_app()
  expect_true("get_shark_data" %in% used, info = "premise: the scan finds the SHARK calls")
  expect_equal(setdiff(used, SHARK4R_FUNCTIONS), character(0))
  expect_equal(setdiff(SHARK4R_FUNCTIONS, used), character(0))
})

test_that("SHARK4R exports every function the app calls (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  expect_equal(setdiff(SHARK4R_FUNCTIONS, getNamespaceExports("SHARK4R")), character(0))
  expect_true(shark4r_installed())
})

test_that("no SHARK handler reports failures with message() (F12)", {
  for (f in c("R/functions/shark_api_utils.R", "R/modules/shark_server.R")) {
    code <- readLines(file.path(app_root, f), warn = FALSE)
    code <- code[!startsWith(trimws(code), "#")]
    expect_false(any(grepl("\\bmessage\\(", code)), info = f)
  }
})

test_that("every QC data type is one SHARK4R::check_fields() knows", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  for (dt in SHARK_QC_DATATYPES) {
    translated <- SHARK4R::translate_shark_datatype(dt)
    expect_no_error(suppressWarnings(SHARK4R::check_fields(data.frame(x = 1), translated)))
  }
})

# ---------------------------------------------------------------------------
# Taxonomy
# ---------------------------------------------------------------------------

test_that("query_shark_worms reads a match_worms_taxa() row (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  local_mocked_bindings(match_worms_taxa = function(taxa_names, ...) {
    seen <<- list(taxa_names = taxa_names, ...)
    worms_row(taxa_names)
  }, .package = "SHARK4R")

  r <- query_shark_worms("Gadus morhua", fuzzy = FALSE, use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_identical(r$aphia_id, 126436L)
  expect_equal(r$scientific_name, "Gadus morhua")
  expect_equal(r$taxon_status, "accepted")
  expect_equal(r$class, "Teleostei")
  expect_equal(r$family, "Gadidae")
  expect_false(seen$fuzzy)
  expect_equal(seen$max_retries, 1)
  expect_equal(seen$sleep_time, 0)
  expect_false(seen$verbose)
})

test_that("a name WoRMS does not know is not_found, not a result (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(match_worms_taxa = function(taxa_names, ...) worms_no_content(taxa_names),
                        .package = "SHARK4R")
  expect_equal(query_shark_worms("Xx yy", use_cache = FALSE)$status, "not_found")
})

test_that("a failing WoRMS lookup warns and returns status error (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(match_worms_taxa = function(...) stop("HTTP 503"), .package = "SHARK4R")
  expect_warning(r <- query_shark_worms("Gadus morhua", use_cache = FALSE), "[shark] WoRMS lookup failed",
                 fixed = TRUE)
  expect_equal(r$status, "error")
  expect_equal(r$message, "HTTP 503")
})

test_that("only found WoRMS results are cached", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  dir <- withr::local_tempdir()
  calls <- 0
  local_mocked_bindings(match_worms_taxa = function(taxa_names, ...) {
    calls <<- calls + 1
    if (taxa_names == "Xx yy") worms_no_content(taxa_names) else worms_row(taxa_names)
  }, .package = "SHARK4R")

  query_shark_worms("Gadus morhua", cache_dir = dir)
  expect_equal(query_shark_worms("Gadus morhua", cache_dir = dir)$aphia_id, 126436L)
  expect_equal(calls, 1)
  query_shark_worms("Xx yy", cache_dir = dir)
  query_shark_worms("Xx yy", cache_dir = dir)
  expect_equal(calls, 3)
})

test_that("Dyntaxa without DYNTAXA_KEY is no_key and makes no call", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "")
  local_mocked_bindings(match_dyntaxa_taxa = function(...) stop("must not be called"), .package = "SHARK4R")
  r <- query_dyntaxa("torsk", use_cache = FALSE)
  expect_equal(r$status, "no_key")
  expect_match(r$message, "DYNTAXA_KEY", fixed = TRUE)
})

test_that("query_dyntaxa reads a match_dyntaxa_taxa() row; fuzzy maps to multiple_options", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "k")
  seen <- NULL
  local_mocked_bindings(match_dyntaxa_taxa = function(taxon_names, ...) {
    seen <<- list(...)
    # Columns as built by SHARK4R 1.2.0 match_dyntaxa_taxa() (source)
    tibble::tibble(search_pattern = taxon_names, taxon_id = 206199L, best_match = "torsk",
                   author = NA_character_, valid_name = "Gadus morhua")
  }, .package = "SHARK4R")

  r <- query_dyntaxa("torsk", fuzzy = TRUE, use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_equal(r$matched_name, "torsk")
  expect_equal(r$scientific_name, "Gadus morhua")
  expect_equal(r$taxon_id, "206199")
  expect_true(is.na(r$author))
  expect_equal(seen$subscription_key, "k")
  expect_false(seen$multiple_options)
  query_dyntaxa("torsk", fuzzy = FALSE, use_cache = FALSE)
  expect_true(seen$multiple_options)
})

test_that("a Dyntaxa row without a taxon_id is not_found", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "k")
  local_mocked_bindings(match_dyntaxa_taxa = function(taxon_names, ...) {
    tibble::tibble(search_pattern = taxon_names, taxon_id = NA, best_match = NA, author = NA, valid_name = NA)
  }, .package = "SHARK4R")
  expect_equal(query_dyntaxa("Xx yy", use_cache = FALSE)$status, "not_found")
})

test_that("query_algaebase splits the name and reads a match_algaebase_taxa() row", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(ALGAEBASE_KEY = "")
  expect_equal(query_algaebase("Skeletonema marinoi", use_cache = FALSE)$status, "no_key")

  withr::local_envvar(ALGAEBASE_KEY = "k")
  seen <- NULL
  local_mocked_bindings(match_algaebase_taxa = function(genera, species, ...) {
    seen <<- list(genera = genera, species = species, ...)
    # Columns of SHARK4R 1.2.0's AlgaeBase result (its error-row template)
    tibble::tibble(input_name = paste(genera, species), id = 12345L, phylum = "Bacillariophyta",
                   class = "Mediophyceae", taxonomic_status = "accepted",
                   accepted_name = "Skeletonema marinoi", authorship = "Sarno & Zingone")
  }, .package = "SHARK4R")
  r <- query_algaebase("Skeletonema marinoi", use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_equal(r$algaebase_id, "12345")
  expect_equal(r$scientific_name, "Skeletonema marinoi")
  expect_equal(r$authority, "Sarno & Zingone")
  expect_equal(seen$genera, "Skeletonema")
  expect_equal(seen$species, "marinoi")
  expect_equal(seen$subscription_key, "k")
})

test_that("the taxonomy sources offered follow the subscription keys", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  withr::local_envvar(DYNTAXA_KEY = "", ALGAEBASE_KEY = "")
  expect_equal(unname(shark_taxonomy_source_choices()), "worms")
  withr::local_envvar(DYNTAXA_KEY = "a", ALGAEBASE_KEY = "b")
  expect_equal(unname(shark_taxonomy_source_choices()), c("dyntaxa", "worms", "algaebase"))
})

# ---------------------------------------------------------------------------
# SHARK data
# ---------------------------------------------------------------------------

test_that("environmental queries call get_shark_data with years, bounds and exact parameters (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  local_mocked_bindings(get_shark_data = function(...) {
    seen <<- list(...)
    shark_rows()
  }, .package = "SHARK4R")

  r <- get_shark_environmental_data(
    parameters = c("Temperature CTD", "Salinity CTD"), start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"), bbox = c(north = 58, south = 57, east = 12, west = 11), max_records = 2
  )
  expect_equal(seen$dataTypes, "Physical and Chemical")
  expect_equal(seen$parameters, c("Temperature CTD", "Salinity CTD"))
  expect_identical(seen$fromYear, 2024L)
  expect_identical(seen$toYear, 2024L)
  expect_equal(seen$bounds, c(11, 57, 12, 58))
  expect_false(seen$verbose)
  # 2023-12-31 and 2025-01-01 fall outside the dates; 3 remain, 2 are shown
  expect_equal(r$status, "ok")
  expect_equal(r$total, 3L)
  expect_equal(nrow(r$data), 2L)
  expect_equal(r$message, "Showing the first 2 of 3 records")
})

test_that("a blank bounding box sends no bounds; a partial or inverted one is refused", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  calls <- 0
  seen <- NULL
  local_mocked_bindings(get_shark_data = function(...) {
    calls <<- calls + 1
    seen <<- list(...)
    shark_rows()
  }, .package = "SHARK4R")
  blank <- c(north = NA, south = NA, east = NA, west = NA)
  r <- get_shark_environmental_data("Temperature CTD", "2024-01-01", "2024-12-31", bbox = blank)
  expect_equal(r$status, "ok")
  expect_true("bounds" %in% names(seen))
  expect_null(seen$bounds)

  partial <- get_shark_environmental_data("Temperature CTD", "2024-01-01", "2024-12-31",
                                          bbox = c(north = 58, south = NA, east = NA, west = NA))
  expect_equal(partial$status, "error")
  expect_match(partial$message, "all four", fixed = TRUE)
  inverted <- get_shark_environmental_data("Temperature CTD", "2024-01-01", "2024-12-31",
                                           bbox = c(north = 57, south = 58, east = 12, west = 11))
  expect_equal(inverted$status, "error")
  expect_equal(calls, 1)
})

test_that("no parameters, a bad date range or no rows give a status, not an error", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(get_shark_data = function(...) shark_rows()[0, ], .package = "SHARK4R")
  expect_equal(get_shark_environmental_data(character(0), "2024-01-01", "2024-12-31")$status, "error")
  expect_equal(get_shark_environmental_data("pH", "2024-12-31", "2024-01-01")$message, "Invalid date range")
  empty <- get_shark_environmental_data("pH", "2024-01-01", "2024-12-31")
  expect_equal(empty$status, "empty")
  expect_null(empty$data)
})

test_that("a failing or timed-out SHARK query warns and returns status error (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(get_shark_data = function(...) stop("HTTP 500"), .package = "SHARK4R")
  expect_warning(r <- get_shark_environmental_data("pH", "2024-01-01", "2024-12-31"),
                 "[shark] environmental query failed", fixed = TRUE)
  expect_equal(r$status, "error")
  expect_match(r$message, "HTTP 500", fixed = TRUE)

  local_global_mock("with_timeout", function(expr, timeout = 10, on_timeout = NULL, verbose = FALSE) on_timeout)
  expect_warning(t <- get_shark_species_occurrence("Macoma balthica", "2022-01-01", "2022-12-31"),
                 "timed out", fixed = TRUE)
  expect_equal(t$status, "error")
})

test_that("occurrence queries filter on the taxon name", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  local_mocked_bindings(get_shark_data = function(...) {
    seen <<- list(...)
    shark_rows(as.Date("2022-06-13"))
  }, .package = "SHARK4R")
  r <- get_shark_species_occurrence("  Macoma balthica ", "2022-01-01", "2022-12-31")
  expect_equal(seen$taxonName, "Macoma balthica")
  expect_null(seen$dataTypes)
  expect_equal(r$status, "ok")
  expect_equal(get_shark_species_occurrence("  ", "2022-01-01", "2022-12-31")$status, "error")
})

test_that("format_shark_results maps SHARK columns and fills missing ones with NA", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  env <- format_shark_results(as.data.frame(shark_rows()), "environmental")
  expect_equal(names(env), c("Date", "Station", "Lat", "Lon", "Depth (m)", "Parameter", "Value", "Unit",
                             "Quality flag"))
  expect_equal(env$Parameter[1], "Temperature CTD")
  occ <- format_shark_results(data.frame(scientific_name = "Macoma balthica", value = 5), "occurrence")
  expect_equal(occ$Species, "Macoma balthica")
  expect_true(is.na(occ$Station))
  expect_equal(format_shark_results(NULL, "occurrence")$Message, "No data to display")
})

# ---------------------------------------------------------------------------
# Quality control
# ---------------------------------------------------------------------------

test_that("format validation summarises SHARK4R::check_fields() issues (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  local_mocked_bindings(check_fields = function(data, datatype, ...) {
    seen <<- datatype
    tibble::tibble(level = c("error", "error", "warning"), field = c("a", "a", "b"), row = NA_integer_,
                   message = c("Required field a is missing", "Required field a is missing", "b is empty"))
  }, .package = "SHARK4R")
  v <- validate_shark_data(data.frame(x = 1), "Physical and Chemical")
  expect_equal(seen, "PhysicalChemical")
  expect_false(v$valid)
  expect_equal(v$n_errors, 2L)
  expect_equal(v$errors, "Required field a is missing")
  expect_equal(v$warnings, "b is empty")
  expect_false(validate_shark_data(data.frame(x = 1), NA_character_)$valid)
})

test_that("the QC data type comes from the UI or a single delivery_datatype value", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  one <- data.frame(delivery_datatype = c("Zoobenthos", "Zoobenthos"))
  mixed <- data.frame(delivery_datatype = c("Zoobenthos", "Zooplankton"))
  expect_equal(resolve_shark_qc_datatype(one, "auto"), "Zoobenthos")
  expect_true(is.na(resolve_shark_qc_datatype(mixed, "auto")))
  expect_true(is.na(resolve_shark_qc_datatype(data.frame(x = 1), "auto")))
  expect_equal(resolve_shark_qc_datatype(mixed, "Zooplankton"), "Zooplankton")
  expect_true(is.na(resolve_shark_qc_datatype(one, "Not a type")))
})

test_that("outliers use SHARK4R thresholds; parameters without one are listed, not warned", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  # Real check_outliers(): its thresholds ship with SHARK4R (offline).
  d <- data.frame(station_name = "S1", sample_date = as.Date("2022-06-01"),
                  parameter = c("Abundance", "Abundance", "No such parameter"),
                  value = c(1, 1e12, 5), stringsAsFactors = FALSE)
  expect_no_warning(o <- check_shark_outliers(d, "Zoobenthos"))
  expect_equal(o$checked, "Abundance")
  expect_equal(o$unchecked, "No such parameter")
  expect_equal(nrow(o$outliers), 1L)
  expect_equal(o$outliers$value, 1e12)
  expect_match(check_shark_outliers(data.frame(x = 1), "Zoobenthos")$message, "long format", fixed = TRUE)
})

test_that("coordinate checks count zero, out-of-range and missing positions", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  d <- data.frame(sample_latitude_dd = c(57, 0, 95, NA), sample_longitude_dd = c(11, 11, 11, 11))
  k <- check_shark_coordinates(d)
  expect_equal(k$zero, 1L)
  expect_equal(k$out_of_range, 1L)
  expect_equal(k$missing, 1L)
  expect_true(is.na(check_shark_coordinates(data.frame(x = 1))$zero))
})

test_that("QC files are read by extension, and an unreadable file warns", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  dir <- withr::local_tempdir()
  tsv <- file.path(dir, "a.txt")
  writeLines(c("station_name\tvalue", "S1\t2"), tsv)
  csv <- file.path(dir, "a.csv")
  writeLines(c("station_name,value", "S1,2"), csv)
  expect_equal(read_shark_qc_file(tsv, "a.txt")$value, 2)
  expect_equal(read_shark_qc_file(csv, "a.csv")$value, 2)
  expect_warning(bad <- read_shark_qc_file(file.path(dir, "missing.csv"), "missing.csv"),
                 "[shark] could not read QC file", fixed = TRUE)
  expect_null(bad)
})

test_that("run_shark_qc runs only the selected checks, and the report lists them", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(check_fields = function(...) {
    tibble::tibble(level = "error", field = paste0("f", 1:25), row = NA_integer_,
                   message = paste("Required field", paste0("f", 1:25), "is missing"))
  }, .package = "SHARK4R")
  d <- data.frame(delivery_datatype = "Zoobenthos", sample_latitude_dd = 57, sample_longitude_dd = 11)
  qc <- run_shark_qc(d, c("format", "coordinates"), resolve_shark_qc_datatype(d))
  expect_null(qc$quality)
  expect_null(qc$outliers)
  expect_equal(qc$validation$n_errors, 25L)
  report <- format_shark_qc_report(qc)
  expect_true("Result: FAILED" %in% report)
  expect_true(" ... and 5 more" %in% report)
  expect_false(any(grepl("COMPLETENESS", report, fixed = TRUE)))
  expect_true("COORDINATES" %in% report)
})
```

The mocked rows use the column names that SHARK4R 1.2.0 returned live on 2026-09-29 (`match_worms_taxa`, `get_shark_data` with `headerLang = "internal_key"`). For Dyntaxa and AlgaeBase no key was available, so their mocked rows use the columns built in the 1.2.0 source. The outlier test calls the real `check_outliers()`, which is offline: its thresholds ship with the package.

- [ ] **Step 2: Run the tests to verify they fail**

Run the one-file command for `test-shark-rewire.R`.

Expected: `tests 25 failing 25 skipped 0 passed 5` (verified on master). The failures are "object 'SHARK4R_FUNCTIONS' not found", "could not find function ..." and wrong results from the old wrappers.

- [ ] **Step 3: Rewrite `R/functions/shark_api_utils.R`**

Replace the whole file with exactly:

```r
# ==============================================================================
# SHARK4R API UTILITIES
# ==============================================================================
# Wrappers for the SHARK Data tab around SHARK4R (>= 1.2.0):
# - taxonomy: WoRMS (no key), Dyntaxa (DYNTAXA_KEY), AlgaeBase (ALGAEBASE_KEY)
# - data: SHARK physical-chemical and biological records (get_shark_data)
# - quality control: SHARK field and outlier checks, plus local checks
#
# SHARK4R 1.2.0 removed every sharkdata_* function the tab used to call. The
# functions called here are listed in SHARK4R_FUNCTIONS; test-shark-rewire.R
# fails if a SHARK4R:: call is not listed or not exported.
#
# Documentation: https://sharksmhi.github.io/SHARK4R/
# ==============================================================================

SHARK4R_MIN_VERSION <- "1.2.0"

# Every SHARK4R function the app calls (here and in database_lookups.R).
SHARK4R_FUNCTIONS <- c(
  "match_worms_taxa", "match_dyntaxa_taxa", "parse_scientific_names",
  "match_algaebase_taxa", "get_shark_data", "translate_shark_datatype",
  "check_fields", "check_outliers"
)

# SHARK answers slowly (25 s for 96 rows, 43 s for an empty taxon query).
SHARK_QUERY_TIMEOUT_S <- 90

# Exact SHARK parameter names (get_shark_options()$parameters, 2026-09-29).
SHARK_ENV_PARAMETERS <- c(
  "Temperature (CTD)" = "Temperature CTD",
  "Salinity (CTD)" = "Salinity CTD",
  "Dissolved oxygen (CTD)" = "Dissolved oxygen O2 CTD",
  "pH" = "pH",
  "Phosphate" = "Phosphate PO4-P",
  "Nitrate" = "Nitrate NO3-N",
  "Chlorophyll-a" = "Chlorophyll-a",
  "Secchi depth" = "Secchi depth"
)

# SHARK data types that SHARK4R::check_fields() knows (after
# translate_shark_datatype()), in SHARK's own spelling.
SHARK_QC_DATATYPES <- c(
  "Bacterioplankton", "Chlorophyll", "Epibenthos", "Grey seal", "Harbour Porpoise",
  "Harbour seal", "Physical and Chemical", "Phytoplankton", "Picoplankton",
  "Primary production", "Ringed seal", "Seal pathology", "Sedimentation",
  "Zoobenthos", "Zooplankton"
)

SHARK4R_MISSING_MESSAGE <- "SHARK4R 1.2.0 or newer is not installed"

#' Is a usable SHARK4R installed? (does not load the package)
#'
#' Reads the installed DESCRIPTION only, so the UI can call it at start-up
#' without loading SHARK4R and its sf/terra dependencies.
#' @return Logical.
shark4r_installed <- function() {
  if (!nzchar(system.file(package = "SHARK4R"))) return(FALSE)
  isTRUE(utils::packageVersion("SHARK4R") >= SHARK4R_MIN_VERSION)
}

#' Check that SHARK4R is installed and exports every function the app calls
#'
#' @return Logical. Warns when the installed SHARK4R lacks functions from
#'   SHARK4R_FUNCTIONS.
check_shark4r_available <- function() {
  if (!shark4r_installed()) return(FALSE)
  missing <- setdiff(SHARK4R_FUNCTIONS, getNamespaceExports("SHARK4R"))
  if (length(missing) > 0) {
    warning(sprintf("[shark] SHARK4R %s lacks: %s", as.character(utils::packageVersion("SHARK4R")),
                    paste(missing, collapse = ", ")), call. = FALSE)
    return(FALSE)
  }
  TRUE
}

#' Subscription key for a key-gated taxonomy source
#'
#' @param source "dyntaxa" or "algaebase".
#' @return The key, or NA when the environment variable is unset or empty.
shark_subscription_key <- function(source) {
  var <- switch(source, dyntaxa = "DYNTAXA_KEY", algaebase = "ALGAEBASE_KEY",
                stop("unknown taxonomy source: ", source))
  key <- Sys.getenv(var, "")
  if (nzchar(key)) key else NA_character_
}

#' Taxonomy sources the SHARK tab can query on this server
#'
#' WoRMS needs no key. Dyntaxa and AlgaeBase are offered only when their
#' subscription key is set (DYNTAXA_KEY / ALGAEBASE_KEY).
#' @return Named character vector for checkboxGroupInput(choices = ).
shark_taxonomy_source_choices <- function() {
  choices <- c("WoRMS (World Register)" = "worms")
  if (!is.na(shark_subscription_key("dyntaxa"))) {
    choices <- c("Dyntaxa (Swedish Taxonomy)" = "dyntaxa", choices)
  }
  if (!is.na(shark_subscription_key("algaebase"))) {
    choices <- c(choices, "AlgaeBase (Algae Database)" = "algaebase")
  }
  choices
}

# ==============================================================================
# CACHE (successful taxonomy lookups only, 30 days)
# ==============================================================================

.shark_cache_file <- function(cache_dir, prefix, name) {
  file.path(cache_dir, paste0(prefix, "_", gsub("[^a-zA-Z0-9]", "_", name), ".rds"))
}

.shark_cache_read <- function(file) {
  if (!file.exists(file)) return(NULL)
  cached <- tryCatch(readRDS(file), error = function(e) NULL)
  if (!is.list(cached) || !is.list(cached$data) || !identical(cached$data$status, "found")) return(NULL)
  if (difftime(Sys.time(), cached$timestamp, units = "days") >= 30) return(NULL)
  cached$data
}

.shark_cache_write <- function(file, data) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(data = data, timestamp = Sys.time()), file)
}

# ==============================================================================
# TAXONOMY FUNCTIONS
# ==============================================================================
# Each returns list(source, query, status, message, ...). status is "found",
# "not_found", "no_key" or "error"; the other fields are set when "found".

#' Query WoRMS via SHARK4R::match_worms_taxa()
#'
#' @param species_name Character, species name.
#' @param fuzzy Logical, fuzzy WoRMS name matching.
#' @param use_cache Logical, read and write the 30-day cache.
#' @param cache_dir Character, cache directory.
#' @return Taxonomy result list (see above) with scientific_name, aphia_id,
#'   authority, taxon_status, kingdom, phylum, class, order, family, genus, rank.
query_shark_worms <- function(species_name, fuzzy = TRUE, use_cache = TRUE,
                              cache_dir = app_path("cache", "shark")) {
  base <- list(source = "WoRMS (SHARK4R)", query = species_name, status = "error")
  if (!check_shark4r_available()) return(utils::modifyList(base, list(message = SHARK4R_MISSING_MESSAGE)))

  cache_file <- .shark_cache_file(cache_dir, "shark_worms", species_name)
  if (use_cache) {
    hit <- .shark_cache_read(cache_file)
    if (!is.null(hit)) return(hit)
  }

  failure <- NULL
  res <- tryCatch(
    SHARK4R::match_worms_taxa(species_name, fuzzy = isTRUE(fuzzy), best_match_only = TRUE,
                              max_retries = 1, sleep_time = 0, verbose = FALSE),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] WoRMS lookup failed for '%s': %s", species_name, failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(utils::modifyList(base, list(message = failure)))

  aphia_id <- if (NROW(res) > 0) suppressWarnings(as.integer(.scalar_chr(res$AphiaID))) else NA_integer_
  if (is.na(aphia_id)) return(utils::modifyList(base, list(status = "not_found")))

  result <- utils::modifyList(base, list(
    status = "found",
    scientific_name = .scalar_chr(res$scientificname),
    aphia_id = aphia_id,
    authority = .scalar_chr(res$authority),
    taxon_status = .scalar_chr(res$status),
    kingdom = .scalar_chr(res$kingdom),
    phylum = .scalar_chr(res$phylum),
    class = .scalar_chr(res$class),
    order = .scalar_chr(res$order),
    family = .scalar_chr(res$family),
    genus = .scalar_chr(res$genus),
    rank = .scalar_chr(res$rank)
  ))
  if (use_cache) .shark_cache_write(cache_file, result)
  result
}

#' Query Dyntaxa (Swedish taxonomy) via SHARK4R::match_dyntaxa_taxa()
#'
#' Needs DYNTAXA_KEY. Searches scientific and Swedish names. With
#' fuzzy = FALSE only a name equal to the query (ignoring case) counts.
#' @inheritParams query_shark_worms
#' @return Taxonomy result list with matched_name, scientific_name (the
#'   recommended name), taxon_id, author.
query_dyntaxa <- function(species_name, fuzzy = TRUE, use_cache = TRUE,
                          cache_dir = app_path("cache", "shark")) {
  base <- list(source = "Dyntaxa", query = species_name, status = "error")
  if (!check_shark4r_available()) return(utils::modifyList(base, list(message = SHARK4R_MISSING_MESSAGE)))
  key <- shark_subscription_key("dyntaxa")
  if (is.na(key)) {
    return(utils::modifyList(base, list(
      status = "no_key", message = "Dyntaxa needs a subscription key in the DYNTAXA_KEY environment variable"
    )))
  }

  cache_file <- .shark_cache_file(cache_dir, "dyntaxa", species_name)
  if (use_cache) {
    hit <- .shark_cache_read(cache_file)
    if (!is.null(hit)) return(hit)
  }

  failure <- NULL
  res <- tryCatch(
    SHARK4R::match_dyntaxa_taxa(species_name, subscription_key = key,
                                multiple_options = !isTRUE(fuzzy), verbose = FALSE),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] Dyntaxa lookup failed for '%s': %s", species_name, failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(utils::modifyList(base, list(message = failure)))

  taxon_id <- if (NROW(res) > 0) .scalar_chr(res$taxon_id) else NA_character_
  if (is.na(taxon_id)) return(utils::modifyList(base, list(status = "not_found")))

  result <- utils::modifyList(base, list(
    status = "found",
    matched_name = .scalar_chr(res$best_match),
    scientific_name = .scalar_chr(res$valid_name),
    taxon_id = taxon_id,
    author = .scalar_chr(res$author)
  ))
  if (use_cache) .shark_cache_write(cache_file, result)
  result
}

#' Query AlgaeBase via SHARK4R::match_algaebase_taxa()
#'
#' Needs ALGAEBASE_KEY (an AlgaeBase API subscription key; the Trait Research
#' AlgaeBase username/password is a different credential).
#' @inheritParams query_shark_worms
#' @return Taxonomy result list with scientific_name, algaebase_id, authority,
#'   taxon_status, phylum, class.
query_algaebase <- function(species_name, use_cache = TRUE, cache_dir = app_path("cache", "shark")) {
  base <- list(source = "AlgaeBase", query = species_name, status = "error")
  if (!check_shark4r_available()) return(utils::modifyList(base, list(message = SHARK4R_MISSING_MESSAGE)))
  key <- shark_subscription_key("algaebase")
  if (is.na(key)) {
    return(utils::modifyList(base, list(
      status = "no_key", message = "AlgaeBase needs a subscription key in the ALGAEBASE_KEY environment variable"
    )))
  }

  cache_file <- .shark_cache_file(cache_dir, "algaebase", species_name)
  if (use_cache) {
    hit <- .shark_cache_read(cache_file)
    if (!is.null(hit)) return(hit)
  }

  failure <- NULL
  res <- tryCatch({
    parsed <- SHARK4R::parse_scientific_names(species_name)
    SHARK4R::match_algaebase_taxa(genera = parsed$genus, species = parsed$species,
                                  subscription_key = key, sleep_time = 0, verbose = FALSE)
  }, error = function(e) {
    failure <<- conditionMessage(e)
    warning(sprintf("[shark] AlgaeBase lookup failed for '%s': %s", species_name, failure), call. = FALSE)
    NULL
  })
  if (!is.null(failure)) return(utils::modifyList(base, list(message = failure)))

  algaebase_id <- if (NROW(res) > 0) .scalar_chr(res$id) else NA_character_
  if (is.na(algaebase_id)) return(utils::modifyList(base, list(status = "not_found")))

  name <- .scalar_chr(res$accepted_name)
  if (is.na(name)) name <- .scalar_chr(res$input_name)
  result <- utils::modifyList(base, list(
    status = "found",
    scientific_name = name,
    algaebase_id = algaebase_id,
    authority = .scalar_chr(res$authorship),
    taxon_status = .scalar_chr(res$taxonomic_status),
    phylum = .scalar_chr(res$phylum),
    class = .scalar_chr(res$class)
  ))
  if (use_cache) .shark_cache_write(cache_file, result)
  result
}

# ==============================================================================
# DATA RETRIEVAL FUNCTIONS
# ==============================================================================
# Both return list(status, data, message, total). status is "ok" (data holds
# at most max_records rows), "empty" or "error"; data is NULL unless "ok".

# bbox (named north/south/east/west) -> SHARK bounds c(lon_min, lat_min,
# lon_max, lat_max). All four blank -> NULL (no spatial filter).
.shark_bounds <- function(bbox) {
  if (is.null(bbox)) return(list(bounds = NULL))
  v <- suppressWarnings(as.numeric(bbox[c("west", "south", "east", "north")]))
  if (all(is.na(v))) return(list(bounds = NULL))
  if (!all(is.finite(v))) return(list(error = "Fill in all four bounding-box fields, or leave all four blank"))
  if (v[1] >= v[3] || v[2] >= v[4]) {
    return(list(error = "Bounding box: West must be less than East and South less than North"))
  }
  list(bounds = v)
}

.shark_fetch <- function(label, ..., start_date, end_date, bbox, max_records) {
  fail <- function(msg) list(status = "error", data = NULL, message = msg, total = 0L)
  if (!check_shark4r_available()) return(fail(SHARK4R_MISSING_MESSAGE))

  start <- tryCatch(as.Date(start_date), error = function(e) as.Date(NA))
  end <- tryCatch(as.Date(end_date), error = function(e) as.Date(NA))
  if (length(start) != 1 || length(end) != 1 || is.na(start) || is.na(end) || start > end) {
    return(fail("Invalid date range"))
  }
  box <- .shark_bounds(bbox)
  if (!is.null(box$error)) return(fail(box$error))
  max_records <- suppressWarnings(as.integer(max_records))
  if (length(max_records) != 1 || is.na(max_records) || max_records < 1) max_records <- 5000L

  timed_out <- structure(list(), class = "shark_timeout")
  failure <- NULL
  raw <- tryCatch(
    with_timeout(
      SHARK4R::get_shark_data(..., fromYear = as.integer(format(start, "%Y")),
                              toYear = as.integer(format(end, "%Y")), bounds = box$bounds,
                              verbose = FALSE),
      timeout = SHARK_QUERY_TIMEOUT_S, on_timeout = timed_out
    ),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] %s query failed: %s", label, failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(fail(paste("SHARK query failed:", failure)))
  if (inherits(raw, "shark_timeout")) {
    warning(sprintf("[shark] %s query timed out after %d s", label, SHARK_QUERY_TIMEOUT_S), call. = FALSE)
    return(fail(sprintf("SHARK did not answer within %d s; narrow the query", SHARK_QUERY_TIMEOUT_S)))
  }

  if (NROW(raw) > 0 && "sample_date" %in% names(raw)) {
    dates <- suppressWarnings(as.Date(raw$sample_date))
    raw <- raw[!is.na(dates) & dates >= start & dates <= end, , drop = FALSE]
  }
  total <- NROW(raw)
  if (total == 0) {
    return(list(status = "empty", data = NULL, message = "No records match the query", total = 0L))
  }
  shown <- min(total, max_records)
  note <- if (shown < total) {
    sprintf("Showing the first %d of %d records", shown, total)
  } else {
    sprintf("Retrieved %d records", total)
  }
  list(status = "ok", data = as.data.frame(raw[seq_len(shown), , drop = FALSE]), message = note,
       total = total)
}

#' Get SHARK physical-chemical data
#'
#' @param parameters Character vector of exact SHARK parameter names
#'   (see SHARK_ENV_PARAMETERS).
#' @param start_date,end_date Date or "YYYY-MM-DD".
#' @param bbox Named numeric c(north, south, east, west), or NULL / all NA.
#' @param max_records Maximum rows kept.
#' @return list(status, data, message, total); see above.
get_shark_environmental_data <- function(parameters, start_date, end_date, bbox = NULL, max_records = 10000) {
  parameters <- as.character(parameters[!is.na(parameters) & nzchar(parameters)])
  if (length(parameters) == 0) {
    return(list(status = "error", data = NULL, message = "Choose at least one parameter", total = 0L))
  }
  .shark_fetch("environmental", dataTypes = "Physical and Chemical", parameters = parameters,
               start_date = start_date, end_date = end_date, bbox = bbox, max_records = max_records)
}

#' Get SHARK records for one taxon (any biological data type)
#'
#' @param species_name Exact scientific name as used in SHARK.
#' @inheritParams get_shark_environmental_data
#' @return list(status, data, message, total); see above.
get_shark_species_occurrence <- function(species_name, start_date, end_date, bbox = NULL, max_records = 5000) {
  species_name <- trimws(.scalar_chr(species_name))
  if (is.na(species_name) || !nzchar(species_name)) {
    return(list(status = "error", data = NULL, message = "Enter a scientific name", total = 0L))
  }
  .shark_fetch("occurrence", taxonName = species_name,
               start_date = start_date, end_date = end_date, bbox = bbox, max_records = max_records)
}

# Display column -> SHARK column (get_shark_data(), headerLang = "internal_key").
SHARK_DISPLAY_COLUMNS <- list(
  environmental = c(
    "Date" = "sample_date", "Station" = "station_name", "Lat" = "sample_latitude_dd",
    "Lon" = "sample_longitude_dd", "Depth (m)" = "sample_min_depth_m", "Parameter" = "parameter",
    "Value" = "value", "Unit" = "unit", "Quality flag" = "quality_flag"
  ),
  occurrence = c(
    "Date" = "sample_date", "Species" = "scientific_name", "Data type" = "delivery_datatype",
    "Station" = "station_name", "Lat" = "sample_latitude_dd", "Lon" = "sample_longitude_dd",
    "Parameter" = "parameter", "Value" = "value", "Unit" = "unit"
  )
)

#' Format SHARK records for display
#'
#' @param raw_data Data frame from get_shark_data().
#' @param result_type "environmental" or "occurrence".
#' @return Data frame with the display columns; a column SHARK did not
#'   return is NA.
format_shark_results <- function(raw_data, result_type = c("environmental", "occurrence")) {
  result_type <- match.arg(result_type)
  if (is.null(raw_data) || NROW(raw_data) == 0) {
    return(data.frame(Message = "No data to display"))
  }
  cols <- SHARK_DISPLAY_COLUMNS[[result_type]]
  out <- lapply(cols, function(col) if (col %in% names(raw_data)) raw_data[[col]] else rep(NA, NROW(raw_data)))
  data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

# ==============================================================================
# QUALITY CONTROL FUNCTIONS
# ==============================================================================

#' Read an uploaded QC file (.txt/.tsv tab-separated, otherwise CSV)
#'
#' @param path File path.
#' @param name Original file name (decides the delimiter).
#' @return Data frame, or NULL with a warning when the file cannot be read.
read_shark_qc_file <- function(path, name = path) {
  ext <- tolower(tools::file_ext(name))
  tryCatch({
    if (ext %in% c("txt", "tsv")) {
      utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(e) {
    warning(sprintf("[shark] could not read QC file '%s': %s", name, conditionMessage(e)), call. = FALSE)
    NULL
  })
}

#' SHARK data type for the QC checks
#'
#' @param data_frame Uploaded data.
#' @param selected The UI choice: "auto" or one of SHARK_QC_DATATYPES.
#' @return A SHARK_QC_DATATYPES value, or NA when "auto" cannot decide (no
#'   delivery_datatype column, or not exactly one known value in it).
resolve_shark_qc_datatype <- function(data_frame, selected = "auto") {
  if (!identical(selected, "auto")) {
    return(if (isTRUE(selected %in% SHARK_QC_DATATYPES)) selected else NA_character_)
  }
  if (!"delivery_datatype" %in% names(data_frame)) return(NA_character_)
  values <- unique(stats::na.omit(as.character(data_frame$delivery_datatype)))
  if (length(values) == 1 && values %in% SHARK_QC_DATATYPES) values else NA_character_
}

SHARK_QC_NO_DATATYPE <- "Choose the data type: the file has no single known delivery_datatype value"

#' Validate SHARK format with SHARK4R::check_fields()
#'
#' @param data_frame Data frame to validate.
#' @param datatype One of SHARK_QC_DATATYPES, or NA.
#' @return list(valid, message, errors, warnings, n_errors, n_warnings);
#'   errors/warnings are the unique messages.
validate_shark_data <- function(data_frame, datatype) {
  result <- function(valid, message, errors = character(0), warnings = character(0), n_errors = 0L,
                     n_warnings = 0L) {
    list(valid = valid, message = message, errors = errors, warnings = warnings,
         n_errors = n_errors, n_warnings = n_warnings)
  }
  if (!check_shark4r_available()) return(result(FALSE, SHARK4R_MISSING_MESSAGE))
  if (is.na(datatype)) return(result(FALSE, SHARK_QC_NO_DATATYPE))

  failure <- NULL
  issues <- tryCatch(
    SHARK4R::check_fields(data_frame, SHARK4R::translate_shark_datatype(datatype), level = "error"),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] format validation failed: %s", failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(result(FALSE, paste("Validation error:", failure)))

  levels <- if (NROW(issues) > 0) as.character(issues$level) else character(0)
  messages <- if (NROW(issues) > 0) as.character(issues$message) else character(0)
  is_error <- levels == "error"
  n_errors <- sum(is_error)
  n_warnings <- sum(!is_error)
  result(n_errors == 0,
         sprintf("%s: %d errors, %d warnings", datatype, n_errors, n_warnings),
         unique(messages[is_error]), unique(messages[!is_error]), n_errors, n_warnings)
}

#' Data completeness (local; no SHARK4R)
#'
#' @param data_frame Data frame to check.
#' @return list(completeness = percent of rows with no missing value,
#'   missing_values, record_count, column_count, message).
check_data_quality <- function(data_frame) {
  n <- NROW(data_frame)
  list(
    completeness = if (n > 0) sum(stats::complete.cases(data_frame)) / n * 100 else NA_real_,
    missing_values = colSums(is.na(data_frame)),
    record_count = n,
    column_count = NCOL(data_frame),
    message = "Completeness is the share of rows with no missing value"
  )
}

#' Outliers against SHARK4R's bundled thresholds (SHARK4R::check_outliers())
#'
#' Checks each parameter in the file. A parameter without a SHARK threshold
#' for the data type is listed in `unchecked`, not warned about.
#' @param data_frame Data in SHARK long format (parameter, value columns).
#' @param datatype One of SHARK_QC_DATATYPES, or NA.
#' @return list(message, outliers (data frame or NULL), checked, unchecked).
check_shark_outliers <- function(data_frame, datatype) {
  result <- function(message, outliers = NULL, checked = character(0), unchecked = character(0)) {
    list(message = message, outliers = outliers, checked = checked, unchecked = unchecked)
  }
  if (!check_shark4r_available()) return(result(SHARK4R_MISSING_MESSAGE))
  if (is.na(datatype)) return(result(SHARK_QC_NO_DATATYPE))
  if (!all(c("parameter", "value") %in% names(data_frame))) {
    return(result("Outlier check needs 'parameter' and 'value' columns (SHARK long format)"))
  }

  d <- data_frame
  d$delivery_datatype <- datatype
  d$value <- suppressWarnings(as.numeric(d$value))
  checked <- character(0)
  unchecked <- character(0)
  found <- list()
  for (p in unique(stats::na.omit(as.character(d$parameter)))) {
    no_threshold <- FALSE
    failed <- FALSE
    res <- tryCatch(
      withCallingHandlers(
        SHARK4R::check_outliers(d, parameter = p, datatype = datatype, return_df = TRUE, verbose = FALSE),
        warning = function(w) {
          if (grepl("No thresholds found", conditionMessage(w), fixed = TRUE)) {
            no_threshold <<- TRUE
            invokeRestart("muffleWarning")
          }
        }
      ),
      error = function(e) {
        failed <<- TRUE
        warning(sprintf("[shark] outlier check failed for '%s': %s", p, conditionMessage(e)), call. = FALSE)
        NULL
      }
    )
    if (failed || no_threshold) {
      unchecked <- c(unchecked, p)
      next
    }
    checked <- c(checked, p)
    if (NROW(res) > 0) found[[p]] <- as.data.frame(res)
  }
  outliers <- if (length(found) > 0) do.call(rbind, unname(found)) else NULL
  result(sprintf("%d outlier values in %d checked parameters; %d parameters have no SHARK threshold",
                 NROW(outliers), length(checked), length(unchecked)),
         outliers, checked, unchecked)
}

#' Coordinate checks (local; no SHARK4R)
#'
#' @param data_frame Data with sample_latitude_dd / sample_longitude_dd.
#' @return list(message, zero, out_of_range, missing) counts of rows.
check_shark_coordinates <- function(data_frame) {
  cols <- c("sample_latitude_dd", "sample_longitude_dd")
  if (!all(cols %in% names(data_frame))) {
    return(list(message = "No sample_latitude_dd / sample_longitude_dd columns",
                zero = NA_integer_, out_of_range = NA_integer_, missing = NA_integer_))
  }
  lat <- suppressWarnings(as.numeric(as.character(data_frame$sample_latitude_dd)))
  lon <- suppressWarnings(as.numeric(as.character(data_frame$sample_longitude_dd)))
  zero <- sum(lat %in% 0 | lon %in% 0)
  out_of_range <- sum((!is.na(lat) & abs(lat) > 90) | (!is.na(lon) & abs(lon) > 180))
  missing <- sum(is.na(lat) | is.na(lon))
  list(message = sprintf("%d rows with a zero coordinate, %d out of range, %d missing",
                         zero, out_of_range, missing),
       zero = zero, out_of_range = out_of_range, missing = missing)
}

#' Run the selected QC checks
#'
#' @param data_frame Uploaded data.
#' @param checks Subset of c("format", "completeness", "outliers", "coordinates").
#' @param datatype One of SHARK_QC_DATATYPES, or NA.
#' @return list(error = FALSE, datatype, validation, quality, outliers,
#'   coordinates, data_summary); an unselected check is NULL.
run_shark_qc <- function(data_frame, checks, datatype) {
  list(
    error = FALSE,
    datatype = datatype,
    validation = if ("format" %in% checks) validate_shark_data(data_frame, datatype),
    quality = if ("completeness" %in% checks) check_data_quality(data_frame),
    outliers = if ("outliers" %in% checks) check_shark_outliers(data_frame, datatype),
    coordinates = if ("coordinates" %in% checks) check_shark_coordinates(data_frame),
    data_summary = list(rows = NROW(data_frame), columns = NCOL(data_frame), column_names = colnames(data_frame))
  )
}

#' Plain-text QC report
#'
#' @param qc Result of run_shark_qc().
#' @param max_items Maximum validation messages listed.
#' @return Character vector of report lines.
format_shark_qc_report <- function(qc, max_items = 20L) {
  rule <- strrep("=", 80)
  lines <- c(rule, "SHARK DATA QUALITY CONTROL REPORT", rule, "",
             "DATA SUMMARY", "------------",
             paste("Rows:", qc$data_summary$rows),
             paste("Columns:", qc$data_summary$columns),
             paste("Data type:", if (is.na(qc$datatype)) "unknown" else qc$datatype), "")
  v <- qc$validation
  if (!is.null(v)) {
    lines <- c(lines, "FORMAT VALIDATION (SHARK4R::check_fields)", "-----------------------------------------",
               paste("Result:", if (isTRUE(v$valid)) "PASSED" else "FAILED"), v$message)
    shown <- utils::head(v$errors, max_items)
    if (length(shown) > 0) lines <- c(lines, paste(" -", shown))
    if (length(v$errors) > length(shown)) {
      lines <- c(lines, sprintf(" ... and %d more", length(v$errors) - length(shown)))
    }
    lines <- c(lines, "")
  }
  q <- qc$quality
  if (!is.null(q)) {
    completeness <- if (is.na(q$completeness)) "n/a" else sprintf("%.1f%%", q$completeness)
    lines <- c(lines, "COMPLETENESS", "------------", paste("Data completeness:", completeness),
               paste("Record count:", q$record_count), "")
  }
  if (!is.null(qc$outliers)) {
    lines <- c(lines, "OUTLIERS (SHARK4R::check_outliers)", "----------------------------------",
               qc$outliers$message, "")
  }
  if (!is.null(qc$coordinates)) {
    lines <- c(lines, "COORDINATES", "-----------", qc$coordinates$message, "")
  }
  c(lines, rule)
}

# ==============================================================================
# END OF SHARK4R API UTILITIES
# ==============================================================================
```

- [ ] **Step 4: Drop the removed wrappers' tests from `test-p0p1-fixes.R`**

In `tests/testthat/test-p0p1-fixes.R`, replace

```r
source(file.path(app_root, "R/modules/data_import_server.R"))
source(file.path(app_root, "R/functions/shark_api_utils.R"))
```

with

```r
source(file.path(app_root, "R/modules/data_import_server.R"))
```

Then delete these two tests, together with the blank line after each. They cover `get_available_shark_parameters()` and `get_shark_datasets()`, which Step 3 removed (deviation 4). They were also empty tests on any machine with SHARK4R (`if (precondition) expect_*()`):

```r
test_that("get_available_shark_parameters returns empty on error, not fake data", {
  result <- get_available_shark_parameters()
  if (!requireNamespace("SHARK4R", quietly = TRUE)) {
    expect_equal(length(result), 0,
                 info = "Should return empty vector when SHARK4R not installed")
  }
})

test_that("get_shark_datasets returns info df on error, not fake data", {
  result <- get_shark_datasets()
  if (!requireNamespace("SHARK4R", quietly = TRUE)) {
    expect_true(nrow(result) <= 1,
                info = "Should not return fake dataset list (6 rows)")
  }
})
```

The next test, "topological indicators handle single-species network", then follows "parse_adjacency_df creates prey-to-predator edges for diet matrix" after one blank line.

- [ ] **Step 5: Parse-check and run the tests**

```bash
for f in R/functions/shark_api_utils.R tests/testthat/test-shark-rewire.R tests/testthat/test-p0p1-fixes.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK\n')"; done
```

Then run the one-file command for `test-shark-rewire.R`, then for `test-p0p1-fixes.R`.

Expected:
- three `OK`;
- `tests 25 failing 0 skipped 0 passed 127`;
- `tests 5 failing 0 skipped 0 passed 9`.

Also check that no cache directory appeared: `git status --short` lists only the three files of this task (and the untracked PDF).

- [ ] **Step 6: Commit**

```bash
git add R/functions/shark_api_utils.R tests/testthat/test-shark-rewire.R tests/testthat/test-p0p1-fixes.R
git commit -m "$(cat <<'EOF'
fix(shark): rewire the SHARK API layer to SHARK4R 1.2.0 (C-6b, F12)

SHARK4R 1.2.0 exports none of the sharkdata_* functions the SHARK tab
called. Taxonomy now uses match_worms_taxa / match_dyntaxa_taxa /
match_algaebase_taxa (the last two only with DYNTAXA_KEY / ALGAEBASE_KEY),
data uses get_shark_data (years + local date filter, bounds, 90 s timeout),
and QC uses check_fields / check_outliers plus local completeness and
coordinate checks. SHARK4R_FUNCTIONS lists every SHARK4R call; a test pins
it to getNamespaceExports("SHARK4R"). Handlers warn instead of message().
The unused get_available_shark_parameters() and get_shark_datasets()
wrappers are removed.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
EOF
)"
```

---

### Task 2: The SHARK tab UI and server (C2.8 wire-or-remove, B3 escaping)

**Files:**
- Rewrite: `R/modules/shark_server.R` (whole file).
- Rewrite: `R/ui/shark_ui.R` (whole file).
- Modify: `tests/testthat/test-shark-rewire.R` (append).

**Interfaces:**
- Consumes: Task 1's `shark_api_utils.R` interfaces; `.scalar_chr()`, `%||%`; shiny, DT, leaflet and bs4Dash as `app.R` attaches them.
- Produces: `shark_taxonomy_card()`, `shark_query_status()`, `shark_occurrence_popup()`, `shark_server(input, output, session)`, `shark_ui()`, `shark_requirements_box()`, `shark_unavailable_ui()`, and the input `shark_qc_datatype`. `app.R` keeps calling `shark_ui()` and `shark_server(input, output, session)` unchanged.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-shark-rewire.R` exactly (the block starts with the blank line that separates it from the last Task 1 test):

```r

# ---------------------------------------------------------------------------
# Rendering (B3: third-party text only through tag builders or escaped)
# ---------------------------------------------------------------------------

test_that("taxonomy cards escape third-party text and show why a lookup gave nothing", {
  source_shark()
  evil <- list(status = "found", scientific_name = "<script>alert(1)</script>", aphia_id = "1 onmouseover=x",
               authority = NA, taxon_status = "accepted", class = "<b>x</b>", family = NA)
  html <- html_of(shark_taxonomy_card("worms", evil))
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", html, fixed = TRUE))
  expect_false(grepl("href", html, fixed = TRUE))  # a non-integer AphiaID gets no link

  good <- html_of(shark_taxonomy_card("worms", list(status = "found", aphia_id = 126436L)))
  expect_true(grepl("taxdetails&amp;id=126436", good, fixed = TRUE))
  expect_match(html_of(shark_taxonomy_card("dyntaxa", list(status = "no_key", message = "needs DYNTAXA_KEY"))),
               "needs DYNTAXA_KEY", fixed = TRUE)
  expect_match(html_of(shark_taxonomy_card("worms", list(status = "not_found"))), "No results found", fixed = TRUE)
  expect_match(html_of(shark_taxonomy_card("worms", list(status = "error", message = "<i>503</i>"))),
               "Lookup failed: &lt;i&gt;503", fixed = TRUE)
})

test_that("occurrence popups escape every field", {
  source_shark()
  d <- data.frame(Species = "<img src=x onerror=alert(1)>", Date = "2022-06-13", Parameter = "Abundance",
                  Value = 5, Unit = "ind/m2", stringsAsFactors = FALSE)
  popup <- shark_occurrence_popup(d)
  expect_false(grepl("<img", popup, fixed = TRUE))
  expect_true(grepl("&lt;img", popup, fixed = TRUE))
})

test_that("query status lines distinguish idle, ok, empty and error", {
  source_shark()
  expect_match(html_of(shark_query_status(NULL, "Nothing yet")), "Nothing yet", fixed = TRUE)
  expect_match(html_of(shark_query_status(list(status = "ok", message = "Retrieved 3 records"), "")),
               "alert-success", fixed = TRUE)
  expect_match(html_of(shark_query_status(list(status = "empty", message = "none"), "")), "alert-warning",
               fixed = TRUE)
  expect_match(html_of(shark_query_status(list(status = "error", message = "<b>x</b>"), "")),
               "&lt;b&gt;x", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# UI and server wiring
# ---------------------------------------------------------------------------

test_that("the SHARK UI offers exact SHARK parameters, the key-gated sources and a QC data type", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "", ALGAEBASE_KEY = "")
  html <- html_of(shark_ui())
  expect_true(grepl('value="Temperature CTD"', html, fixed = TRUE))
  expect_false(grepl('value="temperature"', html, fixed = TRUE))
  expect_true(grepl('id="shark_qc_datatype"', html, fixed = TRUE))
  expect_true(grepl('value="worms"', html, fixed = TRUE))
  expect_false(grepl('value="dyntaxa"', html, fixed = TRUE))
  expect_true(grepl("Not configured on this server (no subscription key): Dyntaxa (DYNTAXA_KEY)", html,
                    fixed = TRUE))
})

test_that("without SHARK4R >= 1.2.0 the tab shows installation help and the server does nothing", {
  source_shark()
  local_global_mock("shark4r_installed", function() FALSE)
  html <- html_of(shark_ui())
  expect_true(grepl("not available on this server", html, fixed = TRUE))
  expect_false(grepl("shark_tabs", html, fixed = TRUE))
  expect_null(shark_server(NULL, NULL, NULL))
})

test_that("the server runs a taxonomy search and an environmental query end to end (testServer)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(
    match_worms_taxa = function(taxa_names, ...) worms_row(taxa_names),
    get_shark_data = function(...) shark_rows(),
    .package = "SHARK4R"
  )
  cache_root <- withr::local_tempdir()  # the default cache_dir must not land in the repo
  local_global_mock("app_path", function(...) file.path(cache_root, ...))
  shiny::testServer(shark_server, {
    session$setInputs(shark_species_name = "Gadus morhua", shark_taxonomy_sources = "worms",
                      shark_fuzzy_search = TRUE, shark_search_taxonomy = 1)
    expect_match(as.character(output$shark_taxonomy_results$html), "126436", fixed = TRUE)

    session$setInputs(shark_date_range = as.Date(c("2024-01-01", "2024-12-31")),
                      shark_parameters = "Temperature CTD", shark_max_env_records = 5000,
                      shark_query_environmental = 1)
    expect_equal(shark_data$environmental$status, "ok")
    expect_match(as.character(output$shark_environmental_status$html), "Retrieved 3 records", fixed = TRUE)
  })
})
```

- [ ] **Step 2: Run the tests to verify the new ones fail**

Run the one-file command for `test-shark-rewire.R`.

Expected: `tests 31 failing 6 skipped 0 passed 129`. The six new tests fail on "could not find function "shark_taxonomy_card"" (and "shark_query_status", "shark_occurrence_popup"), on the old UI's slug parameters, and on the missing install-help fallback. The 25 Task 1 tests still pass.

- [ ] **Step 3: Rewrite `R/modules/shark_server.R`**

Replace the whole file with exactly:

```r
#' SHARK4R Server Module
#'
#' Swedish ocean archives integration (SHARK4R >= 1.2.0): taxonomy search
#' (WoRMS; Dyntaxa and AlgaeBase when their keys are set), physical-chemical
#' data, species records from SHARK, and quality control of SHARK files.
#' Every third-party value is rendered through tag builders or escaped.

# ---------------------------------------------------------------------------
# Pure render helpers (tested directly in test-shark-rewire.R)
# ---------------------------------------------------------------------------

SHARK_SOURCE_TITLES <- c(
  worms = "WoRMS (World Register of Marine Species)",
  dyntaxa = "DYNTAXA (Swedish Taxonomy)",
  algaebase = "ALGAEBASE (Algae Database)"
)

SHARK_SOURCE_CLASSES <- c(worms = "alert alert-info", dyntaxa = "alert alert-success",
                          algaebase = "alert alert-primary")

.shark_value <- function(x) {
  v <- .scalar_chr(x)
  if (is.na(v)) "—" else v
}

.shark_row <- function(label, value) {
  tags$tr(tags$td(tags$strong(label)), tags$td(value))
}

.shark_aphia_link <- function(aphia_id) {
  id <- suppressWarnings(as.integer(.scalar_chr(aphia_id)))
  if (is.na(id)) return("—")
  tags$a(href = paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", id),
         target = "_blank", rel = "noopener noreferrer", as.character(id))
}

#' One result card of the taxonomy search
#'
#' @param source_name "worms", "dyntaxa" or "algaebase".
#' @param result A query_shark_worms() / query_dyntaxa() / query_algaebase() result.
#' @return A shiny tag.
shark_taxonomy_card <- function(source_name, result) {
  title <- tags$h5(tags$strong(SHARK_SOURCE_TITLES[[source_name]]))
  status <- .scalar_chr(result$status)
  if (!identical(status, "found")) {
    text <- switch(
      if (is.na(status)) "error" else status,
      not_found = "No results found in this database",
      no_key = .shark_value(result$message),
      paste("Lookup failed:", .shark_value(result$message))
    )
    return(tags$div(
      class = if (identical(status, "not_found")) "alert alert-secondary" else "alert alert-warning",
      style = "margin-bottom: 15px;",
      title,
      tags$p(icon(if (identical(status, "not_found")) "times-circle" else "exclamation-triangle"), " ", text)
    ))
  }
  rows <- switch(
    source_name,
    worms = list(
      .shark_row("Scientific Name:", .shark_value(result$scientific_name)),
      .shark_row("AphiaID:", .shark_aphia_link(result$aphia_id)),
      .shark_row("Authority:", .shark_value(result$authority)),
      .shark_row("Status:", .shark_value(result$taxon_status)),
      .shark_row("Class:", .shark_value(result$class)),
      .shark_row("Family:", .shark_value(result$family))
    ),
    dyntaxa = list(
      .shark_row("Matched Name:", .shark_value(result$matched_name)),
      .shark_row("Scientific Name:", .shark_value(result$scientific_name)),
      .shark_row("Taxon ID:", .shark_value(result$taxon_id)),
      .shark_row("Author:", .shark_value(result$author))
    ),
    algaebase = list(
      .shark_row("Scientific Name:", .shark_value(result$scientific_name)),
      .shark_row("AlgaeBase ID:", .shark_value(result$algaebase_id)),
      .shark_row("Authority:", .shark_value(result$authority)),
      .shark_row("Phylum:", .shark_value(result$phylum)),
      .shark_row("Class:", .shark_value(result$class))
    )
  )
  tags$div(class = SHARK_SOURCE_CLASSES[[source_name]], style = "margin-bottom: 15px;", title,
           tags$table(class = "table table-sm", rows))
}

#' Status line for a data query result
#'
#' @param res NULL (no query yet) or a get_shark_*() result.
#' @param idle_text Text shown before the first query.
#' @return A shiny tag.
shark_query_status <- function(res, idle_text) {
  if (is.null(res)) {
    return(tags$div(class = "alert alert-info", icon("info-circle"), " ", idle_text))
  }
  switch(
    .shark_value(res$status),
    ok = tags$div(class = "alert alert-success", icon("check-circle"), " ", .shark_value(res$message)),
    empty = tags$div(class = "alert alert-warning", icon("exclamation-triangle"), " ", .shark_value(res$message)),
    tags$div(class = "alert alert-danger", icon("exclamation-triangle"), " ", .shark_value(res$message))
  )
}

#' Leaflet popups for occurrence rows, every field HTML-escaped
#'
#' @param d format_shark_results(..., "occurrence") rows.
#' @return Character vector of popup HTML.
shark_occurrence_popup <- function(d) {
  esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x)))
  paste0("<strong>", esc(d$Species), "</strong><br>",
         "Date: ", esc(d$Date), "<br>",
         esc(d$Parameter), ": ", esc(d$Value), " ", esc(d$Unit))
}

#' SHARK4R Server Module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
shark_server <- function(input, output, session) {
  # shark_ui() shows only an installation hint without a usable SHARK4R.
  if (!shark4r_installed()) return(invisible(NULL))

  shark_data <- reactiveValues(
    taxonomy_results = NULL,
    environmental = NULL,
    occurrence = NULL,
    qc_results = NULL
  )

  # ---------------------------------------------------------------------------
  # TAB 1: Taxonomy Search
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_search_taxonomy, {
    species_name <- trimws(input$shark_species_name %||% "")
    req(nzchar(species_name))
    sources <- intersect(input$shark_taxonomy_sources, names(SHARK_SOURCE_TITLES))
    req(length(sources) > 0)
    fuzzy <- isTRUE(input$shark_fuzzy_search)

    withProgress(message = "Querying taxonomic databases...", value = 0, {
      results <- list()
      for (src in sources) {
        incProgress(1 / length(sources), detail = sprintf("Querying %s...", src))
        results[[src]] <- switch(
          src,
          worms = query_shark_worms(species_name, fuzzy = fuzzy),
          dyntaxa = query_dyntaxa(species_name, fuzzy = fuzzy),
          algaebase = query_algaebase(species_name)
        )
      }
      shark_data$taxonomy_results <- results
    })
  })

  output$shark_taxonomy_results <- renderUI({
    results <- shark_data$taxonomy_results
    req(results)
    do.call(tagList, lapply(names(results), function(src) shark_taxonomy_card(src, results[[src]])))
  })

  # ---------------------------------------------------------------------------
  # TAB 2: Environmental Data
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_query_environmental, {
    req(input$shark_date_range)
    bbox <- c(
      north = input$shark_bbox_north %||% NA,
      south = input$shark_bbox_south %||% NA,
      east = input$shark_bbox_east %||% NA,
      west = input$shark_bbox_west %||% NA
    )
    withProgress(message = "Retrieving environmental data from SHARK...", value = 0.5, {
      shark_data$environmental <- get_shark_environmental_data(
        parameters = input$shark_parameters,
        start_date = input$shark_date_range[1],
        end_date = input$shark_date_range[2],
        bbox = bbox,
        max_records = input$shark_max_env_records
      )
      incProgress(0.5)
    })
  })

  env_table <- reactive({
    res <- shark_data$environmental
    req(identical(res$status, "ok"))
    format_shark_results(res$data, "environmental")
  })

  output$shark_environmental_status <- renderUI({
    shark_query_status(shark_data$environmental,
                       "No data retrieved yet. Configure query parameters and click 'Query Data'.")
  })

  output$shark_environmental_table <- renderDT({
    datatable(env_table(), options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE, class = "cell-border stripe")
  })

  output$shark_download_environmental <- downloadHandler(
    filename = function() paste0("shark_environmental_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(env_table(), file, row.names = FALSE)
  )

  # ---------------------------------------------------------------------------
  # TAB 3: Species Occurrence
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_query_occurrence, {
    req(input$shark_occurrence_species)
    req(input$shark_occurrence_dates)
    withProgress(message = "Retrieving species records from SHARK...", value = 0.5, {
      shark_data$occurrence <- get_shark_species_occurrence(
        species_name = input$shark_occurrence_species,
        start_date = input$shark_occurrence_dates[1],
        end_date = input$shark_occurrence_dates[2],
        max_records = input$shark_max_occ_records
      )
      incProgress(0.5)
    })
  })

  occ_table <- reactive({
    res <- shark_data$occurrence
    req(identical(res$status, "ok"))
    format_shark_results(res$data, "occurrence")
  })

  output$shark_occurrence_status <- renderUI({
    shark_query_status(shark_data$occurrence,
                       "No records retrieved yet. Enter a scientific name and click 'Get Records'.")
  })

  output$shark_occurrence_table <- renderDT({
    datatable(occ_table(), options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE, class = "cell-border stripe")
  })

  output$shark_occurrence_map <- renderLeaflet({
    d <- occ_table()
    lat <- suppressWarnings(as.numeric(d$Lat))
    lon <- suppressWarnings(as.numeric(d$Lon))
    keep <- is.finite(lat) & is.finite(lon)
    if (!any(keep)) {
      return(leaflet() %>% addTiles() %>% setView(lng = 18, lat = 59, zoom = 5))
    }
    d <- d[keep, , drop = FALSE]
    d$Lat <- lat[keep]
    d$Lon <- lon[keep]
    leaflet(d) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Lon, lat = ~Lat, popup = shark_occurrence_popup(d), radius = 5,
                       color = "#007bff", fillOpacity = 0.7, stroke = TRUE, weight = 1) %>%
      fitBounds(lng1 = min(d$Lon), lat1 = min(d$Lat), lng2 = max(d$Lon), lat2 = max(d$Lat))
  })

  output$shark_download_occurrence <- downloadHandler(
    filename = function() paste0("shark_occurrence_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(occ_table(), file, row.names = FALSE)
  )

  # ---------------------------------------------------------------------------
  # TAB 4: Quality Control
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_run_qc, {
    req(input$shark_qc_file)
    withProgress(message = "Running quality control checks...", value = 0, {
      incProgress(0.2, detail = "Reading file...")
      data <- read_shark_qc_file(input$shark_qc_file$datapath, input$shark_qc_file$name)
      if (is.null(data)) {
        shark_data$qc_results <- list(error = TRUE, message = "Failed to read file. Please check file format.")
        return()
      }
      incProgress(0.3, detail = "Checking...")
      datatype <- resolve_shark_qc_datatype(data, input$shark_qc_datatype %||% "auto")
      shark_data$qc_results <- run_shark_qc(data, input$shark_qc_checks %||% character(0), datatype)
      incProgress(0.5, detail = "Complete!")
    })
  })

  output$shark_qc_status <- renderUI({
    qc <- shark_data$qc_results
    if (is.null(qc)) {
      return(tags$div(class = "alert alert-info", icon("info-circle"),
                      " No quality control run yet. Upload a SHARK format file and click 'Run Quality Control'."))
    }
    if (isTRUE(qc$error)) {
      return(tags$div(class = "alert alert-danger", icon("exclamation-triangle"), paste(" Error:", qc$message)))
    }
    tags$div(class = "alert alert-success", icon("check-circle"),
             sprintf(" Quality control completed - %d rows, %d columns analyzed",
                     qc$data_summary$rows, qc$data_summary$columns))
  })

  output$shark_qc_results <- renderPrint({
    qc <- shark_data$qc_results
    req(qc, !isTRUE(qc$error))
    cat(format_shark_qc_report(qc), sep = "\n")
  })

  output$shark_qc_warnings <- renderUI({
    qc <- shark_data$qc_results
    req(qc, !isTRUE(qc$error))
    items <- c(qc$validation$warnings,
               if (NROW(qc$outliers$outliers) > 0) qc$outliers$message,
               if (isTRUE(qc$coordinates$zero + qc$coordinates$out_of_range > 0)) qc$coordinates$message)
    if (length(items) == 0) {
      return(tags$div(class = "alert alert-success", icon("check"), " No warnings detected"))
    }
    tags$div(class = "alert alert-warning",
             tags$h5(icon("exclamation-triangle"), " Warnings:"),
             tags$ul(lapply(items, tags$li)))
  })

  output$shark_qc_summary <- renderUI({
    qc <- shark_data$qc_results
    req(qc, !isTRUE(qc$error))
    completeness <- qc$quality$completeness
    req(isTRUE(is.finite(completeness)))
    status_class <- if (completeness >= 95) "success" else if (completeness >= 80) "warning" else "danger"
    tags$div(
      class = paste0("alert alert-", status_class),
      tags$h5("Data Quality Summary:"),
      tags$p(sprintf("Overall completeness: %.1f%%", completeness)),
      tags$p(
        if (completeness >= 95) {
          "Data quality is excellent."
        } else if (completeness >= 80) {
          "Data quality is acceptable but some values are missing."
        } else {
          "Data quality needs improvement. Significant missing values detected."
        }
      )
    )
  })
}
```

- [ ] **Step 4: Rewrite `R/ui/shark_ui.R`**

Replace the whole file with exactly:

```r
#' SHARK Data UI
#'
#' Creates the SHARK4R tab for accessing Swedish marine environmental archives
#'
#' @return A tabItem for SHARK data access
#'
#' @details
#' Provides 4 sub-tabs:
#' 1. Taxonomy - Query WoRMS; Dyntaxa and AlgaeBase when their keys are set
#' 2. Environmental Data - Retrieve oceanographic measurements
#' 3. Species Occurrence - SHARK records of one taxon
#' 4. Quality Control - Validate SHARK format data
#' Without SHARK4R >= 1.2.0 the tab shows installation instructions only.
shark_ui <- function() {
  if (!shark4r_installed()) return(shark_unavailable_ui())

  taxonomy_choices <- shark_taxonomy_source_choices()
  keyless_sources <- c("Dyntaxa (DYNTAXA_KEY)", "AlgaeBase (ALGAEBASE_KEY)")[
    !c("dyntaxa", "algaebase") %in% taxonomy_choices]

  # ========================================================================
  # SHARK DATA TAB
  # ========================================================================
  tabItem(
    tabName = "shark",

    # Header box with description
    fluidRow(
      box(
        title = "SHARK4R - Swedish Ocean Archives",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        HTML("
          <h4>Access Swedish Marine Environmental Data</h4>
          <p><strong>SHARK</strong> (Svenskt HavsARKiv) is Sweden's national database for marine environmental
          monitoring data, maintained by SMHI (Swedish Meteorological and Hydrological Institute).</p>
          <p>This interface provides access to:</p>
          <ul>
            <li><strong>Taxonomy:</strong> WoRMS; Dyntaxa (Swedish species) and AlgaeBase need subscription keys</li>
            <li><strong>Environmental Data:</strong> Temperature, salinity, nutrients, oxygen from 1900s-present</li>
            <li><strong>Species Occurrence:</strong> Biological records (plankton, benthos, seals)</li>
            <li><strong>Quality Control:</strong> Validate SHARK format data files</li>
          </ul>
          <p style='margin-top: 10px;'>
            <strong>Documentation:</strong>
            <a href='https://sharksmhi.github.io/SHARK4R/' target='_blank'>SHARK4R Package</a> |
            <a href='https://shark.smhi.se/en' target='_blank'>SHARK Database</a>
          </p>
        ")
      )
    ),

    # Main content with 4 sub-tabs
    fluidRow(
      box(
        width = 12,
        solidHeader = FALSE,
        status = "primary",

        tabsetPanel(
          id = "shark_tabs",
          type = "tabs",

          # ====================================================================
          # TAB 1: TAXONOMY LOOKUP
          # ====================================================================
          tabPanel(
            title = tagList(icon("search"), " Taxonomy"),
            value = "shark_taxonomy",
            br(),

            fluidRow(
              # Left column: Query panel
              column(4,
                box(
                  title = "Search Species Taxonomy",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,

                  textInput("shark_species_name",
                           "Species Name:",
                           placeholder = "e.g., 'Gadus morhua', 'Macoma balthica'"),

                  checkboxGroupInput("shark_taxonomy_sources",
                    "Data Sources:",
                    choices = taxonomy_choices,
                    selected = unname(taxonomy_choices)
                  ),

                  if (length(keyless_sources) > 0) {
                    tags$small(paste("Not configured on this server (no subscription key):",
                                     paste(keyless_sources, collapse = ", ")))
                  },

                  checkboxInput("shark_fuzzy_search",
                               "Fuzzy matching",
                               value = TRUE),

                  actionButton("shark_search_taxonomy",
                             "Search Taxonomy",
                             icon = icon("search"),
                             class = "btn-primary btn-block"),

                  br(),
                  tags$small(HTML("
                    <strong>Tip:</strong> Use scientific names. Dyntaxa also accepts Swedish names
                    such as 'torsk' (cod) or 'sill' (herring).
                  "))
                )
              ),

              # Right column: Results panel
              column(8,
                box(
                  title = "Taxonomy Results",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  style = "overflow-y: auto;",

                  uiOutput("shark_taxonomy_results")
                )
              )
            )
          ),

          # ====================================================================
          # TAB 2: ENVIRONMENTAL DATA
          # ====================================================================
          tabPanel(
            title = tagList(icon("water"), " Environmental Data"),
            value = "shark_environmental",
            br(),

            fluidRow(
              # Left column: Query builder
              column(4,
                box(
                  title = "Query Environmental Data",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,

                  dateRangeInput("shark_date_range",
                               "Date Range:",
                               start = Sys.Date() - 3 * 365,
                               end = Sys.Date(),
                               min = "1900-01-01",
                               max = Sys.Date()),

                  selectInput("shark_parameters",
                            "Parameters:",
                            choices = SHARK_ENV_PARAMETERS,
                            multiple = TRUE,
                            selected = c("Temperature CTD", "Salinity CTD")),

                  tags$hr(),
                  tags$h5("Bounding Box (Optional)"),
                  tags$small("Fill in all four fields, or leave all four blank for all Swedish waters"),

                  fluidRow(
                    column(6,
                      numericInput("shark_bbox_north",
                                 "North (Lat):",
                                 value = NULL,
                                 min = 54, max = 66, step = 0.1)
                    ),
                    column(6,
                      numericInput("shark_bbox_south",
                                 "South (Lat):",
                                 value = NULL,
                                 min = 54, max = 66, step = 0.1)
                    )
                  ),

                  fluidRow(
                    column(6,
                      numericInput("shark_bbox_east",
                                 "East (Lon):",
                                 value = NULL,
                                 min = 10, max = 25, step = 0.1)
                    ),
                    column(6,
                      numericInput("shark_bbox_west",
                                 "West (Lon):",
                                 value = NULL,
                                 min = 10, max = 25, step = 0.1)
                    )
                  ),

                  numericInput("shark_max_env_records",
                             "Max Records:",
                             value = 5000,
                             min = 100, max = 50000, step = 1000),

                  actionButton("shark_query_environmental",
                             "Query Data",
                             icon = icon("download"),
                             class = "btn-success btn-block")
                )
              ),

              # Right column: Results display
              column(8,
                box(
                  title = "Environmental Data Results",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,

                  uiOutput("shark_environmental_status"),
                  br(),

                  DTOutput("shark_environmental_table"),

                  br(),
                  downloadButton("shark_download_environmental",
                               "Download CSV",
                               class = "btn-primary")
                )
              )
            )
          ),

          # ====================================================================
          # TAB 3: SPECIES OCCURRENCE
          # ====================================================================
          tabPanel(
            title = tagList(icon("fish"), " Species Occurrence"),
            value = "shark_occurrence",
            br(),

            fluidRow(
              # Left column: Query panel
              column(4,
                box(
                  title = "Query Species Records",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,

                  textInput("shark_occurrence_species",
                           "Scientific Name:",
                           placeholder = "e.g., 'Macoma balthica', 'Temora longicornis'"),

                  dateRangeInput("shark_occurrence_dates",
                               "Date Range:",
                               start = Sys.Date() - 365 * 5,  # 5 years
                               end = Sys.Date(),
                               min = "1900-01-01",
                               max = Sys.Date()),

                  numericInput("shark_max_occ_records",
                             "Max Records:",
                             value = 2000,
                             min = 100, max = 10000, step = 500),

                  actionButton("shark_query_occurrence",
                             "Get Records",
                             icon = icon("search"),
                             class = "btn-primary btn-block"),

                  br(),
                  tags$small(HTML("
                    <strong>Note:</strong> SHARK matches the exact scientific name. It holds plankton,
                    benthos and seal records; most fish are not in SHARK. Each row is one measured
                    parameter (count, abundance, weight).
                  "))
                )
              ),

              # Right column: Results + Map
              column(8,
                box(
                  title = "Species Records",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,

                  uiOutput("shark_occurrence_status"),
                  br(),

                  # Interactive map
                  leafletOutput("shark_occurrence_map", height = "400px"),

                  br(),
                  tags$hr(),

                  # Data table
                  DTOutput("shark_occurrence_table"),

                  br(),
                  downloadButton("shark_download_occurrence",
                               "Download CSV",
                               class = "btn-primary")
                )
              )
            )
          ),

          # ====================================================================
          # TAB 4: QUALITY CONTROL
          # ====================================================================
          tabPanel(
            title = tagList(icon("check-circle"), " Quality Control"),
            value = "shark_qc",
            br(),

            fluidRow(
              # Left column: Upload panel
              column(4,
                box(
                  title = "Upload SHARK Format Data",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,

                  fileInput("shark_qc_file",
                          "Choose File:",
                          accept = c(".csv", ".txt", ".tsv"),
                          placeholder = "Select SHARK format file"),

                  tags$small(HTML("
                    <strong>Accepted formats:</strong> CSV, or tab-separated TXT/TSV<br>
                    <strong>Expected structure:</strong> SHARK standard format
                  ")),

                  br(), br(),

                  selectInput("shark_qc_datatype",
                            "Data Type:",
                            choices = c("Auto (from delivery_datatype column)" = "auto", SHARK_QC_DATATYPES),
                            selected = "auto"),

                  actionButton("shark_run_qc",
                             "Run Quality Control",
                             icon = icon("play"),
                             class = "btn-warning btn-block"),

                  br(),

                  checkboxGroupInput("shark_qc_checks",
                    "Quality Checks:",
                    choices = c(
                      "Format validation" = "format",
                      "Data completeness" = "completeness",
                      "Outlier detection" = "outliers",
                      "Coordinate validation" = "coordinates"
                    ),
                    selected = c("format", "completeness", "coordinates")
                  )
                )
              ),

              # Right column: Results panel
              column(8,
                box(
                  title = "Quality Control Results",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  style = "overflow-y: auto;",

                  uiOutput("shark_qc_status"),
                  br(),

                  verbatimTextOutput("shark_qc_results"),

                  br(),
                  uiOutput("shark_qc_warnings"),

                  br(),
                  uiOutput("shark_qc_summary")
                )
              )
            )
          )

        )  # End tabsetPanel
      )  # End main box
    ),  # End main fluidRow

    shark_requirements_box()

  )  # End tabItem
  # ========================================================================
}

#' Requirements and installation box (collapsible)
shark_requirements_box <- function(collapsed = TRUE) {
  fluidRow(
    box(
      title = "Requirements & Installation",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = collapsed,

      HTML("
        <h5>Required R Package</h5>
        <p>This module requires the SHARK4R package, version 1.2.0 or newer:</p>
        <pre style='background: #f8f9fa; padding: 10px; border-left: 3px solid #007bff;'>
install.packages('SHARK4R')</pre>

        <h5>Optional subscription keys</h5>
        <ul>
          <li><strong>DYNTAXA_KEY</strong> enables Dyntaxa (SLU Artdatabanken API).</li>
          <li><strong>ALGAEBASE_KEY</strong> enables AlgaeBase (AlgaeBase API subscription).</li>
        </ul>
        <p>Set them in the server's <code>.Renviron</code> and restart the app.</p>

        <h5>Package Information</h5>
        <ul>
          <li><strong>Version:</strong> 1.2.0+</li>
          <li><strong>License:</strong> MIT</li>
          <li><strong>Maintainer:</strong> SMHI (Swedish Meteorological and Hydrological Institute)</li>
        </ul>

        <h5>Documentation & Resources</h5>
        <ul>
          <li><a href='https://sharksmhi.github.io/SHARK4R/' target='_blank'>
              SHARK4R Package Documentation</a></li>
          <li><a href='https://shark.smhi.se/en' target='_blank'>
              SHARK Database (SMHI)</a></li>
          <li><a href='https://www.artdatabanken.se/vara-datavaror/dyntaxa/' target='_blank'>
              Dyntaxa - Swedish Taxonomic Database</a></li>
        </ul>

        <h5>Data Coverage</h5>
        <ul>
          <li><strong>Temporal:</strong> 1900s - present (varies by parameter)</li>
          <li><strong>Spatial:</strong> Swedish waters (Baltic Sea, Skagerrak, Kattegat)</li>
          <li><strong>Parameters:</strong> 100+ environmental and biological variables</li>
        </ul>
      ")
    )
  )
}

#' The SHARK tab when SHARK4R >= 1.2.0 is not installed
shark_unavailable_ui <- function() {
  tabItem(
    tabName = "shark",
    fluidRow(
      box(
        title = "SHARK Data is not available on this server",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        tags$p(icon("exclamation-triangle"),
               " The SHARK Data tab needs the SHARK4R package, version 1.2.0 or newer.")
      )
    ),
    shark_requirements_box(collapsed = FALSE)
  )
}
```

- [ ] **Step 5: Parse-check and run the tests**

```bash
for f in R/modules/shark_server.R R/ui/shark_ui.R tests/testthat/test-shark-rewire.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK\n')"; done
```

Then run the one-file command for `test-shark-rewire.R`.

Expected:
- three `OK`;
- `tests 31 failing 0 skipped 0 passed 152`.

`git status --short` must not show a `cache/shark/` change: the testServer test points `app_path()` at a temp directory.

- [ ] **Step 6: Commit**

```bash
git add R/modules/shark_server.R R/ui/shark_ui.R tests/testthat/test-shark-rewire.R
git commit -m "$(cat <<'EOF'
fix(shark): rewire the SHARK tab UI and server to the 1.2.0 wrappers (C-6b, C2.8)

Exact SHARK parameter names instead of slugs; only configured taxonomy
sources (WoRMS; Dyntaxa / AlgaeBase with their keys); status lines that
tell empty from failed; long-format species records; a QC data-type
select; the outlier and coordinate checkboxes now run. Cards, status lines
and leaflet popups render third-party text escaped (B3). Without SHARK4R
>= 1.2.0 the tab shows installation help and the server registers nothing.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
EOF
)"
```

---

### Task 3: SHARK4R in CI; live checks

**Files:**
- Modify: `.github/workflows/ci.yml` (job `testthat-offline`, step "Install R dependencies").
- Modify: `.github/workflows/nightly-live-tests.yml` (job `live-tests`, step "Install R dependencies").
- Modify: `tests/testthat/test-ci-workflows.R` (append one test).
- Create: `tests/testthat/test-shark-live.R`

**Interfaces:**
- Consumes: `job_r_packages(file, job)` (defined in `test-ci-workflows.R`), `skip_if_no_live_tests()` (helper-fixtures), `with_timeout()`, Task 1's `SHARK_ENV_PARAMETERS`, `query_shark_worms()`, `get_shark_environmental_data()`.
- Produces: both test jobs install SHARK4R, so `test-shark-rewire.R` runs in CI instead of skipping; the nightly job runs the live checks.

- [ ] **Step 1: Write the failing CI guard**

In `tests/testthat/test-ci-workflows.R`, replace the end of the file

```r
  expect_gt(length(offline_apt), 0L)
  expect_equal(setdiff(offline_apt, nightly_apt), character(0))
})
```

with

```r
  expect_gt(length(offline_apt), 0L)
  expect_equal(setdiff(offline_apt, nightly_apt), character(0))
})

# test-shark-rewire.R checks that SHARK4R exports every function the SHARK tab
# calls, and mocks SHARK4R for the rest. Without SHARK4R on the runner those
# tests skip, and an API change could break the tab again unnoticed (C-6b, F12:
# SHARK4R 1.2.0 had removed all 12 functions the tab called).
test_that("both test jobs install SHARK4R, whose exports the SHARK tab relies on", {
  skip_if_not_installed("yaml")
  expect_true("SHARK4R" %in% job_r_packages("ci.yml", "testthat-offline"))
  expect_true("SHARK4R" %in% job_r_packages("nightly-live-tests.yml", "live-tests"))
})
```

- [ ] **Step 2: Run it to verify it fails**

Run the one-file command for `test-ci-workflows.R`.

Expected: `tests 6 failing 1 skipped 0 passed 21`. The new test fails: `"SHARK4R" %in% ...` is FALSE.

- [ ] **Step 3: Install SHARK4R in both jobs**

In `.github/workflows/ci.yml`, replace

```yaml
            any::writexl

      - name: Run offline testthat suite (fail on any test failure)
```

with

```yaml
            any::writexl
            any::SHARK4R

      - name: Run offline testthat suite (fail on any test failure)
```

In `.github/workflows/nightly-live-tests.yml`, replace

```yaml
            any::writexl

      - name: Run live testthat suite (fail on any test failure)
```

with

```yaml
            any::writexl
            any::SHARK4R

      - name: Run live testthat suite (fail on any test failure)
```

SHARK4R imports sf and terra. The GDAL/GEOS/PROJ/udunits dev libraries are already in both jobs' apt lists (the existing nightly-superset test checks that), and `use-public-rspm: true` supplies binaries.

- [ ] **Step 4: Create the live checks**

Create `tests/testthat/test-shark-live.R` with exactly:

```r
# =============================================================================
# Live Integration Tests: SHARK Data tab (C-6b, F12)
# =============================================================================
# These tests hit the REAL SHARK and WoRMS APIs through SHARK4R.
# Skipped by default. Enable with: Sys.setenv(RUN_LIVE_TESTS = "true")
# =============================================================================

source_shark_live <- function() {
  source(file.path(get_app_root(), "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(get_app_root(), "R/functions/shark_api_utils.R"), local = FALSE)
}

test_that("SHARK still offers the parameters and data types the tab sends (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark_live()
  options <- with_timeout(SHARK4R::get_shark_options(), timeout = 60, on_timeout = NULL)
  skip_if(is.null(options), "get_shark_options() timed out")
  expect_equal(setdiff(SHARK_ENV_PARAMETERS, options$parameters), character(0))
  expect_true("Physical and Chemical" %in% options$dataTypes)
})

test_that("WoRMS via SHARK4R finds Gadus morhua (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark_live()
  r <- query_shark_worms("Gadus morhua", use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_identical(r$aphia_id, 126436L)
  expect_equal(query_shark_worms("Xx yy", use_cache = FALSE)$status, "not_found")
})

test_that("a small Kattegat temperature query returns SHARK rows (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark_live()
  r <- get_shark_environmental_data("Temperature CTD", "2024-06-01", "2024-06-30",
                                    bbox = c(north = 58, south = 57, east = 12, west = 11))
  expect_equal(r$status, "ok")
  expect_true(all(r$data$parameter == "Temperature CTD"))
  expect_true(all(c("sample_date", "sample_latitude_dd", "value", "unit") %in% names(r$data)))
})
```

- [ ] **Step 5: Parse-check and run the tests**

```bash
for f in tests/testthat/test-ci-workflows.R tests/testthat/test-shark-live.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK\n')"; done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "invisible(lapply(c('.github/workflows/ci.yml', '.github/workflows/nightly-live-tests.yml'), yaml::read_yaml)); cat('YAML OK\n')"
```

Then run the one-file command for `test-ci-workflows.R`, then for `test-shark-live.R`.

Expected:
- two `OK` and `YAML OK`;
- `tests 6 failing 0 skipped 0 passed 23`;
- `tests 3 failing 0 skipped 3 passed 0` (offline, the live checks skip).

**NETWORK (optional; about 1 minute):** run the live file once against the real APIs:

```bash
RUN_LIVE_TESTS=true "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_file('tests/testthat/test-shark-live.R', reporter = 'silent', stop_on_failure = FALSE)); cat('tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n')"
```

Expected: `tests 3 failing 0 skipped 0 passed 8` (as on 2026-09-29). If SHARK is down, record that and continue; the nightly job runs these checks.

- [ ] **Step 6: Commit**

```bash
git add .github/workflows/ci.yml .github/workflows/nightly-live-tests.yml tests/testthat/test-ci-workflows.R tests/testthat/test-shark-live.R
git commit -m "$(cat <<'EOF'
ci: install SHARK4R in the offline and nightly test jobs; live SHARK checks (C-6b)

Without SHARK4R on the runner, the export guard and every mocked SHARK
test skip, which is how SHARK4R 1.2.0's removals reached production
unnoticed. test-ci-workflows.R pins SHARK4R in both jobs;
test-shark-live.R (RUN_LIVE_TESTS=true) checks the parameter names, WoRMS
and one small SHARK query.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
EOF
)"
```

---

### Task 4: Convention note, lint, push and PR

**Files:**
- Modify: `CONTRIBUTING.md` (one bullet)

**Interfaces:**
- Consumes: Tasks 1-3.
- Produces: the fix PR, with the export map and export list in its description, squash-merged without a version bump (Execution ruling 1).

- [ ] **Step 1: Add the convention**

In `CONTRIBUTING.md`, "Trait-pipeline & concurrency patterns", replace

```
  values the cache already holds, bump `TRAIT_LOOKUP_REVISION`
  (`harmonization.R`).

## Commit Messages
```

with

```
  values the cache already holds, bump `TRAIT_LOOKUP_REVISION`
  (`harmonization.R`).
- **List every `SHARK4R::` call in `SHARK4R_FUNCTIONS`**
  (`R/functions/shark_api_utils.R`). SHARK4R 1.2.0 dropped every function
  the SHARK tab called, and nothing noticed until production.
  `test-shark-rewire.R` now fails when a `SHARK4R::` call is missing from
  the list or from `getNamespaceExports("SHARK4R")`, and CI installs
  SHARK4R so the test runs. The SHARK wrappers return a status (`found` /
  `not_found` / `no_key` / `error`, or `ok` / `empty` / `error`) instead
  of NULL, so the tab can say why nothing came back.

## Commit Messages
```

- [ ] **Step 2: Lint the new code (no new lints)**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/functions/shark_api_utils.R', 'R/modules/shark_server.R', 'R/ui/shark_ui.R', 'tests/testthat/test-shark-rewire.R', 'tests/testthat/test-shark-live.R', 'tests/testthat/test-ci-workflows.R', 'tests/testthat/test-p0p1-fixes.R')) { l <- lintr::lint(f, linters = list(lintr::line_length_linter(120), lintr::trailing_whitespace_linter(), lintr::assignment_linter())); cat(f, length(l), '\n') }"
```

Expected (verified): `0` for all seven files.

- [ ] **Step 3: Re-run the touched test files (not the full suite)**

Run the one-file command, one file at a time, for `test-shark-rewire.R`, `test-p0p1-fixes.R`, `test-ci-workflows.R` and `test-shark-live.R`.

Expected:
- `tests 31 failing 0 skipped 0 passed 152`;
- `tests 5 failing 0 skipped 0 passed 9`;
- `tests 6 failing 0 skipped 0 passed 23`;
- `tests 3 failing 0 skipped 3 passed 0`.

The full suite runs in CI on the PR (Execution ruling 8). If CI shows a failure in a file this plan does not touch, compare it with master's CI run before changing anything.

- [ ] **Step 4: Commit**

```bash
git add CONTRIBUTING.md
git commit -m "$(cat <<'EOF'
docs(contributing): list every SHARK4R call in SHARK4R_FUNCTIONS (C-6b)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
EOF
)"
```

- [ ] **Step 5: Record the export list for the PR (spec rollout item 3)**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "x <- sort(getNamespaceExports('SHARK4R')); cat('SHARK4R', as.character(packageVersion('SHARK4R')), '-', length(x), 'exports:\n'); cat(x, sep = ', '); cat('\n\nSHARK4R_FUNCTIONS missing from the exports:', length(setdiff(c('match_worms_taxa', 'match_dyntaxa_taxa', 'parse_scientific_names', 'match_algaebase_taxa', 'get_shark_data', 'translate_shark_datatype', 'check_fields', 'check_outliers'), x)), '\n')" > "<scratchpad>/shark4r_exports.txt"
cat "<scratchpad>/shark4r_exports.txt"
```

Expected: `SHARK4R 1.2.0 - 160 exports:` (1.2.1 may differ), the comma-separated list, and `... missing from the exports: 0`.

- [ ] **Step 6: Push and open the PR (STOP - ask the user before running)**

Write `<scratchpad>/c6b_pr_body.md` with the Write tool. It contains:
1. the text below;
2. the export map table from this plan ("The SHARK4R export map" section), copied verbatim where marked;
3. the content of `<scratchpad>/shark4r_exports.txt`, pasted where marked.

```markdown
Implements spec C (`docs/superpowers/specs/2026-09-26-fix-c-trait-pipeline-correctness-design.md`) PR C-6b: C2.8 and F12.

SHARK4R 1.2.0 (on laguna) exports none of the 12 functions the SHARK Data tab called, so every action on the tab failed in production. By user decision the tab is rewired, not removed.

**Export map** (spec rollout item 3):

<the export map table from the plan>

- **Taxonomy:** WoRMS via `match_worms_taxa()`. Dyntaxa and AlgaeBase need `DYNTAXA_KEY` / `ALGAEBASE_KEY`; without a key they are not offered, and the tab says so. Laguna has neither key today (user decision 1).
- **Data:** `get_shark_data()` with exact parameter names, year range plus a date filter, `bounds`, a 90 s timeout, and statuses that tell "no records" apart from "SHARK failed". Species records are long format (one row per measured parameter).
- **QC:** format validation with `check_fields()` for a chosen or detected data type; outliers with `check_outliers()`; local completeness and coordinate checks. The two formerly dead checkboxes now run (user decision 2).
- **F12:** all handlers `warning()` with a `[shark]` prefix; no `message()` left in the SHARK files.
- **B3:** cards, status lines and leaflet popups escape third-party text.
- **Guard:** `SHARK4R_FUNCTIONS` plus `test-shark-rewire.R` pin every `SHARK4R::` call to the export list; CI now installs SHARK4R (user decision 3). Live checks are in `test-shark-live.R` (nightly).
- Without SHARK4R >= 1.2.0 the tab shows installation help; `app.R` is unchanged.

No offline-DB rebuild is needed. Deviations are in `docs/superpowers/plans/2026-09-29-c6b-shark-rewire.md`.

<details><summary>getNamespaceExports("SHARK4R") at PR time</summary>

<paste shark4r_exports.txt here>

</details>

🤖 Generated with [Claude Code](https://claude.com/claude-code)

https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
```

Then:

```bash
git push -u origin fix/c6b-shark-rewire
gh pr create --base master --title "fix(shark): C-6b rewire the SHARK Data tab to SHARK4R 1.2.0" --body-file "<scratchpad>/c6b_pr_body.md"
```

Expected: a PR URL and green CI. The offline job now installs SHARK4R, so `test-shark-rewire.R` runs in full there. The live checks skip in that job and run nightly.

- [ ] **Step 7: Merge (STOP - ask the user)**

Squash-merge per Execution ruling 1 (no version bump in this PR). Then continue with Task 5 from the updated master.

---

### Task 5: Release 1.6.3 (separate release PR)

Run only after the Task 4 PR is merged. The version follows Execution ruling 2.

**Files:**
- Modify: `VERSION`, `R/config.R` (the `load_version_info()` fallback), `app.R` (header), `README.md`, `CHANGELOG.md`
- Create: `docs/releases/1.6.3-notes.md`

**Interfaces:**
- Consumes: the merged master; tags `v1.6.0` to `v1.6.2`.
- Produces: `VERSION=1.6.3`, `## [1.6.3] - <date>` at the CHANGELOG head, and tag `v1.6.3`.

- [ ] **Step 1: Branch and check tags**

```bash
git checkout master && git pull --ff-only
git tag -l "v1.6.*"
grep -n -m2 "^## \[" CHANGELOG.md
ls docs/releases/
git checkout -b release/1.6.3
```

Expected:
- tags `v1.6.0`, `v1.6.1` and `v1.6.2`;
- the CHANGELOG head is `## [1.6.2] - 2026-09-29`;
- `docs/releases/` lists `1.5.0-results-changed.md`, `1.5.2-notes.md`, `1.5.3-notes.md`, `1.6.0-notes.md`, `1.6.1-notes.md` and `1.6.2-notes.md`.

If `v1.6.2` is missing, **STOP - ask the user**: the generator would merge 1.6.2's commits into 1.6.3.

- [ ] **Step 2: Bump the version strings**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.6.3 --name "SHARK Rewire"
sed -i 's/\r$//' VERSION app.R README.md
sed -i 's/^GIT_BRANCH=.*/GIT_BRANCH=master/' VERSION
```

`version_bump.R` writes `VERSION` with CRLF and records the current branch; the two `sed` lines fix both. It does not touch the `R/config.R` fallback. In `load_version_info()`'s `version_info <- list(...)`, set:
- `VERSION = "1.6.3"`;
- `VERSION_NAME = "SHARK Rewire"`;
- `RELEASE_DATE = "<release date>"`;
- `PATCH = 3`.

Keep `MAJOR = 1`, `MINOR = 6` and `STATUS = "stable"`.

```bash
grep -n "^VERSION=\|^VERSION_NAME=\|^RELEASE_DATE=\|^MINOR=\|^PATCH=\|^GIT_BRANCH=" VERSION
grep -n 'VERSION = "\|VERSION_NAME = "\|RELEASE_DATE = "\|MINOR = \|PATCH = ' R/config.R | head -5
grep -n "CURRENT VERSION" app.R
grep -n "1\.6\.[0-9]" README.md
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config.R'); parse(file='app.R'); cat('OK\n')"
```

Expected:
- `VERSION=1.6.3`, `MINOR=6`, `PATCH=3`, `GIT_BRANCH=master` and today's `RELEASE_DATE`;
- the same values in `R/config.R`;
- `# CURRENT VERSION: v1.6.3 (...)`;
- README shows 1.6.3 and no 1.6.2;
- `OK`.

- [ ] **Step 3: Write the hand-written release notes**

Create `docs/releases/1.6.3-notes.md` with exactly:

```markdown
### Fixed — the SHARK Data tab works again

- **SHARK4R 1.2.0 had removed every function the SHARK Data tab called**, so every search, query and check on the
  tab failed. The tab now uses the SHARK4R 1.2.0 functions:
  - **Taxonomy:** WoRMS works as before. Dyntaxa and AlgaeBase now need subscription keys (`DYNTAXA_KEY`,
    `ALGAEBASE_KEY` in the server's `.Renviron`); without them they are not offered, and the tab says so.
  - **Environmental data:** the parameters are SHARK's exact names (e.g. "Temperature CTD"). A partly filled
    bounding box is refused. The status says whether a query found nothing, was cut to "Max Records", or failed.
  - **Species records:** SHARK matches the exact scientific name and returns one row per measured parameter (count,
    abundance, weight). Most fish are not in SHARK.
  - **Quality control:** format validation uses SHARK4R's field definitions for the chosen data type (detected from
    the file's `delivery_datatype` column by default). "Outlier detection" (SHARK4R thresholds) and "Coordinate
    validation" now run; before, these checkboxes did nothing.
- The tab shows installation help instead of failing when SHARK4R 1.2.0 or newer is not installed.

### Notes

- No offline-database rebuild is needed.
- SHARK4R 1.2.0 or newer is required for the SHARK Data tab.
```

- [ ] **Step 4: Regenerate the CHANGELOG and re-insert every hand-written note**

1. Save this throwaway helper outside the repo with the Write tool, e.g. `<scratchpad>/reinsert_release_notes.R`. It checks the first NON-blank line, because some notes files start with a blank line.

```r
# Re-insert hand-written release notes (docs/releases/<version>-*.md) under
# their "## [<version>]" headings after scripts/generate_changelog.R has
# regenerated CHANGELOG.md from git history (which drops them).
x <- readLines("CHANGELOG.md", warn = FALSE)
notes <- list.files("docs/releases", pattern = "^[0-9]+\\.[0-9]+\\.[0-9]+-.+\\.md$", full.names = TRUE)
for (f in notes) {
  ver <- sub("-.*$", "", basename(f))
  h <- grep(paste0("^## \\[", gsub(".", "\\.", ver, fixed = TRUE), "\\] - "), x)
  if (length(h) != 1) stop("expected one '## [", ver, "]' heading, found ", length(h))
  body <- readLines(f, warn = FALSE)
  first <- body[nzchar(trimws(body))][1]
  nxt <- grep("^## \\[", x)
  end <- min(c(nxt[nxt > h], length(x) + 1)) - 1
  if (any(x[h:end] == first)) next  # already present
  x <- append(x, c(body, ""), after = h + 1)
  cat("re-inserted", basename(f), "under [", ver, "]\n")
}
while (length(x) > 0 && x[length(x)] == "") x <- x[-length(x)]  # one trailing newline
con <- file("CHANGELOG.md", "wb")
writeLines(x, con, sep = "\n")
close(con)
```

2. Then run:

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.6.3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"   # second run must print nothing
sed -i 's/\r$//' CHANGELOG.md
git diff CHANGELOG.md | grep '^-' | grep -v '^---'
grep -n -m2 "^## \[" CHANGELOG.md
grep -n "^### Changed\|^### Fixed\|^### Results changed\|^### Notes" CHANGELOG.md
tail -c 2 CHANGELOG.md | od -c
```

Expected:
- **First run:** one `re-inserted ...` line per notes file whose section the generator dropped: 1.5.0, 1.5.2, 1.5.3, 1.6.0, 1.6.1, 1.6.2 and 1.6.3, so up to seven lines. A file whose first line is still present is skipped silently.
- **Second run:** it prints nothing.
- **The `-` list:** it is empty, or holds only footer compare-links.
- **The head:** `## [1.6.3] - <date>`, then `## [1.6.2]`.
- **Section headings:**
  - `### Fixed — the SHARK Data tab works again` directly under `[1.6.3]`;
  - the earlier releases' own headings (`### Changed` under `[1.6.2]` and `[1.6.0]`, "### Results changed" under `[1.5.0]`);
  - "### Notes" under each release whose notes carry one.
- **Ending:** the file ends in exactly one `\n`.

If any other `-` line appears, `git checkout -- CHANGELOG.md`, paste `scripts/generate_changelog.R --preview --version 1.6.3` above the old head by hand, and re-run the helper.

- [ ] **Step 5: Commit**

```bash
git add VERSION R/config.R app.R README.md CHANGELOG.md docs/releases/1.6.3-notes.md
git commit -m "$(cat <<'EOF'
chore(release): 1.6.3 - SHARK rewire (C-6b)

Version 1.6.3 in VERSION, the R/config.R fallback, the app.R header and
README. CHANGELOG regenerated with every hand-written note re-inserted and
the new 1.6.3 "the SHARK Data tab works again" section, also kept in
docs/releases/1.6.3-notes.md.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
EOF
)"
```

- [ ] **Step 6: Push, PR, merge, tag (STOP - ask the user before each)**

```bash
git push -u origin release/1.6.3
gh pr create --base master --title "chore(release): 1.6.3 - SHARK rewire" --body "$(cat <<'EOF'
Release PR for C-6b (SHARK Data tab rewired to SHARK4R 1.2.0). Version strings, regenerated CHANGELOG with every hand-written note re-inserted, and the new "the SHARK Data tab works again" section (`docs/releases/1.6.3-notes.md`).

After merge: tag `v1.6.3` on the merge commit and deploy. No offline-DB rebuild is needed.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

https://claude.ai/code/session_01MpQsZdwdoLNvNWPsBZgz4y
EOF
)"
```

After the user merges it:

```bash
git checkout master && git pull --ff-only
git tag -a v1.6.3 -m "1.6.3 - SHARK Rewire" && git push origin v1.6.3
```

---

### Task 6: Deploy 1.6.3 (no offline-DB rebuild)

Every step touches production or the shared server. **Each is STOP: show the user the exact command and wait for confirmation.** Deploy from the merged, tagged master.

**Files:** none in the repo. Remote: `/home/razinka/EcoNeTool_staging/` and `/srv/shiny-server/EcoNeTool/`.

**Interfaces:**
- Consumes: `v1.6.3` on master; SHARK4R 1.2.0 in laguna's site library.
- Produces: production on 1.6.3. The offline DB is untouched.

- [ ] **Step 1: Pre-deploy check (local, read-only)**

```bash
git checkout master && git pull --ff-only && git log -1 --oneline
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; cd ..
```

Expected: the 1.6.3 release commit is HEAD, and there are no errors. The script must run from inside `deployment/`.

- [ ] **Step 2: Upload to staging (STOP)**

```bash
powershell ./deploy-windows.ps1 -NoSudo
```

Expected: the script empties staging and uploads the code. It skips `data/` by default and never uploads `config/api_keys.*`, `config/harmonization_custom.json`, `.Renviron`, `*.bak` or `*safeBackup*`.

Audit staging before copying:

```bash
ssh razinka@laguna.ku.lt "cd /home/razinka/EcoNeTool_staging && ls R/functions/shark_api_utils.R R/modules/shark_server.R R/ui/shark_ui.R && find . \( -name '*.bak' -o -name '*safeBackup*' -o -name '.Renviron' -o -name 'api_keys.*' \) | head"
```

Expected: the three files are listed, and `find` prints nothing. Ignore any printed `sudo` / `rm -rf /srv/shiny-server/EcoNeTool/*` suggestion.

- [ ] **Step 3: Copy over the live tree and reload (STOP)**

```bash
ssh razinka@laguna.ku.lt "cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt"
```

Expected: silent success. `data/`, `cache/` (including `cache/offline_traits.db`) and `.Renviron` survive, because `cp -rT` deletes nothing.

- [ ] **Step 4: Verify the code (STOP - read-only)**

```bash
ssh razinka@laguna.ku.lt "cd /srv/shiny-server/EcoNeTool && grep -c 'SHARK4R_FUNCTIONS <- ' R/functions/shark_api_utils.R; grep -c 'SHARK4R::sharkdata_' R/functions/shark_api_utils.R; grep -c 'shark_taxonomy_card <- function' R/modules/shark_server.R; grep -c 'shark_unavailable_ui <- function' R/ui/shark_ui.R; grep '^VERSION=' VERSION; stat -c %y restart.txt; ls -la cache/ | grep offline_traits"
curl -sL -o /dev/null -w '%{http_code}\n' http://laguna.ku.lt/EcoNeTool/
```

Expected:
- `1`, `0`, `1`, `1`;
- `VERSION=1.6.3`;
- a fresh `restart.txt`;
- `offline_traits.db` present with its pre-deploy timestamp;
- `200`.

- [ ] **Step 5: Server-side check (STOP - read-only; makes one WoRMS and one SHARK call from laguna)**

Save this script with the Write tool as `<scratchpad>/c6b_laguna_check.R` (deviation 16: it does not source the whole `app.R`):

```r
# C-6b server-side check: build the SHARK tab and make one WoRMS and one SHARK
# call, without cache. Run from the app directory. Writes nothing.
if (dir.exists("r-libs")) .libPaths(c(normalizePath("r-libs"), .libPaths()))
suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(DT)
  library(leaflet)
})
source("R/functions/validation_utils.R")
source("R/functions/shark_api_utils.R")
source("R/modules/shark_server.R")
source("R/ui/shark_ui.R")
cat("SHARK4R", as.character(packageVersion("SHARK4R")), "available:", check_shark4r_available(), "\n")
html <- as.character(shark_ui())
cat("ui has Temperature CTD:", grepl("Temperature CTD", html, fixed = TRUE), "\n")
cat("taxonomy sources:", paste(shark_taxonomy_source_choices(), collapse = ", "), "\n")
w <- query_shark_worms("Gadus morhua", use_cache = FALSE)
cat("worms:", w$status, w$aphia_id, "\n")
e <- get_shark_environmental_data("Temperature CTD", "2024-06-01", "2024-06-30",
                                  bbox = c(north = 58, south = 57, east = 12, west = 11))
cat("shark:", e$status, e$message, "\n")
```

Then run it on laguna through stdin, so nothing is copied to the server:

```bash
ssh razinka@laguna.ku.lt 'cd /srv/shiny-server/EcoNeTool && R --no-save --no-restore -s' < "<scratchpad>/c6b_laguna_check.R"
```

Expected (the script ran locally in planning with this output; the record count may change):

```
SHARK4R 1.2.0 available: TRUE
ui has Temperature CTD: TRUE
taxonomy sources: worms
worms: found 126436
shark: ok Retrieved 48 records
```

`taxonomy sources` also lists `dyntaxa` / `algaebase` only if a key was provisioned (Step 7). If `available` is FALSE or `shark` is `error`, **STOP** and report the output.

- [ ] **Step 6: Smoke test (user or browser automation, with the user's go-ahead)**

On https://laguna.ku.lt/EcoNeTool/, **SHARK Data**:
1. **Taxonomy:** `Gadus morhua` with WoRMS gives a WoRMS card with AphiaID 126436 linked to marinespecies.org. The note under the sources says Dyntaxa and AlgaeBase are not configured, unless keys were provisioned.
2. **Environmental Data:**
   - Temperature (CTD), 2024-06-01 to 2024-06-30, bounding box N 58 / S 57 / E 12 / W 11: a green "Retrieved N records" line, and a table with Date / Station / Lat / Lon / Depth / Parameter / Value / Unit.
   - Clear only West: the red status says to fill all four fields.
3. **Species Occurrence:** `Macoma balthica`, the last 5 years: records on the map and in the table (Parameter "# counted" / "Abundance" / "Wet weight").
4. **Quality Control:** upload the environmental CSV downloaded in step 2, choose data type "Physical and Chemical", and tick all four checks. The report shows all four sections and no error box. Because the download uses display column names (Date, Lat, ...), not SHARK's, three results are expected:
   - FORMAT VALIDATION is FAILED, listing missing SHARK fields;
   - OUTLIERS says it needs 'parameter' and 'value' columns;
   - COORDINATES says there are no `sample_latitude_dd` / `sample_longitude_dd` columns.

   With a real SHARK delivery file, the same run validates the file's own fields.

- [ ] **Step 7 (only if the user chose User decision 1 (c)): provision a subscription key (STOP)**

The user puts the key in a local file, e.g. `<scratchpad>/dyntaxa_key.txt`. Never paste the key into chat or onto a command line; it travels through stdin.

The memory gotcha applies: strip any CR first.

```bash
tr -d '\r\n' < "<scratchpad>/dyntaxa_key.txt" | { IFS= read -r KEY; printf 'DYNTAXA_KEY=%s\n' "$KEY"; } | ssh razinka@laguna.ku.lt 'cd /srv/shiny-server/EcoNeTool && cp -p .Renviron .Renviron.bak-$(date +%Y%m%d) && cat >> .Renviron && grep -c "^DYNTAXA_KEY=" .Renviron && touch restart.txt'
```

Expected: `1`. Re-run Step 5: `taxonomy sources: dyntaxa, worms`. For AlgaeBase, use the same command with `ALGAEBASE_KEY` and the AlgaeBase key file.

---

## Self-Review

1. **Spec coverage.**
   - **C2.8, first bullet:**
     - `sharkdata_worms_search` -> `match_worms_taxa` (Task 1).
     - `sharkdata_get_biological` -> `get_shark_data` (Task 1).
     - `sharkdata_dyntaxa_search` -> `match_dyntaxa_taxa`, not `get_dyntaxa_records`, because the latter takes IDs (deviation 1; key-gated, User decision 1).
   - **C2.8, second bullet:** each of the six other wrappers, checked against `getNamespaceExports("SHARK4R")`:
     - algaebase -> rewired;
     - get_physical_chemical -> rewired;
     - validate -> rewired;
     - get_parameters and list_datasets -> same-purpose exports exist, but the wrappers had no caller, so they are removed (deviations 3-4);
     - quality_check -> no export, so it stays local (deviation 5).

     The export list is recorded in the PR (Task 4 Step 5-6), as the export map above and the pasted `getNamespaceExports()` output.
   - **C2.8, third bullet (remove the whole tab if none of the three core functions exists):** they exist, so the tab stays (user decision to rewire).
   - **C2.8, fourth bullet / F12:** "All handlers use `warning()`". Task 1's static test finds no `message()` in either SHARK file; the error-path tests expect `[shark]` warnings.
   - **F12:** "The tab is unconditional" -> Task 2's install-help fallback; `app.R` is unchanged (deviation 13).
   - **Dead UI:**
     - the two QC checkboxes are wired (User decision 2);
     - the slug parameters are replaced by exact names;
     - the "Max Records" inputs are honoured (deviation 8).
   - **Section 5, SHARK row:** `test-shark-rewire.R`, 31 offline tests, plus `test-shark-live.R`.
   - **Section 6, rollout item 3:** Tasks 0-4. Release and deploy -> Tasks 5-6 (no rebuild, Execution ruling 3).
   - **Section 7 risk "SHARK4R API drift":** the export guard in CI (Task 3, User decision 3).
2. **Placeholder scan.** Every code step carries complete code, identical to the files run on the scratch copy of master. Values filled at run time:
   - `<scratchpad>`;
   - `<release date>`;
   - in the PR body, the export map table (copied from this plan) and the `shark4r_exports.txt` output.
3. **Type consistency.**
   - `shark4r_installed()` is used by `shark_ui()`, `shark_server()` and `check_shark4r_available()`.
   - The taxonomy result fields (`status`, `message`, `aphia_id`, `taxon_status`, `matched_name`, ...) match `shark_taxonomy_card()`.
   - The data result `list(status, data, message, total)` matches `shark_query_status()` and the server's `env_table()` / `occ_table()`.
   - `run_shark_qc()`'s `validation` / `quality` / `outliers` / `coordinates` / `data_summary` match `format_shark_qc_report()` and the three QC outputs.
   - `SHARK_ENV_PARAMETERS` and `SHARK_QC_DATATYPES` feed the UI and the live test.
   - The counts per task (Task 1: 25 tests / 127 expectations; Task 2: 31 / 152; Task 3: 6 / 23 and 3 skipped) come from the scratch runs.
4. **Review Focus.** Each of the six lines names the test that pins it (Tasks 1, 2 and 3).
