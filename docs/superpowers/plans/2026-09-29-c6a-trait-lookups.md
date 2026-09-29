# C-6a: Trait Lookups (F33 lookup side, F32, F78, F79, F9, F10, F11) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The trait lookups return correct raw values, and one bad taxon can no longer abort a Trait Research batch. WoRMS body sizes are read in the unit WoRMS gives with them. FishBase weights stay in grams. The AlgaeBase fallback reads the WoRMS tibble correctly. WoRMS classifications are "medium". Common names get an exact-match rule, graded confidence and a region-keyed cache. Binomials are looked up before any singular form. By user decision, cached lookups written before the fixes are refreshed, and infaunal bivalves are classified before the depth rule. The branch ships as patch release 1.6.2 and is deployed; no offline-DB rebuild is needed.

**Architecture:** A shared scalar normaliser, `.scalar_chr()` in `validation_utils.R`, turns zero-length and NA database fields into `NA_character_` where they are read. `database_lookups.R` gains two pure helpers, `to_cm()` and `worms_body_size_cm()`, that read the unit child record of each WoRMS "Body size" row. `taxonomic_api_utils.R` gains three pure helpers: `singularize_common_name()`, `fishbase_species_in_region()` and `resolve_fishbase_name()`. The last one returns a `match_confidence`, which `classify_species_api()` passes on. The orchestrator gains `lookup_species_traits_safely()`. It turns one species' error into a warning plus an error row, and both the Trait Research loop and `batch_lookup_traits()` call it. `harm_config_hash()` hashes a `TRAIT_LOOKUP_REVISION` (Task 5), so cache envelopes written before these fixes are refreshed. The `infaunal_bivalves` taxon rule moves in `TRAIT_VOCAB` from the after-depth group to the before-depth group (Task 6), a data change with no code-meaning change. All tests are offline and mock the network. The fixture re-capture and the new live asserts are network steps behind `RUN_LIVE_TESTS=true`.

**Tech Stack:** R 4.4.1, shiny 1.11.1 (`testServer`), testthat 3.3.2 (`local_mocked_bindings`), withr 3.0.2, tibble, worrms 0.4.3, rfishbase 5.0.3, httr. Windows dev box (Git Bash / PowerShell), Linux deploy target (laguna.ku.lt, shiny-server).

**Spec:** `docs/superpowers/specs/2026-09-26-fix-c-trait-pipeline-correctness-design.md`. This plan covers:
- section 2 rows F33 (lookup side), F32, F78, F79, F9, F10 and F11;
- section 4 C2.1-C2.7 (not C2.8 SHARK, which is C-6b);
- the `test-trait-lookup-correctness.R` table in section 5, except its SHARK row, plus the fixture paragraph under it;
- section 6 Rollout item 2 ("PR C-6a (lookups): C2.1-C2.7 and fixture re-capture");
- section 8 acceptance criterion 2 (the lookup reproductions) and criterion 5 ("no batch abort").

It also implements one user decision outside the spec's C-6a scope: the infaunal-bivalve EP rule runs before the depth rule (Task 6; Deviations, item 13). It also uses `docs/superpowers/specs/2026-09-26-fix-overview.md` for the shared rules. It does not cover C3 provenance and confidence (C-8), nor the harmonizer-side F33, which C-5 already fixed (see Deviations, item 1).

> **Execution rulings (plan-time facts and the user's decisions of 2026-09-29; the controller confirms 2 at release time). These override the tasks below where they conflict:**
> 1. **Release flow:** the fix PR (Tasks 0-7) is squash-merged WITHOUT a version bump or a CHANGELOG regeneration. The release then runs as its own PR, cut from the updated master (Task 8):
>    - `scripts/version_bump.R`;
>    - CRLF -> LF;
>    - `GIT_BRANCH=master`;
>    - the `R/config.R` fallback;
>    - `scripts/generate_changelog.R`;
>    - re-insert every `docs/releases/<ver>-*.md` under its header (`1.5.0-results-changed.md`, `1.5.2-notes.md`, `1.5.3-notes.md`, `1.6.0-notes.md`, `1.6.1-notes.md` if release PR #18 merged it, and the new `1.6.2-notes.md`);
>    - exactly one trailing newline;
>    - merge, then tag.
> 2. **Version:** the next PATCH. When this plan was written, 1.6.1 was taken by release PR #18 (`release/1.6.1`, open; it ships the #17 deploy fix). This plan therefore uses **1.6.2** / `v1.6.2`, name "Trait Lookups". The controller rules at release time. If #18 was abandoned, use 1.6.1 and replace every `1.6.2` below.
> 3. **No production offline-DB rebuild.** C-6a does not change what the offline-DB writer outputs. `scripts/initialization/build_offline_trait_db.R` sources only `harmonization_config.R`, `validation_utils.R`, `harmonization.R` and `offline_db_rebuild.R` (lines 31-39). It calls none of the lookups changed here, and it does not use `.scalar_chr()` or `harm_config_hash()`. Adding a helper to `validation_utils.R`, and a constant plus one hash line to `harmonization.R` (Task 5), leaves every written row identical. The Task 6 rule move does not reach the writer either: its only EP path is `harmonize_fuzzy_habitat()` (build script line 286), which uses `classify_by_patterns()` and never `apply_taxon_rules()` or `harmonize_environmental_position()`; the only caller of `harmonize_environmental_position()` is the live orchestrator (`orchestrator.R` ~L1295). Task 9 has no rebuild step. If a reviewer's change makes C-6a touch the writer after all, **STOP - ask the user** before deploying: a rebuild is `cd /srv/shiny-server/EcoNeTool && Rscript scripts/initialization/build_offline_trait_db.R` on laguna.
> 4. **Cache refresh (decided 2026-09-29: include Task 5).** The codes in `cache/taxonomy/<species>.rds` were harmonized from the wrong sizes: a WoRMS-sized invertebrate got MS from a size 10x too small, e.g. blue mussel MS3 instead of MS5. Without a key change they would stay valid for 30 days, and phylogenetic imputation also reads them as relatives.
>    - Task 5 adds `TRAIT_LOOKUP_REVISION = 2L` to `harm_config_hash()`, so every pre-C-6a envelope becomes a miss and is refreshed on first read. This is the same mechanism C-5 used for the vocabulary. (Task 6's `TRAIT_VOCAB` edit changes the hash too, because `harm_config_hash()` hashes `get_trait_vocab()`.)
>    - Old `<name>.classify.rds` files are never read again under the new region-suffixed key (Task 2). They are harmless orphans.
> 4a. **Body size (decided):** any length dimension counts, as today (deviation 4). **F9 default (decided):** the unmatched-class "Fish" guess stays "low" (deviation 5).
> 4b. **EP order (decided: option (b)).** Only the `infaunal_bivalves` rule moves before the depth rule (Task 6); fish and every other rule keep their order. **No `trait_vocab_version` bump:** no code changes meaning, only one taxon's classification moves. The cache is refreshed anyway (the vocabulary and `TRAIT_LOOKUP_REVISION` are both in `harm_config_hash()`). The offline DB is unaffected (Execution ruling 3), and a bump would make the vocab gate skip the production DB until a rebuild that would change nothing. The ML gate reads the version only for MB.
> 5. Every outward step is **STOP - ask the user**: push, PR, merge, tag, and anything on laguna. Production `data/` must never be deleted.
> 6. **Network steps** (Task 4 Steps 5-6) call the live WoRMS / FishBase APIs from the dev box. They are marked **NETWORK**, not STOP, and each has an offline fallback. The offline suite never touches the network.

## Global Constraints

- **Branch:** `fix/c6a-trait-lookups`, cut from `master` after release PR #18 has merged and `v1.6.1` is tagged. If #18 is still open, cut it from `master` at `4486cca`: #17 and #18 touch no file under `R/`, and the controller rules on this. Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- **Spec C2.1:** "every taxonomy field is normalised with `.scalar_chr(x)`, which returns `NA_character_` if the length is 0, else `x[1]`"; "The batch loop ... wraps *each* `lookup_species_traits` call in `tryCatch`. The handler calls `warning(sprintf("[trait_research] lookup failed for '%s': %s", sp, msg))` and appends a row with `species`, all codes NA and `error = msg`. The outer tryCatch ... remains only for setup failures."
- **Spec C2.2:** read `children[[i]]`, pick `measurementValue` where `measurementType == "Unit"`, and convert with `to_cm(value, unit)`: mm ×0.1, cm ×1, m ×100, µm ×1e-4. With no unit child, fall back to the qualitative row, then to the class heuristic, `warning("[worms] no unit for '%s' body size; assuming %s from class")`. Take the max over converted rows. `traits$size_unit_source` is one of "child", "qualitative" or "class_heuristic".
- **Spec C2.3:** `traits$max_weight_g <- weight` (drop `* 1000`).
- **Spec C2.4:** "Guard with `is.null(worms_data) || NROW(worms_data) == 0`. Access `worms_data$phylum[1]` and `worms_data$class[1]`. The error handler adds `warning()` while keeping `result$error <<-`."
- **Spec C2.5:** `result$confidence <- confidence_to_label(0.66)` ("medium") when the WoRMS classification succeeds, before the name-based overrides, which "correctly keep 'low'". "Delete the dead `is.null` branch."
- **Spec C2.6:** keep the exact rows (`tolower(ComName) == tolower(query)`) if there are any, else the substring rows. Grading:
  - one unique species gives "high";
  - several species, of which exactly one matches the region, give "medium";
  - otherwise the first species by `Species` sort order gets "low", plus `warning("[fishbase] '%s' ambiguous: %d candidates")`.

  The cache key becomes `<clean_name>__<region|any>.classify.rds`.
- **Spec C2.7:** try the original string first, as a scientific name and then as a common name. Try the singularised form only if both return nothing. "There is no name-shape gate."
- **Spec section 5:** "All offline tests use `local_mocked_bindings` on synthetic tibbles or recorded fixtures. Anything touching the network is gated with `skip_if_no_live_tests()` and wrapped in `with_timeout()`."
- **Spec fixture paragraph:** the fixtures are "re-captured with `capture_fixtures.R` under `RUN_LIVE_TESTS=true`, and the diff is reviewed in the PR. `test-trait-lookup-live.R` gains asserts Mytilus ≥ 10 cm and cod weight < 1e6 g."
- **CLAUDE.md conventions:**
  - `warning()`, not `message()`, in error handlers;
  - `<<-` in error closures;
  - `app_path()` for runtime paths;
  - `skip_if()` / `skip_if_not_installed()`, never `if (cond) expect_*()`;
  - `<-`, 120-char lines, no tabs or trailing whitespace;
  - live tests behind `RUN_LIVE_TESTS=true` with `with_timeout()`.
- **Parse-check** every edited `.R` file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='<path>'); cat('OK\n')"`.
- **Tests:**
  - One file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_file('tests/testthat/<file>', reporter = 'silent', stop_on_failure = FALSE)); cat('tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n')"`.
  - Full suite in the FOREGROUND (about 12 min): `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`.
  - Run one R process at a time (16 GB RAM).
  - `tests/run_all_tests.R` needs `dggridR`; do not use it.
- **Writing files:** write R files and multi-line replacements with the Write/Edit tools only, never shell heredocs. In R source every regex backslash is doubled (`"\\b"`). The code blocks below are exactly what goes into the files. New R code is ASCII except for literal `→` / `✓` / `✗` in `taxonomic_api_utils.R` progress messages, where the file already uses them. The micro sign is built with `intToUtf8(0xB5)`.
- **Staging and forbidden files:**
  - `git add` explicit paths only.
  - Never stage `WBGIFSV5ISSUE70.pdf`, anything under `config/` or `cache/`, or a `*-laguna-safeBackup-*` copy.
  - Never create, read, copy, move or delete `R/config-laguna-safeBackup-0001-CONTAINS-freshwaterecology-API-KEY.R.bak` or any other `*.bak` / `*safeBackup*` file.
  - Never write `cache/offline_traits.db` in the repo.
  - Every test that makes a cache runs in a temp directory (`withr::local_tempdir()`).
- **Verification basis:**
  - Every code block below was applied to a scratch copy of master. The copy held explicit files only, via `git archive master -- <file list>`, never `R/` wholesale.
  - **`test-trait-lookup-correctness.R`:** all 28 tests fail on master and all 28 pass after Tasks 1-3 (107 passing expectations).
  - **Task 5's test:** it fails before Task 5 and passes after it.
  - **Task 6's tests:** the Mya test fails before the rule move (EP3) and passes after it (EP4). The shallow-fish guard passes before and after, as intended. End state of the file: 31 tests, 0 failing, 116 passing expectations; on master all 30 non-guard tests fail.
  - **Regression files, identical results before and after:** `test-trait-lookup-unit.R`, `test-trait-vocabulary.R`, `test-trait-cache-config-hash.R`, `test-rebuild-observer.R`, `test-layer4-ui.R`, `test-trait-table-escaping.R`, `test-hotfix.R`, `test-trait-lookup-species.R`, `test-layer1-quality.R`, `test-ui-inputs-have-handlers.R`, `test-harmonization-settings-server.R` and `test-harmonization-config-io.R` all have 0 failures. `test-offline-traits.R` has the same single failure before and after on the scratch copy only, because the copy has no `data/external_traits/` stubs. `test-trait-lookup-live.R` skips all its tests offline.
  - **Lint:** no new lints from line length, trailing whitespace or assignment.
  - **Not run in planning:** the full suite (it needs the whole tree) and the network steps (Task 4 Steps 5-6).
- **Live API calls during planning:** the live WoRMS / FishBase APIs were called only to inspect response shapes (2026-09-29):
  - `wm_attr_data()` for Mytilus, Gadus, Carcinus, Phoca, Clupea and Calanus;
  - `wm_classification()` for Calanus and Acartia;
  - `validate_names()` and `common_to_sci()` on the names in the tests;
  - `faoareas()` / `ecosystem()` for Gadus;
  - `species("Gadus morhua")$Weight`.

## Deviations from the spec (and why)

1. **F33 in the harmonizers is already done** (C-5). `apply_taxon_rules()` treats zero-length ranks as absent, and `test-trait-vocabulary.R` "a zero-length taxonomy field does not abort the harmonizers (F33)" passes on master. C-6a covers the lookup side:
   - `database_lookups.R`'s taxonomy extraction: the old `if (length > 1)` trimmed long vectors but passed `character(0)` through;
   - the size block's `!is.null(class) && tolower(class) %in% ...`, whose `if (NA)` errors on R >= 4.3;
   - the per-species batch `tryCatch`.
2. **`.scalar_chr()` lives in `validation_utils.R`**, not in `database_lookups.R`. It is shared: `database_lookups.R` and `taxonomic_api_utils.R` use it, and `validation_utils.R` is sourced first everywhere, including by tests and the build script. It also maps `""` and NA to `NA_character_`, and unlists a length-1 list. Downstream consumers already treat NA as missing:
   - `%||%` does, so `prepare_ml_features()` and `orchestrator.R`'s `%||% "?"` need no change;
   - `route_trait_databases()`, `apply_taxon_rules()` and `calculate_taxonomic_distance()` do too. With `character(0)` the last one errored on `is.na(val) || ...`. Task 1 pins all of this.
3. **WoRMS units beyond the spec's list**, all confirmed on the live API:
   - **(a) Weights:** a "Body size" row whose unit child is not a length (Phoca has kg weight rows next to its cm lengths) is skipped, never read as a length. `to_cm()` returns NA for it.
   - **(b) Micro sign:** the micro sign and Greek mu both map to `um`.
   - **(c) `size_unit_source`:** it is the source of the row that gave the maximum.
   - **(d) The removed `max_length_mm` field:** it held the raw value in whatever unit (a cod's "200 mm" was 200 cm), and nothing reads it.
   - **(e) Row types:** only "Body size" rows count, not "Body size (qualitative)". The old `grepl("body size")` matched both; the qualitative text never parsed as a number, so no result changes.
4. **Any length dimension counts** (spec: silent). Live rows carry `Dimension` = length, breadth or prosome length under the `Type` child. The maximum over all length-unit rows is taken, exactly as today's code does, so Carcinus stays 7.3 cm, from its 73 mm carapace breadth. Filtering to `Dimension == length` was a science choice; the user kept "any length dimension" (User decision 2).
5. **F9: the unmatched-class "Fish" default stays "low"** (spec: "medium" unconditionally; confirmed by the user, User decision 4). `classify_by_taxonomy()` returns "Fish" for a class it does not recognise (e.g. a nematode's Enoplea) and when there is no class at all. That is a pattern guess, not WoRMS evidence, which is the spec's own reason for keeping the name overrides "low". The default now carries `attr(, "default") = TRUE`, `classify_species_api()` gives it "low", and the attribute is stripped before the value is stored.
6. **F10 region check rewritten (finding).** rfishbase 5.0.3 has no `distribution()` (checked against `getNamespaceExports("rfishbase")`). The old region filter called it for every candidate and always fell through to "first match" with a warning. `fishbase_species_in_region()` uses `faoareas()$FAO` and `ecosystem()$EcosystemName` instead (e.g. "Baltic Sea", "North Sea").
   - Candidates are distinct `Species`, so one species behind several ComName rows is one candidate.
   - The region loop stops at the second in-region match.
   - At most 25 candidates are region-checked (two queries each). A broad name ("cod": 286 species live) is "low" without 572 queries.
7. **F10 in `query_fishbase()`:** `resolve_fishbase_name()` returns `match_confidence`, which `query_fishbase()` adds to its result. `classify_species_api()` uses it instead of the hard-coded "high". A scientific-name hit (`validate_names()`) is "high".
8. **F11 order verified live:** `rfishbase::validate_names()` returns NA for "Cod", "Atlantic cod", "Herring", "Sandeels" and "Ammodytes", and returns the name itself for "Pollachius virens". So scientific-first never hides a common name.
9. **The batch wrapper is `lookup_species_traits_safely()` in `orchestrator.R`** (spec: an inline tryCatch in the server loop). It is a single tested unit, and `batch_lookup_traits()` uses it too.
   - Its error row carries all eight trait columns plus `source` NA, `confidence` "none" and `error`. That lets a batch whose only species failed still render the table (`format_trait_badge` is mapped over MS..ST).
   - The Trait Research table shows the `error` column, escaped. The completion notification counts "Failed" and turns into a warning notification when that count is not 0.
   - The warning keeps the spec's `[trait_research]` prefix.
10. **`query_fishbase()`'s outer error handler and the new `validate_names()` / `common_to_sci()` / region handlers now `warning()`** (CLAUDE.md). The first previously only called `message()` through `update_progress`.
11. **Fixture re-capture is selective:** `capture_fixtures.R worms fishbase`.
    - `capture_fixtures.R` gains a `RUN_LIVE_TESTS=true` guard (spec: "under `RUN_LIVE_TESTS=true`"), section arguments, and Phoca, whose fixture used to be captured ad hoc.
    - EcoBase fixtures, which the edge-contract and import tests use, stay untouched.
    - The `sealifebase_*` and `traits_*` fixtures stay untouched. `traits_*` depend on the local offline DB and cache, and are only format-tested.
12. **Addition (User decision 1):** `TRAIT_LOOKUP_REVISION` in `harm_config_hash()`, Task 5.
13. **EP order: infaunal bivalves before depth** (User decision 3, option (b); this deviates from the spec's C1.3 EP order and from C-5 plan deviation 5). The spec order is text -> pelagic taxon rules -> depth -> the other taxon rules -> EP3. Now the `infaunal_bivalves` rule (Mollusca / Bivalvia -> EP4, flag `infaunal_bivalves`) moves from `TRAIT_VOCAB$taxon_rules$environmental` to the end of `$environmental_pelagic`, the group that runs before the depth rule.
    - Reason: a shallow depth range says where a bivalve lives, not whether it lives in the sediment, so Mya or Macoma with only OBIS depths became EP3.
    - Explicit habitat text still wins, and the flag still switches the rule off.
    - Fish order rules and every other rule keep their place, so a shallow cod or herring stays EP3.
    - The group keeps its name. Renaming it would touch `apply_taxon_rules()` callers and C-5's tests for no behavioural gain; the config comment says what the group now holds.
    - This is a data change in `TRAIT_VOCAB`, not a private regex, so the C-5 static guard still passes.
    - No `trait_vocab_version` bump (Execution ruling 4b). No offline-DB rebuild (Execution ruling 3).

## Rulings on the items routed to C-6a

1. **Copepoda under Hexanauplia: not reproduced; no rule change.** On 2026-09-29, `wm_classification()` returned rank `Class = Copepoda` for Calanus finmarchicus (104464) and Acartia clausi (104251). Branchiopoda (cladocerans) is also a class and is already in the rule. The `class`-only taxon rules therefore work, and the copepod at 10-20 m gets EP1 (pinned by C-5). Task 4 adds a live drift check: Acartia's class is "Copepoda". If WoRMS moves Copepoda again, the fix is a `TRAIT_VOCAB` change (a subclass rank), never a private regex. No subclass extraction now (YAGNI: nothing would read it).
2. **"Run the environmental taxon rules before the depth fallback" (CodeRabbit, `harmonization.R` ~L764): partly adopted, by user decision.** It is harmonizer ordering, not lookup correctness, and C-5 kept the spec order deliberately (C-5 deviation 5).
   - **Science impact:** OBIS depths (`orchestrator.R` ~L1131) reach `harmonize_environmental_position()` for any taxon. So an infaunal bivalve (Macoma, Mya, Cerastoderma) with no burrowing habitat text and a mean depth < 50 m got EP3 (epibenthic) from the depth rule before the `infaunal_bivalves` EP4 rule was reached. EP_MS then priced it as exposed epibenthic prey.
   - Moving all rules before depth would also send shallow and deep fish to their order rules instead of EP3 / EP2.
   - The user chose option (b): only the bivalve rule moves (Task 6, deviation 13). Fish keep the spec order.
3. **`lookup_worms_traits()` derives habitat "benthic" from functional group "benthos": no change.** Since C-5, neither word sets an EP ("bare 'benthic' / 'benthos' say nothing about EP", `test-trait-vocabulary.R`), so it is inert.

## User decisions (decided 2026-09-29)

1. **Cache refresh (Execution ruling 4): include Task 5.** Every `cache/taxonomy` envelope is refreshed on first use. The cost is one live lookup per species on its next request, and phylogenetic imputation has fewer cached relatives until they refill (as after C-5).
2. **Body-size dimension (deviation 4): any length dimension**, today's behaviour. The rejected alternative was `Dimension == length` only.
3. **EP rules before depth (routed item 2): option (b).** Only `infaunal_bivalves` moves into the pre-depth group (Task 6). The rejected options were (a) keeping the order and (c) moving all environmental rules, which would also change fish.
   - No `trait_vocab_version` bump (Execution ruling 4b).
   - No offline-DB rebuild: the writer never calls the live EP cascade (Execution ruling 3).
4. **F9 default (deviation 5): the unmatched-class "Fish" guess stays "low".**

## Review Focus

- **A broad common name with a model region** ("cod" matches 286 FishBase species): the lookup must finish quickly, return "low" with one warning, and not issue hundreds of FishBase queries. Pinned by Task 2 test "a broad common name is low without one region query per candidate (F10)".
- **WoRMS body-size rows that are weights or other dimensions** (Phoca: 87-170 kg rows next to 148-186 cm rows): a weight is never read as a length, and the maximum length wins. Pinned by Task 1 test "weight rows (kg) never become a length (F32, Phoca)"; the dimension question is User decision 2.
- **A WoRMS record with a missing rank flowing downstream** (no Class rank): the lookup succeeds, and routing, the three harmonizers and the phylogenetic distance all accept the NA. Pinned by Task 1 test "a WoRMS classification without a Class rank does not abort the lookup (F33)".
- **A batch whose only species fails:** the results table and summary still render, the row shows the error, and nothing is left in progress. Pinned by Task 3 test "a batch whose only species fails still renders its table and summary (F33, testServer)".
- **Caches written before the upgrade** (`cache/taxonomy/*.rds` with the 2 cm mussel's MS3; the region-less `*.classify.rds`): they are not served as current. Pinned by Task 5 test "harm_config_hash changes with the lookup revision, so pre-C-6a envelopes are misses", and by Task 2 test "the classification cache key carries the region ..." (old keys are never read).
- **An infaunal bivalve whose only EP evidence is a shallow OBIS depth** (Mya arenaria, 0-10 m): EP4, while a shallow cod or herring keeps the depth rule's EP3. Pinned by Task 6 tests "an infaunal bivalve with only a shallow depth range is endobenthic (EP4)" and "shallow and deep fish keep the depth rule before their order rules".

## Interfaces produced (C-6b, C-8 and later tasks rely on these)

| Name | Signature / value | Where |
|---|---|---|
| `.scalar_chr(x)` | `-> character(1)`; `NA_character_` for NULL / length 0 / NA / "" | `R/functions/validation_utils.R` |
| `to_cm(value, unit)` | vectorised `-> numeric` cm; NA for a non-length or missing unit | `R/functions/trait_lookup/database_lookups.R` |
| `worms_body_size_cm(attributes, class_name = NA_character_, species_name = "")` | `-> NULL` or `list(max_length_cm, size_unit_source)`; source in "child", "qualitative", "class_heuristic" | same |
| `lookup_worms_traits()` result | `traits$phylum/class/order/family/genus` are `character(1)` (NA when unreported); `traits$size_unit_source` new; `traits$max_length_mm` removed | same |
| `singularize_common_name(name)` | `-> character(1)` | `R/functions/taxonomic_api_utils.R` |
| `fishbase_species_in_region(species, region)` | `-> logical(1)` | same |
| `resolve_fishbase_name(species_name, geographic_region = NULL, update_progress = function(msg) invisible(NULL), max_region_checks = 25L)` | `-> NULL` or `list(valid_name, match_confidence)`, confidence in "high", "medium", "low" | same |
| `query_fishbase()` result | gains `match_confidence` | same |
| `classify_species_api()` | cache file `<clean_name>__<region or "any">.classify.rds`; `confidence` from `match_confidence` (FishBase), "medium" / "low" (WoRMS) | same |
| `classify_by_taxonomy(taxonomy)` | the no-match "Fish" carries `attr(, "default") = TRUE` | same |
| `lookup_species_traits_safely(species_name, ...)` | `-> lookup_species_traits()` frame, or a 1-row frame: `species`, `MS`..`ST` NA, `source` NA, `confidence` "none", `error` | `R/functions/trait_lookup/orchestrator.R` |
| `TRAIT_LOOKUP_REVISION` (Task 5) | `2L`, hashed into `harm_config_hash()` as `.lookup_revision` | `R/functions/trait_lookup/harmonization.R` |
| `TRAIT_VOCAB$taxon_rules$environmental_pelagic` (Task 6) | now ends with the `infaunal_bivalves` rule (Mollusca / Bivalvia -> EP4); `$environmental` holds only the fish rules; `trait_vocab_version` stays 2L | `R/config/harmonization_config.R` |

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `R/functions/validation_utils.R` | Modify (Task 1) | `.scalar_chr()` |
| `R/functions/trait_lookup/database_lookups.R` | Modify (Task 1) | FishBase grams (F78); AlgaeBase tibble access + warning (F79); `to_cm()`, `worms_body_size_cm()`; WoRMS ranks via `.scalar_chr()` (F33, F32) |
| `R/functions/taxonomic_api_utils.R` | Modify (Task 2) | `singularize_common_name()`, `fishbase_species_in_region()`, `resolve_fishbase_name()`; `query_fishbase()` uses them; `classify_species_api()` cache key, FishBase and WoRMS confidence; `classify_by_taxonomy()` default marker (F9, F10, F11) |
| `R/functions/trait_lookup/orchestrator.R` | Modify (Task 3) | `lookup_species_traits_safely()`; `batch_lookup_traits()` uses it |
| `R/modules/trait_research_server.R` | Modify (Task 3) | Per-species safe lookup; `error` in raw details and the table; "Failed" count |
| `R/functions/trait_lookup/harmonization.R` | Modify (Tasks 5, 6) | `TRAIT_LOOKUP_REVISION` in `harm_config_hash()`; the EP cascade's step comments |
| `R/config/harmonization_config.R` | Modify (Task 6) | `infaunal_bivalves` rule moved into the pre-depth group |
| `tests/testthat/test-trait-lookup-correctness.R` | Create (Task 1), append (Tasks 2, 3, 5, 6) | All C-6a offline tests |
| `tests/testthat/capture_fixtures.R` | Modify (Task 4) | `RUN_LIVE_TESTS` guard, section arguments, Phoca |
| `tests/testthat/fixtures/worms_*.rds` (5), `fishbase_*.rds` (2) | Re-capture (Task 4, NETWORK) | Fixtures without the F32 / F78 bugs |
| `tests/testthat/test-trait-lookup-unit.R` | Modify (Task 4) | Fixture asserts (Mytilus ≥ 10 cm, cod < 1e6 g) with an actionable skip on pre-C-6a fixtures |
| `tests/testthat/test-trait-lookup-live.R` | Modify (Task 4) | Live asserts: Mytilus cm, cod grams, Pollachius virens, Copepoda drift |
| `CONTRIBUTING.md` | Modify (Task 7) | Convention: normalise database fields with `.scalar_chr()`; batch lookups go through the safe wrapper |
| `VERSION`, `R/config.R`, `app.R`, `README.md`, `CHANGELOG.md`, `docs/releases/1.6.2-notes.md` | Modify / Create (Task 8, release PR) | 1.6.2 |

Not touched (noted for reviewers):
- `ml_trait_prediction.R` (NA-safe through `%||%`) and `phylogenetic_imputation.R` (NA-safe);
- the harmonizer code (C-5); Task 6 changes only `TRAIT_VOCAB` data and two comments;
- `build_offline_trait_db.R` (Execution ruling 3);
- `ecopath_import_server.R`: it keeps passing `geographic_region`, and its high/medium filter now accepts WoRMS classifications (the point of F9);
- legacy `tests/test_taxonomic_*.R` scripts outside testthat that call `classify_by_taxonomy()`: they compare with `==`, which the attribute does not affect.

---

### Task 0: Branch, preconditions and baseline

**Files:** none modified.

**Interfaces:**
- Consumes: master with C-5 (1.6.0) and #17 merged. `apply_taxon_rules()`, `harm_config_hash()`, `read_cache_field(..., vocab_version =)` and `route_trait_databases()` exist.
- Produces: branch `fix/c6a-trait-lookups`, and a recorded `BASELINE pass N fail 0 skip M` line for Task 7.

- [ ] **Step 1: Confirm preconditions and create the branch**

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git checkout master && git pull --ff-only
grep -n "^VERSION=" VERSION
git tag -l "v1.6.*"
gh pr view 18 --json state -q .state
grep -n "^apply_taxon_rules <- function\|^harm_config_hash <- function" R/functions/trait_lookup/harmonization.R
grep -n "^route_trait_databases <- function\|^batch_lookup_traits <- function" R/functions/trait_lookup/orchestrator.R
grep -c "weight_kg \* 1000\|worms_data\[\[1\]\]\$phylum" R/functions/trait_lookup/database_lookups.R
git checkout -b fix/c6a-trait-lookups
```

Expected:
- `VERSION=1.6.1` with tag `v1.6.1` and PR 18 `MERGED`, or else `VERSION=1.6.0`, tag `v1.6.0` only and PR 18 `OPEN` (Execution ruling 2: the controller decides whether to proceed);
- the four definitions printed;
- the count `2` (both bugs still present).

If a definition is missing or the count is not 2, **STOP** and tell the user.

- [ ] **Step 2: Record the baseline**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('BASELINE pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: `fail 0 ... error 0`, about `pass 2455 ... skip 51`. Write the line into your notes. If the baseline is red, stop and report.

---

### Task 1: WoRMS, FishBase and AlgaeBase raw values (F33 lookup side, F32, F78, F79)

**Files:**
- Modify: `R/functions/validation_utils.R`: add `.scalar_chr()` after `safe_get()`.
- Modify: `R/functions/trait_lookup/database_lookups.R`:
  - `lookup_fishbase_traits()` weight;
  - `lookup_algaebase_traits()`;
  - new `to_cm()` and `worms_body_size_cm()` before `lookup_worms_traits()`;
  - the taxonomy and body-size blocks of `lookup_worms_traits()`.
- Create: `tests/testthat/test-trait-lookup-correctness.R`

**Interfaces:**
- Consumes: `%||%`, `safe_get()`, `is_valid_value()`, `with_mocked_function()` (helper-fixtures), `route_trait_databases()`, the harmonizers, `calculate_taxonomic_distance()`.
- Produces: `.scalar_chr()`, `to_cm()`, `worms_body_size_cm()`. `lookup_worms_traits()$traits` gains `size_unit_source` and loses `max_length_mm`, and its ranks are `character(1)`. `lookup_fishbase_traits()$traits$max_weight_g` is in grams.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-trait-lookup-correctness.R` with exactly this content:

```r
# Sub-project C, PR C-6a (spec C2.1-C2.7): the trait lookups return correct
# raw values. A WoRMS classification without a Class rank aborted the whole
# Trait Research batch (F33), WoRMS body sizes ignored the unit WoRMS gives
# with them (F32), FishBase weights were multiplied by 1000 (F78), the
# AlgaeBase fallback indexed a tibble like a list (F79), a WoRMS
# classification was always "low" (F9), the first substring common-name
# match was "high" and its cache ignored the region (F10), and binomials
# were singularised before they were looked up (F11).
# Every test here is offline: the network calls are mocked.

source_app_dependencies()

# ---------------------------------------------------------------------------
# Fixtures shaped like the live responses (probed 2026-09-29)
# ---------------------------------------------------------------------------

# worrms::wm_attr_data(): one row per measurement; the unit (and deeper
# qualifiers such as Type / Dimension) sit in the `children` list column.
worms_attr <- function(type, value, unit = NA_character_) {
  unit <- rep_len(unit, length(type))
  out <- tibble::tibble(AphiaID = 1L, measurementType = type, measurementValue = as.character(value))
  out$children <- lapply(unit, function(u) {
    if (is.na(u)) return(data.frame())
    data.frame(measurementType = "Unit", measurementValue = u, stringsAsFactors = FALSE)
  })
  out
}

# "micrometre" as WoRMS writes it, with the micro sign (kept out of the source
# as a literal so the file stays ASCII).
micro_m <- paste0(intToUtf8(0xB5), "m")

# One AphiaRecordsByName row (worms_records_by_name_http()).
worms_record <- function(name) {
  data.frame(AphiaID = 1L, scientificname = name, rank = "Species", isMarine = 1L,
             isBrackish = 0L, isFreshwater = 0L, stringsAsFactors = FALSE)
}

# Run lookup_worms_traits() against mocked WoRMS responses. `ranks` is a named
# character vector rank -> name, as worrms::wm_classification() returns it.
mock_worms_lookup <- function(name, ranks, attrs) {
  testthat::local_mocked_bindings(
    wm_classification = function(id, ...) {
      data.frame(rank = names(ranks), scientificname = unname(ranks), stringsAsFactors = FALSE)
    },
    wm_attr_data = function(id, ...) attrs,
    .package = "worrms"
  )
  with_mocked_function(globalenv(), "worms_records_by_name_http", function(...) worms_record(name),
                       lookup_worms_traits(name))
}

# ---------------------------------------------------------------------------
# F33 - zero-length WoRMS ranks
# ---------------------------------------------------------------------------

test_that(".scalar_chr turns a missing rank into NA and keeps the first value", {
  expect_identical(.scalar_chr(character(0)), NA_character_)
  expect_identical(.scalar_chr(NULL), NA_character_)
  expect_identical(.scalar_chr(NA), NA_character_)
  expect_identical(.scalar_chr(""), NA_character_)
  expect_identical(.scalar_chr(c("Bivalvia", "Other")), "Bivalvia")
  expect_identical(.scalar_chr(list("Mollusca")), "Mollusca")
})

test_that("a WoRMS classification without a Class rank does not abort the lookup (F33)", {
  skip_if_not_installed("worrms")
  expect_warning(
    res <- mock_worms_lookup("Enoplus brevis",
                             c(Kingdom = "Animalia", Phylum = "Nematoda", Order = "Enoplida"),
                             worms_attr("Body size", 5)),
    "no unit for 'Enoplus brevis' body size; assuming mm from class"
  )
  expect_true(res$success)
  expect_null(res$error)
  expect_identical(res$traits$phylum, "Nematoda")
  expect_identical(res$traits$class, NA_character_)
  expect_identical(res$traits$order, "Enoplida")
  expect_equal(res$traits$max_length_cm, 0.5)
  expect_identical(res$traits$size_unit_source, "class_heuristic")

  # The NA rank flows through every downstream consumer without an error.
  expect_no_error(route_trait_databases(res$traits$phylum, res$traits$class, 1L, TRUE))
  expect_no_error(harmonize_mobility(NULL, NULL, res$traits))
  expect_no_error(harmonize_environmental_position(taxonomic_info = res$traits))
  expect_no_error(harmonize_protection(NULL, res$traits))
  source(file.path(get_app_root(), "R/functions/phylogenetic_imputation.R"), local = TRUE)
  expect_no_error(calculate_taxonomic_distance(res$traits, list(phylum = "Nematoda", class = "Enoplea")))
})

# ---------------------------------------------------------------------------
# F32 - WoRMS body-size units
# ---------------------------------------------------------------------------

test_that("to_cm converts lengths and refuses non-length units", {
  expect_equal(to_cm(c(1, 1, 1, 1, 1, 1), c("mm", "cm", "m", micro_m, "um", "kg")),
               c(0.1, 1, 100, 1e-4, 1e-4, NA))
  expect_equal(to_cm(20, "CM "), 20)
  expect_true(is.na(to_cm(20, NA_character_)))
})

test_that("a WoRMS body size is read in the unit of its child record (F32, Mytilus)", {
  skip_if_not_installed("worrms")
  res <- mock_worms_lookup("Mytilus edulis",
                           c(Phylum = "Mollusca", Class = "Bivalvia", Order = "Mytilida"),
                           worms_attr(rep("Body size", 3), c(5.1, 10, 20), "cm"))
  expect_true(res$success)
  expect_equal(res$traits$max_length_cm, 20)
  expect_identical(res$traits$size_unit_source, "child")
})

test_that("without a unit child the class heuristic applies, with a warning (F32)", {
  attrs <- worms_attr("Body size", 20)
  expect_warning(size <- worms_body_size_cm(attrs, "Bivalvia", "Mytilus edulis"), "no unit")
  expect_equal(size$max_length_cm, 2)
  expect_identical(size$size_unit_source, "class_heuristic")
  expect_warning(fish <- worms_body_size_cm(attrs, "Teleostei", "Gadus morhua"), "assuming cm")
  expect_equal(fish$max_length_cm, 20)
})

test_that("the qualitative body-size row gives the unit before the class heuristic (F32)", {
  attrs <- worms_attr(c("Body size", "Body size (qualitative)"), c("20", "up to 20 cm"))
  expect_no_warning(size <- worms_body_size_cm(attrs, "Bivalvia", "Mytilus edulis"))
  expect_equal(size$max_length_cm, 20)
  expect_identical(size$size_unit_source, "qualitative")
})

test_that("weight rows (kg) never become a length (F32, Phoca)", {
  attrs <- worms_attr(rep("Body size", 3), c(186, 250, 160), c("cm", "kg", "cm"))
  size <- worms_body_size_cm(attrs, "Mammalia", "Phoca vitulina")
  expect_equal(size$max_length_cm, 186)
  expect_null(worms_body_size_cm(worms_attr("Body size", 87, "kg"), "Mammalia", "Phoca vitulina"))
  expect_null(worms_body_size_cm(worms_attr("Functional group", "benthos"), "Bivalvia", "Mytilus edulis"))
  expect_null(worms_body_size_cm(NULL, "Bivalvia", "Mytilus edulis"))
})

test_that("a microscopic size in micrometres converts to cm (F32)", {
  size <- worms_body_size_cm(worms_attr("Body size", 50, micro_m), "Bacillariophyceae", "Skeletonema costatum")
  expect_equal(size$max_length_cm, 0.005)
  expect_identical(size$size_unit_source, "child")
})

# ---------------------------------------------------------------------------
# F78 - FishBase weight is already in grams
# ---------------------------------------------------------------------------

test_that("FishBase Weight is stored in grams, not multiplied by 1000 (F78)", {
  skip_if_not_installed("rfishbase")
  testthat::local_mocked_bindings(
    species = function(species_list, ...) data.frame(Length = 200, Weight = 96000),
    morphology = function(species_list, ...) NULL,
    ecology = function(species_list, ...) data.frame(FoodTroph = 4.4),
    .package = "rfishbase"
  )
  res <- lookup_fishbase_traits("Gadus morhua")
  expect_true(res$success)
  expect_equal(res$traits$max_weight_g, 96000)
  expect_equal(res$traits$max_length_cm, 200)
})

# ---------------------------------------------------------------------------
# F79 - AlgaeBase fallback reads the WoRMS tibble by column
# ---------------------------------------------------------------------------

test_that("the AlgaeBase fallback reads phylum and class from the WoRMS tibble (F79)", {
  skip_if_not_installed("worrms")
  testthat::local_mocked_bindings(
    wm_records_name = function(name, ...) {
      tibble::tibble(AphiaID = 149098L, phylum = "Ochrophyta", class = "Bacillariophyceae")
    },
    .package = "worrms"
  )
  res <- lookup_algaebase_traits("Skeletonema costatum")
  expect_null(res$error)
  expect_true(res$success)
  expect_identical(res$traits$phylum, "Ochrophyta")
  expect_identical(res$traits$class, "Bacillariophyceae")
})

test_that("an AlgaeBase fallback failure warns and is recorded (F79)", {
  skip_if_not_installed("worrms")
  testthat::local_mocked_bindings(
    wm_records_name = function(name, ...) stop("(500) Internal Server Error"),
    .package = "worrms"
  )
  expect_warning(res <- lookup_algaebase_traits("Skeletonema costatum"),
                 "\\[algaebase\\] WoRMS fallback failed for 'Skeletonema costatum'")
  expect_identical(res$error, "(500) Internal Server Error")
  expect_false(res$success)
})
```

- [ ] **Step 2: Run it to verify it fails**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_file('tests/testthat/test-trait-lookup-correctness.R', reporter = 'silent', stop_on_failure = FALSE)); cat('tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n')"
```

Expected: `tests 11 failing 11` (verified on master). The failures are:
- `.scalar_chr`, `to_cm` and `worms_body_size_cm` not found;
- the Class-less classification aborting inside the size block (`[worms] error processing ...` instead of the "no unit" warning);
- `max_length_cm` 2 instead of 20;
- `max_weight_g` 9.6e7;
- `$ operator is invalid for atomic vectors` in the AlgaeBase fallback, and no `[algaebase]` warning.

- [ ] **Step 3: Add `.scalar_chr()`**

In `R/functions/validation_utils.R`, replace

```r
  if (is_valid_value(value)) value else default
}

#' Coalesce Multiple Values
```

with

```r
  if (is_valid_value(value)) value else default
}

#' First value of a lookup field as a character scalar
#'
#' A taxonomic rank that a database does not report comes back as
#' `character(0)` (e.g. a WoRMS classification without a Class rank), and a
#' zero-length value makes `!is.na(x) && grepl(...)` or `if (x == ...)` error
#' (F33). Normalise every such field once, where it is read.
#'
#' @param x Any value: NULL, a vector, a length-1 list.
#' @return `NA_character_` for NULL, zero-length, NA or "" input; otherwise
#'   the first element as character.
#' @export
.scalar_chr <- function(x) {
  if (length(x) == 0L) return(NA_character_)
  v <- as.character(unlist(x))[1]
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}

#' Coalesce Multiple Values
```

- [ ] **Step 4: FishBase weight in grams (F78)**

In `R/functions/trait_lookup/database_lookups.R`, replace

```r
    # Weight (convert kg to g)
    weight_kg <- safe_get(species_data, "Weight")
    if (is_valid_value(weight_kg)) {
      traits$max_weight_g <- weight_kg * 1000
    }
```

with

```r
    # Weight. FishBase's species.Weight is already in grams (cod: 96000 g);
    # the old "* 1000" recorded a 96 t cod (F78).
    weight_g <- safe_get(species_data, "Weight")
    if (is_valid_value(weight_g)) {
      traits$max_weight_g <- weight_g
    }
```

- [ ] **Step 5: AlgaeBase fallback (F79)**

In the same file, inside `lookup_algaebase_traits()`, replace

```r
    if (is.null(worms_data) || length(worms_data) == 0) {
      result$note <- "Species not found in WoRMS (AlgaeBase fallback)"
      return(result)
    }

    # Check if it's algae/phytoplankton
    phylum <- worms_data[[1]]$phylum
    class <- worms_data[[1]]$class
```

with

```r
    # wm_records_name() returns a tibble: length() counts its columns and
    # [[1]] is its first column, so read rows and columns explicitly (F79).
    if (is.null(worms_data) || NROW(worms_data) == 0) {
      result$note <- "Species not found in WoRMS (AlgaeBase fallback)"
      return(result)
    }

    # Check if it's algae/phytoplankton
    phylum <- .scalar_chr(worms_data$phylum[1])
    class <- .scalar_chr(worms_data$class[1])
```

then replace

```r
    if (tolower(phylum) %in% tolower(algae_phyla) || grepl("phyceae", class, ignore.case = TRUE)) {
```

with

```r
    if (isTRUE(tolower(phylum) %in% tolower(algae_phyla)) || isTRUE(grepl("phyceae", class, ignore.case = TRUE))) {
```

then replace

```r
      result$note <- "Species found but not classified as algae"
    }

  }, error = function(e) {
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
  })
```

with

```r
      result$note <- "Species found but not classified as algae"
    }

  }, error = function(e) {
    warning(sprintf("[algaebase] WoRMS fallback failed for '%s': %s",
                    species_name, conditionMessage(e)), call. = FALSE)
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
  })
```

- [ ] **Step 6: Add `to_cm()` and `worms_body_size_cm()`**

In the same file, replace the line

```r
#' Use WoRMS for taxonomic information and basic traits
```

with

```r
#' Convert a length to centimetres
#'
#' @param value Numeric (or numeric text) length(s).
#' @param unit Unit string(s): "mm", "cm", "m", "um" / micro sign + "m".
#'   Case and surrounding space are ignored.
#' @return Numeric cm; NA for a unit that is not a length (e.g. "kg", which
#'   WoRMS body-size rows also use for weights) or a missing unit.
#' @export
to_cm <- function(value, unit) {
  value <- suppressWarnings(as.numeric(value))
  u <- tolower(trimws(as.character(unit)))
  u <- gsub(paste0("[", intToUtf8(c(0xB5, 0x3BC)), "]"), "u", u)  # micro sign, Greek mu
  factor <- c(um = 1e-4, micrometre = 1e-4, micrometer = 1e-4, mm = 0.1, cm = 1, m = 100)[u]
  unname(value * factor)
}

#' Maximum body length (cm) from WoRMS Traits Portal attributes (F32)
#'
#' Every "Body size" row carries its unit as a child record
#' (`children[[i]]`, measurementType "Unit"). Rows are converted with
#' [to_cm()]; rows whose unit is not a length (weights in kg) are skipped.
#' A row without a unit child takes the unit from the "Body size
#' (qualitative)" text, then from the class (fish, mammals, reptiles and
#' birds report cm, everything else mm), with a warning.
#'
#' @param attributes Tibble from `worrms::wm_attr_data()`, or NULL.
#' @param class_name WoRMS class (scalar, may be NA).
#' @param species_name For the warning.
#' @return NULL when no body-size row converts, else
#'   `list(max_length_cm, size_unit_source)` where the source is "child",
#'   "qualitative" or "class_heuristic" (the row that gave the maximum).
#' @export
worms_body_size_cm <- function(attributes, class_name = NA_character_, species_name = "") {
  if (NROW(attributes) == 0 || !all(c("measurementType", "measurementValue") %in% names(attributes))) {
    return(NULL)
  }
  type <- as.character(attributes$measurementType)
  size_rows <- which(grepl("body size", type, ignore.case = TRUE) &
                       !grepl("qualitative", type, ignore.case = TRUE))
  if (length(size_rows) == 0) return(NULL)

  child_unit <- function(i) {
    if (!"children" %in% names(attributes)) return(NA_character_)
    ch <- attributes$children[[i]]
    if (NROW(ch) == 0 || !all(c("measurementType", "measurementValue") %in% names(ch))) {
      return(NA_character_)
    }
    .scalar_chr(ch$measurementValue[ch$measurementType == "Unit"])
  }

  values <- suppressWarnings(as.numeric(attributes$measurementValue[size_rows]))
  units <- vapply(size_rows, child_unit, character(1))
  sources <- ifelse(is.na(units), NA_character_, "child")

  no_unit <- !is.na(values) & is.na(units)
  if (any(no_unit)) {
    fallback <- NA_character_
    fallback_source <- "qualitative"
    qual <- attributes$measurementValue[grepl("body size \\(qualitative\\)", type, ignore.case = TRUE)]
    if (length(qual) > 0) {
      q <- tolower(qual[1])
      if (grepl("\\bcm\\b", q)) {
        fallback <- "cm"
      } else if (grepl("\\bmm\\b", q)) {
        fallback <- "mm"
      } else if (grepl("\\bm\\b", q)) {
        fallback <- "m"
      }
    }
    if (is.na(fallback)) {
      # Vertebrate classes whose WoRMS body sizes are conventionally in cm.
      # Bony fish span Actinopterygii / Actinopteri / Teleostei depending on
      # the classification level; mammals (seals, whales), reptiles and birds
      # too - without them a 186 cm seal records as 18.6 cm.
      cm_reporting_classes <- c(
        "actinopterygii", "actinopteri", "teleostei",
        "elasmobranchii", "holocephali", "chondrichthyes",
        "sarcopterygii", "myxini", "petromyzonti",
        "mammalia", "reptilia", "aves"
      )
      fallback <- if (isTRUE(tolower(class_name) %in% cm_reporting_classes)) "cm" else "mm"
      fallback_source <- "class_heuristic"
      warning(sprintf("[worms] no unit for '%s' body size; assuming %s from class",
                      species_name, fallback), call. = FALSE)
    }
    units[no_unit] <- fallback
    sources[no_unit] <- fallback_source
  }

  cm <- to_cm(values, units)
  ok <- !is.na(cm)
  if (!any(ok)) return(NULL)
  best <- which(ok)[which.max(cm[ok])]
  list(max_length_cm = cm[best], size_unit_source = sources[best])
}

#' Use WoRMS for taxonomic information and basic traits
```

- [ ] **Step 7: WoRMS ranks via `.scalar_chr()` (F33)**

In `lookup_worms_traits()`, replace

```r
    # Taxonomic information
    if (!is.null(classification) && nrow(classification) > 0) {
      traits$phylum <- classification$scientificname[classification$rank == "Phylum"]
      traits$class <- classification$scientificname[classification$rank == "Class"]
      traits$order <- classification$scientificname[classification$rank == "Order"]
      traits$family <- classification$scientificname[classification$rank == "Family"]
      traits$genus <- classification$scientificname[classification$rank == "Genus"]

      # Handle multiple matches
      if (length(traits$phylum) > 1) traits$phylum <- traits$phylum[1]
      if (length(traits$class) > 1) traits$class <- traits$class[1]
      if (length(traits$order) > 1) traits$order <- traits$order[1]
      if (length(traits$family) > 1) traits$family <- traits$family[1]
      if (length(traits$genus) > 1) traits$genus <- traits$genus[1]
    }
```

with

```r
    # Taxonomic information. A rank WoRMS does not report comes back as
    # character(0); .scalar_chr() makes it NA (and keeps the first of
    # several), so no guard downstream meets a zero-length value (F33).
    if (!is.null(classification) && nrow(classification) > 0) {
      rank_name <- function(rank) .scalar_chr(classification$scientificname[classification$rank == rank])
      traits$phylum <- rank_name("Phylum")
      traits$class <- rank_name("Class")
      traits$order <- rank_name("Order")
      traits$family <- rank_name("Family")
      traits$genus <- rank_name("Genus")
    }
```

- [ ] **Step 8: WoRMS body size in its own unit (F32)**

In `lookup_worms_traits()`, replace the whole body-size block (current lines ~994-1041, just before `# 2. Functional group - Extract for habitat/environmental position`):

```r
      # 1. Body size - Extract all body size measurements
      body_size_rows <- attributes[grepl("body size", attributes$measurementType, ignore.case = TRUE), ]
      if (nrow(body_size_rows) > 0) {
        # Get numeric body size values
        sizes <- suppressWarnings(as.numeric(body_size_rows$measurementValue))
        sizes <- sizes[!is.na(sizes)]

        if (length(sizes) > 0) {
          # Take maximum body size
          traits$max_length_mm <- max(sizes)

          # Detect length unit from the qualitative body-size string; WoRMS
          # embeds the unit in the value text, not as a separate field.
          qual_size <- attributes$measurementValue[grepl("body size \\(qualitative\\)",
                                                         attributes$measurementType,
                                                         ignore.case = TRUE)]
          unit <- NA_character_
          if (length(qual_size) > 0) {
            q <- tolower(qual_size[1])
            if      (grepl("\\bcm\\b", q)) unit <- "cm"
            else if (grepl("\\bmm\\b", q)) unit <- "mm"
            else if (grepl("\\bm\\b",  q)) unit <- "m"
          }

          if (is.na(unit)) {
            # Vertebrate classes whose WoRMS body-size measurements are
            # conventionally reported in cm. Bony fish span Actinopterygii /
            # Actinopteri / Teleostei depending on classification level; the
            # cartilaginous & jawless fish classes round out the fish list;
            # mammals (incl. cetaceans/pinnipeds, returned at class level as
            # Mammalia), reptiles, and birds also report cm — without them
            # a 200 cm seal silently records as 20 cm.
            cm_reporting_classes <- c(
              "actinopterygii", "actinopteri", "teleostei",
              "elasmobranchii", "holocephali", "chondrichthyes",
              "sarcopterygii", "myxini", "petromyzonti",
              "mammalia", "reptilia", "aves"
            )
            unit <- if (!is.null(traits$class) &&
                        tolower(traits$class) %in% cm_reporting_classes) "cm" else "mm"
          }

          traits$max_length_cm <- switch(unit,
                                         cm = traits$max_length_mm,
                                         mm = traits$max_length_mm / 10,
                                         m  = traits$max_length_mm * 100)
        }
      }
```

with

```r
      # 1. Body size in the unit WoRMS gives with each row (F32); weights
      #    (kg rows) are skipped. The raw value used to be divided by 10 for
      #    every non-vertebrate, so a 20 cm mussel recorded as 2 cm.
      body_size <- worms_body_size_cm(attributes, traits$class, species_name)
      if (!is.null(body_size)) {
        traits$max_length_cm <- body_size$max_length_cm
        traits$size_unit_source <- body_size$size_unit_source
      }
```

Afterwards, `grep -n "max_length_mm\|cm_reporting_classes" R/functions/trait_lookup/database_lookups.R` must print only:
- the `cm_reporting_classes` lines inside `worms_body_size_cm()`;
- the unrelated `max_length_mm = safe_get(data, "size_max")` in `lookup_freshwaterecology_traits()`.

- [ ] **Step 9: Parse-check and run the tests**

```bash
for f in R/functions/validation_utils.R R/functions/trait_lookup/database_lookups.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"; done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('test-trait-lookup-correctness.R', 'test-trait-lookup-unit.R', 'test-trait-vocabulary.R')) { r <- as.data.frame(testthat::test_file(file.path('tests/testthat', f), reporter = 'silent', stop_on_failure = FALSE)); cat(f, 'tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n') }"
```

Expected:
- two `OK` lines;
- `test-trait-lookup-correctness.R tests 11 failing 0 skipped 0 passed 49`;
- `test-trait-lookup-unit.R tests 26 failing 0 skipped 0` (the fixtures still hold the old values; no existing assert reads `max_length_mm` or the weight);
- `test-trait-vocabulary.R tests 58 failing 0 skipped 0`.

- [ ] **Step 10: Commit**

```bash
git add R/functions/validation_utils.R R/functions/trait_lookup/database_lookups.R tests/testthat/test-trait-lookup-correctness.R
git commit -m "$(cat <<'EOF'
fix(traits): C-6a WoRMS sizes in their own unit, FishBase grams, NA ranks

WoRMS body sizes are converted from the unit child record of each row
(F32; a 20 cm mussel was 2 cm, kg weight rows are no longer lengths);
missing WoRMS ranks become NA via .scalar_chr() instead of character(0),
which aborted the lookup (F33); FishBase Weight stays in grams (F78); the
AlgaeBase fallback reads the WoRMS tibble by column and warns (F79).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Name resolution and classification confidence (F9, F10, F11)

**Files:**
- Modify: `R/functions/taxonomic_api_utils.R`:
  - new helpers before `query_fishbase()`;
  - `query_fishbase()` name resolution, result and error handler;
  - `classify_species_api()`;
  - `classify_by_taxonomy()`.
- Test: `tests/testthat/test-trait-lookup-correctness.R` (append)

**Interfaces:**
- Consumes: `.scalar_chr()` (Task 1), `confidence_to_label()` (`harmonization.R`), `%||%`, `assign_functional_group()` (`functional_group_utils.R`).
- Produces: `singularize_common_name()`, `fishbase_species_in_region()` and `resolve_fishbase_name()` (signatures in Interfaces produced). `query_fishbase()$match_confidence`. `classify_species_api()` writes `<clean_name>__<region|any>.classify.rds`. `classify_by_taxonomy()`'s default carries `attr(, "default")`.

- [ ] **Step 1: Append the failing tests**

Append to `tests/testthat/test-trait-lookup-correctness.R`:

```r

# rfishbase name-resolution mocks. `common` maps a lower-case query to the
# rows common_to_sci() returns; `sci` lists names validate_names() accepts;
# `region_species` are the species whose ecosystems include the Baltic Sea.
# Returns an environment that records every call.
mock_fishbase_names <- function(common = list(), sci = character(), region_species = character(),
                                env = parent.frame()) {
  calls <- new.env()
  calls$validate <- character()
  calls$common <- character()
  calls$region <- character()
  testthat::local_mocked_bindings(
    validate_names = function(species_list, ...) {
      calls$validate <- c(calls$validate, species_list)
      if (species_list %in% sci) species_list else NA_character_
    },
    common_to_sci = function(x, ...) {
      calls$common <- c(calls$common, x)
      rows <- common[[tolower(x)]]
      if (is.null(rows)) data.frame(Species = character(), ComName = character()) else rows
    },
    faoareas = function(species_list, ...) {
      calls$region <- c(calls$region, species_list)
      data.frame(Species = species_list, FAO = "Pacific, Northwest")
    },
    ecosystem = function(species_list, ...) {
      data.frame(Species = species_list,
                 EcosystemName = if (species_list %in% region_species) "Baltic Sea" else "Sea of Okhotsk")
    },
    .package = "rfishbase",
    .env = env
  )
  calls
}

common_rows <- function(species, comname) {
  data.frame(Species = species, ComName = comname, Language = "English", stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# F9 - a WoRMS classification is "medium"
# ---------------------------------------------------------------------------

mussel_worms <- function(...) {
  list(aphia_id = 140480L, scientific_name = "Mytilus edulis", rank = "Species",
       phylum = "Mollusca", class = "Bivalvia", order = "Mytilida", family = "Mytilidae")
}

test_that("a WoRMS classification is medium confidence (F9)", {
  res <- with_mocked_function(globalenv(), "query_worms", mussel_worms,
    classify_species_api("Mytilus edulis", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$functional_group, "Benthos")
  expect_identical(res$source, "WoRMS")
  expect_identical(res$confidence, "medium")
})

test_that("a name-based override and the unmatched-class default stay low (F9)", {
  res <- with_mocked_function(globalenv(), "query_worms", mussel_worms,
    classify_species_api("Mussel worm", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$confidence, "medium")  # "worm" only overrides a Fish verdict

  nematode <- function(...) list(aphia_id = 1L, phylum = "Nematoda", class = "Enoplea")
  res <- with_mocked_function(globalenv(), "query_worms", nematode,
    classify_species_api("Enoplus brevis", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$functional_group, "Fish")  # classify_by_taxonomy's fallback, not WoRMS evidence
  expect_identical(res$confidence, "low")

  cod_as_benthos <- function(...) list(aphia_id = 1L, phylum = "Mollusca", class = "Bivalvia")
  res <- with_mocked_function(globalenv(), "query_worms", cod_as_benthos,
    classify_species_api("Baltic cod", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$functional_group, "Fish")
  expect_identical(res$confidence, "low")
})

test_that("assign_functional_group_enhanced keeps a WoRMS classification (F9)", {
  withr::local_dir(withr::local_tempdir())  # classify_species_api caches under ./cache/taxonomy
  res <- with_mocked_function(globalenv(), "query_fishbase", function(...) NULL,
    with_mocked_function(globalenv(), "query_worms", mussel_worms,
      assign_functional_group_enhanced("Xyzzy obscura", use_api = TRUE)))
  expect_identical(res, "Benthos")
})

# ---------------------------------------------------------------------------
# F10 - common names: exact match first, graded confidence, region-keyed cache
# ---------------------------------------------------------------------------

test_that("an exact common-name match wins and is high confidence (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(common = list(
    "atlantic cod" = common_rows(c("Gadus morhua", "Lepidion lepidion"), c("Atlantic cod", "North Atlantic codling"))
  ))
  res <- resolve_fishbase_name("Atlantic cod")
  expect_identical(res$valid_name, "Gadus morhua")
  expect_identical(res$match_confidence, "high")
})

test_that("one species behind several common-name rows is still one candidate (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(common = list(
    "antenna codlet" = common_rows(rep("Bregmaceros atlanticus", 2), c("Antenna codlet", "Antenna Codlet"))
  ))
  res <- resolve_fishbase_name("Antenna codlet")
  expect_identical(res$valid_name, "Bregmaceros atlanticus")
  expect_identical(res$match_confidence, "high")
})

test_that("an ambiguous common name is low confidence, deterministic, and warns (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(common = list(
    cod = common_rows(c("Gadus macrocephalus", "Eleginus nawaga"), c("Alaska cod", "Arctic cod"))
  ))
  expect_warning(res <- resolve_fishbase_name("cod"), "\\[fishbase\\] 'cod' ambiguous: 2 candidates")
  expect_identical(res$valid_name, "Eleginus nawaga")  # first by Species sort order
  expect_identical(res$match_confidence, "low")
})

test_that("exactly one candidate in the region is medium confidence (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(
    common = list(cod = common_rows(c("Gadus macrocephalus", "Eleginus nawaga", "Gadus morhua"),
                                    c("Alaska cod", "Arctic cod", "Baltic cod"))),
    region_species = "Gadus morhua"
  )
  res <- resolve_fishbase_name("cod", geographic_region = "Baltic")
  expect_identical(res$valid_name, "Gadus morhua")
  expect_identical(res$match_confidence, "medium")
})

test_that("two candidates in the region stay low (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(
    common = list(cod = common_rows(c("Gadus macrocephalus", "Gadus morhua"), c("Alaska cod", "Baltic cod"))),
    region_species = c("Gadus macrocephalus", "Gadus morhua")
  )
  expect_warning(res <- resolve_fishbase_name("cod", geographic_region = "Baltic"), "ambiguous: 2 candidates")
  expect_identical(res$match_confidence, "low")
})

test_that("a broad common name is low without one region query per candidate (F10)", {
  skip_if_not_installed("rfishbase")
  species <- sprintf("Genus%02d species", 1:30)
  calls <- mock_fishbase_names(common = list(cod = common_rows(species, paste(species, "cod"))),
                               region_species = species[1])
  expect_warning(res <- resolve_fishbase_name("cod", geographic_region = "Baltic"), "ambiguous: 30 candidates")
  expect_identical(res$match_confidence, "low")
  expect_length(calls$region, 0)
})

test_that("the classification cache key carries the region and FishBase confidence is passed on (F10)", {
  cache_dir <- withr::local_tempdir()
  fb <- function(...) {
    list(avg_weight_g = NA, max_weight_g = NA, trophic_level = NA, habitat = NA,
         min_depth_m = NA, max_depth_m = NA, match_confidence = "low")
  }
  res <- with_mocked_function(globalenv(), "query_fishbase", fb,
    classify_species_api("Atlantic cod", geographic_region = "Baltic Sea", cache_dir = cache_dir))
  expect_identical(res$confidence, "low")
  expect_true(file.exists(file.path(cache_dir, "Atlantic_cod__Baltic_Sea.classify.rds")))
  with_mocked_function(globalenv(), "query_fishbase", fb,
    classify_species_api("Atlantic cod", cache_dir = cache_dir))
  expect_true(file.exists(file.path(cache_dir, "Atlantic_cod__any.classify.rds")))
})

# ---------------------------------------------------------------------------
# F11 - the original name first, the singular form only as a fallback
# ---------------------------------------------------------------------------

test_that("a binomial resolves as itself and is never singularised (F11)", {
  skip_if_not_installed("rfishbase")
  calls <- mock_fishbase_names(sci = "Pollachius virens")
  res <- resolve_fishbase_name("Pollachius virens")
  expect_identical(res$valid_name, "Pollachius virens")
  expect_identical(res$match_confidence, "high")
  expect_identical(calls$validate, "Pollachius virens")
  expect_false(any(grepl("viren$", c(calls$validate, calls$common))))
})

test_that("a genus-like name resolves before any singular form is tried (F11)", {
  skip_if_not_installed("rfishbase")
  calls <- mock_fishbase_names(common = list(ammodytes = common_rows("Ammodytes tobianus", "Ammodytes")))
  res <- resolve_fishbase_name("Ammodytes")
  expect_identical(res$valid_name, "Ammodytes tobianus")
  expect_false("Ammodyte" %in% c(calls$validate, calls$common))
})

test_that("an English plural falls back to its singular form (F11)", {
  skip_if_not_installed("rfishbase")
  calls <- mock_fishbase_names(common = list(
    sandeel = common_rows(c("Ammodytes marinus", "Ammodytes tobianus"), c("Lesser sandeel", "Lesser sandeel"))
  ))
  expect_warning(res <- resolve_fishbase_name("Sandeels"), "ambiguous: 2 candidates")
  expect_identical(res$valid_name, "Ammodytes marinus")
  expect_identical(calls$validate, c("Sandeels", "Sandeel"))
  expect_identical(calls$common, c("Sandeels", "Sandeel"))
})

test_that("a name nothing resolves returns NULL (F11)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names()
  expect_null(resolve_fishbase_name("Nonexistus fictitious"))
})
```

- [ ] **Step 2: Run to verify the new tests fail**

Run the Task 1 Step 2 command. Expected: `tests 25 failing 14`. All 14 new tests fail, as verified on master:
- WoRMS confidence is "low";
- `resolve_fishbase_name` is not found;
- the cache file has no region suffix.

- [ ] **Step 3: Add the name-resolution helpers**

In `R/functions/taxonomic_api_utils.R`, replace

```r
#' Query FishBase API for Fish Species Data
#'
#' @param species_name Character, species name to query
```

with

```r
#' Singular form of an English common name (FishBase names are singular)
#'
#' Only a fallback: resolve_fishbase_name() tries the name as given first, so
#' a binomial ("Pollachius virens", "Ammodytes") is never cut down (F11).
#'
#' @param name Character(1).
#' @return Character(1), `name` itself when no plural pattern applies.
#' @export
singularize_common_name <- function(name) {
  if (grepl("gobies$", name, ignore.case = TRUE)) {
    return(sub("gobies$", "goby", name, ignore.case = TRUE))
  }
  if (grepl("([^aeiouy])ies$", name, ignore.case = TRUE)) {
    # herries -> herry, guppies -> guppy
    return(sub("([^aeiouy])ies$", "\\1y", name, ignore.case = TRUE))
  }
  if (grepl("(ss|sh|ch|x|z)es$", name, ignore.case = TRUE)) {
    # basses -> bass, fishes -> fish
    return(sub("(ss|sh|ch|x|z)es$", "\\1", name, ignore.case = TRUE))
  }
  if (grepl("([aeiou]y)s$", name, ignore.case = TRUE)) {
    # rays -> ray (keep the 'y')
    return(sub("s$", "", name))
  }
  if (grepl("s$", name, ignore.case = TRUE) && !grepl("(us|is|ss)$", name, ignore.case = TRUE)) {
    # eels -> eel, cods -> cod, but NOT (nautilus, analysis, bass)
    return(sub("s$", "", name))
  }
  name
}

#' Does FishBase place a species in a region?
#'
#' rfishbase 5 has no `distribution()` (the old region filter called it and
#' always failed); the FAO areas (`faoareas()$FAO`, e.g. "Atlantic,
#' Northeast") and ecosystems (`ecosystem()$EcosystemName`, e.g. "Baltic
#' Sea", "North Sea") carry the region names.
#'
#' @param species Scientific name.
#' @param region Free-text region ("Baltic", "North Sea"), matched as a
#'   fixed, case-insensitive substring.
#' @return TRUE / FALSE; a failed query warns and counts as no match.
#' @export
fishbase_species_in_region <- function(species, region) {
  column <- function(fn, col) {
    rows <- tryCatch(
      getExportedValue("rfishbase", fn)(species),
      error = function(e) {
        warning(sprintf("[fishbase] %s() failed for '%s': %s", fn, species, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )
    if (NROW(rows) == 0 || !col %in% names(rows)) character() else as.character(rows[[col]])
  }
  places <- tolower(c(column("faoareas", "FAO"), column("ecosystem", "EcosystemName")))
  any(grepl(tolower(region), places, fixed = TRUE))
}

#' Resolve a species or common name to one FishBase species (F10, F11)
#'
#' Lookup order (spec C2.7): the name as given, first as a scientific name
#' (`rfishbase::validate_names`), then as a common name; only if both find
#' nothing, the singular form, in the same order. A scientific-name hit is
#' "high". For a common name (spec C2.6), `common_to_sci()` matches
#' substrings, so rows whose ComName equals the query (case-insensitive) are
#' the candidates when there are any, else all rows; candidates are distinct
#' species:
#' - one species: "high";
#' - several, exactly one of them in `geographic_region`: "medium";
#' - otherwise the first by `Species` sort order: "low", with a warning.
#' At most `max_region_checks` candidates are checked against the region
#' (a `faoareas()` and an `ecosystem()` query each); a broad name ("cod": 286 species) is
#' "low" without querying them all.
#'
#' @param species_name Character(1).
#' @param geographic_region Character(1) or NULL.
#' @param update_progress Function(msg) for progress lines.
#' @param max_region_checks Integer, cap on candidates checked against the region.
#' @return NULL, or `list(valid_name, match_confidence)`.
#' @export
resolve_fishbase_name <- function(species_name, geographic_region = NULL,
                                  update_progress = function(msg) invisible(NULL),
                                  max_region_checks = 25L) {
  region <- .scalar_chr(geographic_region)

  from_common_name <- function(name) {
    rows <- tryCatch(
      rfishbase::common_to_sci(name, Language = "English"),
      error = function(e) {
        warning(sprintf("[fishbase] common_to_sci failed for '%s': %s", name, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )
    if (NROW(rows) == 0) return(NULL)
    exact <- rows[tolower(trimws(rows$ComName)) == tolower(trimws(name)), , drop = FALSE]
    candidates <- if (nrow(exact) > 0) exact else rows
    species <- sort(unique(as.character(candidates$Species)))
    species <- species[!is.na(species) & nzchar(species)]
    if (length(species) == 0) return(NULL)
    if (length(species) == 1) {
      update_progress(sprintf("      → FishBase: Common name '%s' → '%s'", name, species))
      return(list(valid_name = species, match_confidence = "high"))
    }
    update_progress(sprintf("      → FishBase: '%s' matches %d species", name, length(species)))
    if (!is.na(region) && length(species) <= max_region_checks) {
      in_region <- character()
      for (sp in species) {
        if (fishbase_species_in_region(sp, region)) in_region <- c(in_region, sp)
        if (length(in_region) > 1) break
      }
      if (length(in_region) == 1) {
        update_progress(sprintf("      ✓ FishBase: '%s' is the only candidate in %s", in_region, region))
        return(list(valid_name = in_region, match_confidence = "medium"))
      }
    }
    warning(sprintf("[fishbase] '%s' ambiguous: %d candidates", name, length(species)), call. = FALSE)
    list(valid_name = species[1], match_confidence = "low")
  }

  for (name in unique(c(species_name, singularize_common_name(species_name)))) {
    if (!identical(name, species_name)) {
      update_progress(sprintf("      → FishBase: Singularized '%s' → '%s'", species_name, name))
    }
    sci <- tryCatch(
      suppressWarnings(rfishbase::validate_names(name)),
      error = function(e) {
        warning(sprintf("[fishbase] validate_names failed for '%s': %s", name, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )
    sci <- as.character(sci)[!is.na(sci)]
    if (length(sci) > 0) {
      update_progress(sprintf("      → FishBase: Validated as scientific name '%s'", sci[1]))
      return(list(valid_name = sci[1], match_confidence = "high"))
    }
    hit <- from_common_name(name)
    if (!is.null(hit)) return(hit)
  }
  NULL
}

#' Query FishBase API for Fish Species Data
#'
#' @param species_name Character, species name to query
```

- [ ] **Step 4: `query_fishbase()` uses the resolver**

The block to replace is 132 lines: the singularisation, "STRATEGY 1: Try as COMMON NAME first" and "STRATEGY 2: ... SCIENTIFIC NAME". It runs from the line `    # Singularize common plural forms (FishBase uses singular common names)` through the line `    valid_name <- validated`. Replace it with a throwaway script, not by hand.

1. Save the script with the Write tool as `<scratchpad>/c6a_replace_resolution.R`, outside the repo:

```r
# One-off (C-6a Task 2): replace query_fishbase()'s name-resolution block with
# the call to resolve_fishbase_name(). Run from the repo root. Both anchors
# must occur exactly once, 131 lines apart.
f <- "R/functions/taxonomic_api_utils.R"
x <- readLines(f, warn = FALSE, encoding = "UTF-8")
s <- which(x == "    # Singularize common plural forms (FishBase uses singular common names)")
e <- which(x == "    valid_name <- validated")
stopifnot(length(s) == 1, length(e) == 1, e - s == 131)
new <- c(
  "    # Name resolution (spec C2.6, C2.7): the name as given first, as a",
  "    # scientific then a common name; the singular form only as a fallback;",
  "    # graded confidence for common-name matches.",
  "    resolved <- resolve_fishbase_name(species_name, geographic_region, update_progress)",
  "    if (is.null(resolved)) {",
  "      update_progress(sprintf(\"      ✗ FishBase: '%s' not found\", species_name))",
  "      return(NULL)",
  "    }",
  "    valid_name <- resolved$valid_name"
)
x <- c(x[seq_len(s - 1)], new, x[(e + 1):length(x)])
con <- file(f, "wb")
writeLines(enc2utf8(x), con, sep = "\n", useBytes = TRUE)
close(con)
cat("replaced", e - s + 1, "lines with", length(new), "\n")
```

2. Run it and check the result:

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/c6a_replace_resolution.R"
git diff --stat R/functions/taxonomic_api_utils.R
grep -n "STRATEGY 1: Try as COMMON NAME\|rfishbase::distribution\|resolved\$valid_name" R/functions/taxonomic_api_utils.R
```

Expected:
- `replaced 132 lines with 9`;
- the diffstat shows only this file;
- grep prints only the `resolved$valid_name` line: STRATEGY 1 and `distribution` are gone.

If the `stopifnot` fires, **STOP**: the block has changed since this plan was written.

3. Then, in `query_fishbase()`, replace

```r
      max_depth_m = max_depth,
      functional_group = "Fish"
    )
```

with

```r
      max_depth_m = max_depth,
      functional_group = "Fish",
      match_confidence = resolved$match_confidence  # how sure the name match is (F10)
    )
```

and replace

```r
  }, error = function(e) {
    update_progress(sprintf("      ✗ FishBase query error: %s", conditionMessage(e)))
    return(NULL)
  })
}
```

with

```r
  }, error = function(e) {
    warning(sprintf("[fishbase] query failed for '%s': %s", species_name, conditionMessage(e)),
            call. = FALSE)
    update_progress(sprintf("      ✗ FishBase query error: %s", conditionMessage(e)))
    return(NULL)
  })
}
```

- [ ] **Step 5: `classify_species_api()`: region-keyed cache, FishBase and WoRMS confidence**

In `classify_species_api()`, replace

```r
  # `list(traits=...)` shape - sharing the filename corrupted both (#4).
  cache_file <- file.path(cache_dir, paste0(gsub("[^a-zA-Z0-9]", "_", clean_name), ".classify.rds"))
```

with

```r
  # `list(traits=...)` shape - sharing the filename corrupted both (#4).
  # The region is part of the key: it can pick a different FishBase species
  # for the same common name (F10).
  region <- .scalar_chr(geographic_region)
  region_key <- if (is.na(region)) "any" else gsub("[^a-zA-Z0-9]", "_", region)
  cache_file <- file.path(cache_dir, paste0(gsub("[^a-zA-Z0-9]", "_", clean_name), "__", region_key,
                                            ".classify.rds"))
```

replace

```r
      result$source <- "FishBase"
      result$confidence <- "high"
      result$taxonomy <- fishbase_result
```

with

```r
      result$source <- "FishBase"
      # "high" only for a scientific name or a unique common name (F10)
      result$confidence <- fishbase_result$match_confidence %||% "high"
      result$taxonomy <- fishbase_result
```

replace

```r
      message(sprintf("    └─ ✓ SUCCESS: FishBase → %s (habitat: %s, confidence: high)",
                      result$functional_group,
                      ifelse(is.na(result$habitat), "NA", result$habitat)))
```

with

```r
      message(sprintf("    └─ ✓ SUCCESS: FishBase → %s (habitat: %s, confidence: %s)",
                      result$functional_group,
                      ifelse(is.na(result$habitat), "NA", result$habitat),
                      result$confidence))
```

replace

```r
    # Classify based on WoRMS taxonomic class
    worms_classification <- classify_by_taxonomy(worms_result)
```

with

```r
    # Classify based on WoRMS taxonomic class. A WoRMS-derived group is
    # "medium" (F9; "high" is kept for curated sources). The overrides below
    # and classify_by_taxonomy()'s no-match "Fish" default are pattern
    # guesses, not WoRMS evidence, so they stay "low".
    worms_classification <- classify_by_taxonomy(worms_result)
    result$confidence <- if (isTRUE(attr(worms_classification, "default"))) "low" else confidence_to_label(0.66)
    worms_classification <- as.vector(worms_classification)
```

and replace

```r
    result$source <- "WoRMS"
    if (is.null(result$confidence)) result$confidence <- "medium"
    result$taxonomy <- worms_result
```

with

```r
    result$source <- "WoRMS"
    result$taxonomy <- worms_result
```

- [ ] **Step 6: Mark `classify_by_taxonomy()`'s default**

Replace

```r
#' @param taxonomy List with taxonomic information
#' @return Character, functional group
#'
```

with

```r
#' @param taxonomy List with taxonomic information
#' @return Character, functional group. The "Fish" returned when no class
#'   matched (or there is no class) carries `attr(, "default") = TRUE`, so
#'   callers can tell a guess from a taxonomic match.
#'
```

replace

```r
    message(sprintf("      → classify_by_taxonomy: Defaulting to Fish (no class)"))
    return("Fish")  # Default
  }
```

with

```r
    message(sprintf("      → classify_by_taxonomy: Defaulting to Fish (no class)"))
    return(structure("Fish", default = TRUE))  # a guess, not a taxonomic match (F9)
  }
```

and replace

```r
                  taxonomy$class,
                  ifelse(is.null(taxonomy$phylum), "NA", taxonomy$phylum)))
  return("Fish")
}
```

with

```r
                  taxonomy$class,
                  ifelse(is.null(taxonomy$phylum), "NA", taxonomy$phylum)))
  return(structure("Fish", default = TRUE))  # a guess, not a taxonomic match (F9)
}
```

- [ ] **Step 7: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/taxonomic_api_utils.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('test-trait-lookup-correctness.R', 'test-trait-lookup-unit.R')) { r <- as.data.frame(testthat::test_file(file.path('tests/testthat', f), reporter = 'silent', stop_on_failure = FALSE)); cat(f, 'tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n') }"
grep -c "classify_by_taxonomy(" R/functions/taxonomic_api_utils.R
```

Expected:
- `OK`;
- `test-trait-lookup-correctness.R tests 25 failing 0 skipped 0 passed 86`;
- `test-trait-lookup-unit.R tests 26 failing 0`;
- `2`: the new comment line ("`classify_by_taxonomy()`'s no-match ...") and the call in `classify_species_api()`. The definition line (`classify_by_taxonomy <- function(`) does not match the pattern.

- [ ] **Step 8: Commit**

```bash
git add R/functions/taxonomic_api_utils.R tests/testthat/test-trait-lookup-correctness.R
git commit -m "$(cat <<'EOF'
fix(traits): C-6a name resolution - binomials first, graded common names, WoRMS medium

resolve_fishbase_name() tries the name as given (scientific, then common)
before any singular form, so "Pollachius virens" is no longer looked up as
"Pollachius viren" (F11). Exact common names win; one species is high, one
of several in the region medium, otherwise the first by name low with a
warning; the region check uses faoareas()/ecosystem() because rfishbase 5
has no distribution(); the classify cache is keyed by region (F10). A
WoRMS classification is medium, a name override or the unmatched-class
Fish default stays low (F9).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: One failing species does not abort a batch (F33, server side)

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R`: add `lookup_species_traits_safely()`, and make `batch_lookup_traits()` use it.
- Modify: `R/modules/trait_research_server.R`: the lookup loop, the completion summary and the trait table.
- Test: `tests/testthat/test-trait-lookup-correctness.R` (append)

**Interfaces:**
- Consumes: `lookup_species_traits()`, `trait_research_server(input, output, session, shared_data)`, and the testServer pattern of `test-rebuild-observer.R`.
- Produces: `lookup_species_traits_safely(species_name, ...)`. `rv$trait_results` may carry an `error` column, and `rv$raw_results[[sp]]$error`.

- [ ] **Step 1: Append the failing tests**

Append to `tests/testthat/test-trait-lookup-correctness.R`:

```r

# ---------------------------------------------------------------------------
# F33 - one failing species does not abort a Trait Research batch
# ---------------------------------------------------------------------------

test_that("lookup_species_traits_safely turns an error into a warning and an error row (F33)", {
  boom <- function(species_name, ...) stop("argument is of length zero")
  expect_warning(
    row <- with_mocked_function(globalenv(), "lookup_species_traits", boom,
                                lookup_species_traits_safely("Enoplus brevis")),
    "\\[trait_research\\] lookup failed for 'Enoplus brevis': argument is of length zero"
  )
  expect_identical(row$species, "Enoplus brevis")
  expect_identical(row$error, "argument is of length zero")
  for (trait in c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")) {
    expect_true(is.na(row[[trait]]), info = trait)
  }
})

# The Trait Research server as a testServer() app (the pattern of
# test-rebuild-observer.R), run in a temp working directory because the
# lookup observer creates ./cache/taxonomy.
local_trait_research_module <- function(env = parent.frame()) {
  skip_if_not_installed("plotly")
  skip_if_not_installed("DT")
  skip_if_not_installed("bs4Dash")
  withr::local_package("bs4Dash", .local_envir = env)
  for (f in c("R/functions/admin_auth.R", "R/functions/offline_db_rebuild.R",
              "R/modules/trait_research_server.R")) {
    source(file.path(get_app_root(), f), local = FALSE)
  }
  withr::local_dir(withr::local_tempdir(.local_envir = env), .local_envir = env)
  mod <- trait_research_server
  formals(mod)$shared_data <- NULL
  mod
}

fake_lookup <- function(species_name, ...) {
  if (grepl("^Bad", species_name)) stop("argument is of length zero")
  data.frame(species = species_name, MS = "MS4", FS = "FS1", MB = "MB5", EP = "EP2", PR = "PR0",
             RS = NA_character_, TT = NA_character_, ST = NA_character_, source = "FishBase",
             stringsAsFactors = FALSE)
}

test_that("a Trait Research batch survives one species whose lookup throws (F33, testServer)", {
  mod <- local_trait_research_module()
  with_mocked_function(globalenv(), "lookup_species_traits", fake_lookup, {
    shiny::testServer(mod, {
      session$setInputs(trait_research_input_method = "manual",
                        trait_research_species_list = "Good one\nBad one\nGood two",
                        trait_research_databases = "worms")
      expect_warning(session$setInputs(trait_research_run_lookup = 1),
                     "\\[trait_research\\] lookup failed for 'Bad one'")
      res <- rv$trait_results
      expect_identical(res$species, c("Good one", "Bad one", "Good two"))
      expect_identical(res$MS, c("MS4", NA, "MS4"))
      expect_identical(res$error, c(NA, "argument is of length zero", NA))
      expect_identical(rv$raw_results[["Bad one"]]$error, "argument is of length zero")
      expect_false(rv$lookup_in_progress)
    })
  })
})

test_that("a batch whose only species fails still renders its table and summary (F33, testServer)", {
  mod <- local_trait_research_module()
  with_mocked_function(globalenv(), "lookup_species_traits", fake_lookup, {
    shiny::testServer(mod, {
      session$setInputs(trait_research_input_method = "manual",
                        trait_research_species_list = "Bad only",
                        trait_research_databases = "worms")
      expect_warning(session$setInputs(trait_research_run_lookup = 1), "lookup failed for 'Bad only'")
      expect_identical(nrow(rv$trait_results), 1L)
      expect_no_error(output$trait_research_table)
      expect_no_error(output$trait_research_summary)
    })
  })
})
```

- [ ] **Step 2: Run to verify the new tests fail**

Run the Task 1 Step 2 command. Expected: `tests 28 failing 3`, as verified on master. The failures are:
- `lookup_species_traits_safely` not found;
- the batch aborting on "Bad one", so no warning is emitted and `rv$trait_results` stays NULL;
- the single-failure batch rendering nothing.

The console prints the module's lookup banners; that is expected.

- [ ] **Step 3: The safe wrapper**

In `R/functions/trait_lookup/orchestrator.R`, replace

```r
#' Batch lookup traits for multiple species
#'
#' @param species_list Character vector of species names
#' @param ... Additional arguments passed to lookup_species_traits
#' @return Data frame with all species traits
#' @export
batch_lookup_traits <- function(species_list, ...) {

  results_list <- list()

  for (i in seq_along(species_list)) {
    message("\n[", i, "/", length(species_list), "] Processing ", species_list[i])

    result <- lookup_species_traits(species_list[i], ...)
    results_list[[i]] <- result
```

with

```r
#' Look up one species without letting its failure abort a batch (F33)
#'
#' Runs lookup_species_traits(). An error (one taxon with an unexpected
#' database response used to abort a whole Trait Research run) becomes a
#' warning and a row with every trait code NA and the message in `error`.
#'
#' @param species_name Scientific name.
#' @param ... Passed to lookup_species_traits().
#' @return The lookup's data frame, or the one-row error frame.
#' @export
lookup_species_traits_safely <- function(species_name, ...) {
  tryCatch(
    lookup_species_traits(species_name, ...),
    error = function(e) {
      msg <- conditionMessage(e)
      warning(sprintf("[trait_research] lookup failed for '%s': %s", species_name, msg), call. = FALSE)
      data.frame(
        species = species_name,
        MS = NA_character_, FS = NA_character_, MB = NA_character_,
        EP = NA_character_, PR = NA_character_,
        RS = NA_character_, TT = NA_character_, ST = NA_character_,
        source = NA_character_, confidence = "none",
        error = msg,
        stringsAsFactors = FALSE
      )
    }
  )
}

#' Batch lookup traits for multiple species
#'
#' @param species_list Character vector of species names
#' @param ... Additional arguments passed to lookup_species_traits
#' @return Data frame with all species traits (a failed species is a row with
#'   NA codes and its `error`)
#' @export
batch_lookup_traits <- function(species_list, ...) {

  results_list <- list()

  for (i in seq_along(species_list)) {
    message("\n[", i, "/", length(species_list), "] Processing ", species_list[i])

    result <- lookup_species_traits_safely(species_list[i], ...)
    results_list[[i]] <- result
```

- [ ] **Step 4: The Trait Research loop, summary and table**

In `R/modules/trait_research_server.R`, replace

```r
        # called lookup_species_traits again — doubling API calls.
        full_result <- lookup_species_traits(
          species,
          biotic_file = biotic_file,
          maredat_file = maredat_file,
          ptdb_file = ptdb_file,
          cache_dir = cache_dir
        )

        results_list[[i]] <- full_result
        # Build raw_data summary from the orchestrator result
        raw_data <- list(species = species, source = full_result$source)
        raw_list[[species]] <- raw_data
```

with

```r
        # called lookup_species_traits again — doubling API calls.
        # A species whose lookup throws becomes a warning and an error row;
        # the rest of the batch continues (F33). The tryCatch around this
        # loop is only for setup failures.
        full_result <- lookup_species_traits_safely(
          species,
          biotic_file = biotic_file,
          maredat_file = maredat_file,
          ptdb_file = ptdb_file,
          cache_dir = cache_dir
        )

        results_list[[i]] <- full_result
        # Build raw_data summary from the orchestrator result
        raw_data <- list(species = species, source = full_result$source)
        if (!is.null(full_result$error)) raw_data$error <- full_result$error
        raw_list[[species]] <- raw_data
```

replace

```r
      n_missing <- sum(is.na(results_df$MS) & is.na(results_df$FS) &
                       is.na(results_df$MB) & is.na(results_df$EP) & is.na(results_df$PR))

      # Console summary
      cat("\n========================================\n")
      cat("LOOKUP COMPLETE\n")
      cat(sprintf("Complete: %d | Partial: %d | No data: %d\n", n_complete, n_partial, n_missing))
      cat("========================================\n\n")

      showNotification(
        HTML(sprintf("<b>Trait lookup complete!</b><br>Complete: %d | Partial: %d | No data: %d",
                     n_complete, n_partial, n_missing)),
        type = "message",
        duration = 8
      )
```

with

```r
      n_missing <- sum(is.na(results_df$MS) & is.na(results_df$FS) &
                       is.na(results_df$MB) & is.na(results_df$EP) & is.na(results_df$PR))
      n_failed <- if ("error" %in% names(results_df)) sum(!is.na(results_df$error)) else 0L

      # Console summary
      cat("\n========================================\n")
      cat("LOOKUP COMPLETE\n")
      cat(sprintf("Complete: %d | Partial: %d | No data: %d | Failed: %d\n",
                  n_complete, n_partial, n_missing, n_failed))
      cat("========================================\n\n")

      showNotification(
        HTML(sprintf("<b>Trait lookup complete!</b><br>Complete: %d | Partial: %d | No data: %d | Failed: %d",
                     n_complete, n_partial, n_missing, n_failed)),
        type = if (n_failed > 0) "warning" else "message",
        duration = 8
      )
```

and replace

```r
    if ("source" %in% names(df)) display_df$sources <- df$source

```

with

```r
    if ("source" %in% names(df)) display_df$sources <- df$source
    # A species whose lookup failed (F33) shows why; escaped like `sources`.
    if ("error" %in% names(df)) display_df$error <- df$error

```

`badge_escape_columns(display_df, trait_badge_cols)` keeps every non-badge column escaped, so `error`, which contains exception text, is escaped (`test-trait-table-escaping.R`).

- [ ] **Step 5: Parse-check and run the tests**

```bash
for f in R/functions/trait_lookup/orchestrator.R R/modules/trait_research_server.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"; done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('test-trait-lookup-correctness.R', 'test-rebuild-observer.R', 'test-trait-table-escaping.R', 'test-layer4-ui.R', 'test-trait-cache-config-hash.R', 'test-ci-workflows.R')) { r <- as.data.frame(testthat::test_file(file.path('tests/testthat', f), reporter = 'silent', stop_on_failure = FALSE)); cat(f, 'tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n') }"
```

Expected:
- two `OK` lines;
- `test-trait-lookup-correctness.R tests 28 failing 0 skipped 0 passed 107`;
- `failing 0` for the other five files.
  - The first four were verified on the scratch copy: `test-rebuild-observer.R` 10 tests, `test-trait-table-escaping.R` 9, `test-layer4-ui.R` 6, `test-trait-cache-config-hash.R` 9.
  - `test-ci-workflows.R` was not run in planning. The module gains no `pkg::` call, so its package-list check is unaffected.

- [ ] **Step 6: Commit**

```bash
git add R/functions/trait_lookup/orchestrator.R R/modules/trait_research_server.R tests/testthat/test-trait-lookup-correctness.R
git commit -m "$(cat <<'EOF'
fix(traits): C-6a one failing species no longer aborts a Trait Research batch

lookup_species_traits_safely() turns a lookup error into a warning and a
row with NA codes and the message in `error` (F33); the Trait Research loop
and batch_lookup_traits() use it. The table shows the error, the completion
notice counts failed species.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Fixtures without the bugs; fixture and live asserts

**Files:**
- Modify: `tests/testthat/capture_fixtures.R`: add a `RUN_LIVE_TESTS` guard, section arguments and Phoca.
- Modify: `tests/testthat/test-trait-lookup-unit.R`: two fixture asserts.
- Modify: `tests/testthat/test-trait-lookup-live.R`: four live asserts.
- Re-capture (**NETWORK**): `tests/testthat/fixtures/worms_{gadus_morhua,clupea_harengus,mytilus_edulis,carcinus_maenas,phoca_vitulina}.rds` and `tests/testthat/fixtures/fishbase_{gadus_morhua,clupea_harengus}.rds`.

**Interfaces:**
- Consumes: Tasks 1-2 (`size_unit_source`, grams, `resolve_fishbase_name()`), `with_timeout()`, `skip_if_no_live_tests()`, `skip_if_offline()`.
- Produces: fixtures holding `max_length_cm` from unit records and weights in grams.

- [ ] **Step 1: `capture_fixtures.R`: the live guard, sections and Phoca**

In `tests/testthat/capture_fixtures.R`, replace

```r
# Usage:
#   Rscript tests/testthat/capture_fixtures.R
#
# Run from the EcoNeTool project root directory.
# =============================================================================

cat("=== Fixture Capture Script ===\n\n")

# Ensure we're in the right directory
if (!file.exists("R/functions/ecobase_connection.R")) {
  stop("Please run from the EcoNeTool root directory")
}
```

with

```r
# Usage (it calls live APIs, so only with RUN_LIVE_TESTS=true):
#   RUN_LIVE_TESTS=true Rscript tests/testthat/capture_fixtures.R
#   RUN_LIVE_TESTS=true Rscript tests/testthat/capture_fixtures.R worms fishbase
# With arguments, only the named sections (ecobase, worms, fishbase,
# sealifebase, traits) are captured; the other fixtures stay untouched.
#
# Run from the EcoNeTool project root directory.
# =============================================================================

cat("=== Fixture Capture Script ===\n\n")

# Ensure we're in the right directory
if (!file.exists("R/functions/ecobase_connection.R")) {
  stop("Please run from the EcoNeTool root directory")
}

if (!identical(Sys.getenv("RUN_LIVE_TESTS"), "true")) {
  stop("capture_fixtures.R calls live APIs; run it with RUN_LIVE_TESTS=true")
}

all_sections <- c("ecobase", "worms", "fishbase", "sealifebase", "traits")
sections <- tolower(commandArgs(trailingOnly = TRUE))
if (length(sections) == 0) sections <- all_sections
unknown <- setdiff(sections, all_sections)
if (length(unknown) > 0) {
  stop("Unknown section(s): ", paste(unknown, collapse = ", "),
       ". Choose from: ", paste(all_sections, collapse = ", "))
}
want <- function(section) section %in% sections
cat("Sections:", paste(sections, collapse = ", "), "\n")
```

replace

```r
cat("\n[1/5] Capturing EcoBase fixtures...\n")

tryCatch({
```

with

```r
cat("\n[1/5] Capturing EcoBase fixtures...\n")

if (want("ecobase")) tryCatch({
```

replace

```r
test_species_worms <- c(
  "Gadus morhua",      # Atlantic cod (fish)
  "Clupea harengus",   # Herring (fish)
  "Mytilus edulis",     # Blue mussel (invertebrate)
  "Carcinus maenas"    # Shore crab (invertebrate)
)
```

with

```r
test_species_worms <- if (want("worms")) c(
  "Gadus morhua",      # Atlantic cod (fish)
  "Clupea harengus",   # Herring (fish)
  "Mytilus edulis",     # Blue mussel (invertebrate)
  "Carcinus maenas",   # Shore crab (invertebrate)
  "Phoca vitulina"     # Harbour seal (cm lengths next to kg weights)
) else character()
```

replace

```r
test_species_fishbase <- c("Gadus morhua", "Clupea harengus")
```

with

```r
test_species_fishbase <- if (want("fishbase")) c("Gadus morhua", "Clupea harengus") else character()
```

replace

```r
test_species_slb <- c("Mytilus edulis", "Carcinus maenas")
```

with

```r
test_species_slb <- if (want("sealifebase")) c("Mytilus edulis", "Carcinus maenas") else character()
```

and replace

```r
test_species_traits <- c(
  "Gadus morhua",
  "Mytilus edulis",
  "Clupea harengus"
)
```

with

```r
test_species_traits <- if (want("traits")) c(
  "Gadus morhua",
  "Mytilus edulis",
  "Clupea harengus"
) else character()
```

- [ ] **Step 2: Fixture asserts in `test-trait-lookup-unit.R`**

In `tests/testthat/test-trait-lookup-unit.R`, replace

```r
  expect_lt(result$traits$max_length_cm, 300,
            label = "harbour seal max_length_cm should be < 300 cm")
})

```

with

```r
  expect_lt(result$traits$max_length_cm, 300,
            label = "harbour seal max_length_cm should be < 300 cm")
})

test_that("the WoRMS Mytilus fixture records its body size in cm (F32)", {
  result <- load_fixture("worms_mytilus_edulis")
  skip_if(is.null(result$traits$size_unit_source),
          paste("worms_mytilus_edulis predates C-6a (2 cm mussel); re-capture with",
                "RUN_LIVE_TESTS=true Rscript tests/testthat/capture_fixtures.R worms fishbase"))
  expect_gte(result$traits$max_length_cm, 10)
  expect_identical(result$traits$size_unit_source, "child")
})

```

and replace

```r
test_that("FishBase trait list has expected fields", {
```

with

```r
test_that("the FishBase cod fixture records its weight in grams (F78)", {
  result <- load_fixture("fishbase_gadus_morhua")
  skip_if(!isTRUE(result$success) || is.null(result$traits$max_weight_g),
          "fishbase_gadus_morhua fixture has no max_weight_g; refresh with capture_fixtures.R")
  skip_if(isTRUE(result$traits$max_weight_g == 9.6e7),
          paste("fishbase_gadus_morhua predates C-6a (96 t cod); re-capture with",
                "RUN_LIVE_TESTS=true Rscript tests/testthat/capture_fixtures.R worms fishbase"))
  expect_lt(result$traits$max_weight_g, 1e6)
  expect_gt(result$traits$max_weight_g, 1e4)
})

test_that("FishBase trait list has expected fields", {
```

The skips name the exact command. On the old fixtures these two tests skip (verified: `tests 28 failing 0 skipped 2`). Once re-captured, they must pass.

- [ ] **Step 3: Live asserts in `test-trait-lookup-live.R`**

In `tests/testthat/test-trait-lookup-live.R`, replace

```r
  expect_equal(tolower(result$traits$phylum), "mollusca")
})

```

(the end of "WoRMS returns taxonomy for Mytilus edulis (live)") with

```r
  expect_equal(tolower(result$traits$phylum), "mollusca")
})

test_that("WoRMS body size for Mytilus edulis is in cm from its unit record (live, F32)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")
  skip_if_no_package("worrms")

  result <- with_timeout(lookup_worms_traits("Mytilus edulis"), timeout = 90)
  skip_if(is.null(result), "WoRMS lookup for Mytilus edulis timed out")
  expect_true(result$success)
  expect_gte(result$traits$max_length_cm, 10)
  expect_identical(result$traits$size_unit_source, "child")
})

test_that("WoRMS still reports Copepoda at class rank (live drift check for the taxon rules)", {
  # TRAIT_VOCAB's copepod rules match `class`; WoRMS has placed Copepoda as a
  # subclass of Hexanauplia in the past. If this fails, the rules need a
  # vocabulary change (a subclass rank), not a private regex.
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")
  skip_if_no_package("worrms")

  result <- with_timeout(lookup_worms_traits("Acartia clausi"), timeout = 90)
  skip_if(is.null(result), "WoRMS lookup for Acartia clausi timed out")
  expect_true(result$success)
  expect_identical(result$traits$class, "Copepoda")
})

```

and replace

```r
test_that("FishBase returns data for Clupea harengus (live)", {
```

with

```r
test_that("FishBase cod weight is in grams (live, F78)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  result <- with_timeout(lookup_fishbase_traits("Gadus morhua", timeout = 90), timeout = 120)
  skip_if(is.null(result) || is.null(result$traits$max_weight_g),
          "FishBase returned no weight for Gadus morhua (timeout or no data)")
  expect_lt(result$traits$max_weight_g, 1e6)
  expect_gt(result$traits$max_weight_g, 1e4)
})

test_that("FishBase resolves Pollachius virens as itself, not 'Pollachius viren' (live, F11)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  result <- with_timeout(resolve_fishbase_name("Pollachius virens"), timeout = 120)
  skip_if(is.null(result), "FishBase name resolution timed out")
  expect_identical(result$valid_name, "Pollachius virens")
  expect_identical(result$match_confidence, "high")
})

test_that("FishBase returns data for Clupea harengus (live)", {
```

- [ ] **Step 4: Parse-check and run offline**

```bash
for f in tests/testthat/capture_fixtures.R tests/testthat/test-trait-lookup-unit.R tests/testthat/test-trait-lookup-live.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"; done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/testthat/capture_fixtures.R worms 2>&1 | tail -2
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('test-trait-lookup-unit.R', 'test-trait-lookup-live.R')) { r <- as.data.frame(testthat::test_file(file.path('tests/testthat', f), reporter = 'silent', stop_on_failure = FALSE)); cat(f, 'tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), '\n') }"
```

Expected:
- three `OK` lines;
- the capture script refuses to run: `Error: capture_fixtures.R calls live APIs; run it with RUN_LIVE_TESTS=true`;
- `test-trait-lookup-unit.R tests 28 failing 0 skipped 2` (verified);
- `test-trait-lookup-live.R tests 20 failing 0 skipped 20` (verified).

- [ ] **Step 5: Re-capture the WoRMS and FishBase fixtures (NETWORK)**

First check reachability:

```bash
curl -sS -o /dev/null -w '%{http_code}\n' --max-time 20 https://www.marinespecies.org/rest/AphiaRecordsByName/Mytilus%20edulis
```

- **Offline fallback:** if this does not print `200`, or the capture below fails for a species, skip Steps 5-6. Leave the old fixtures in place: the two Step 2 tests then skip with the re-capture command as their reason. Continue with Step 7, and **STOP - tell the user** that the fixtures were not re-captured. The PR body (Task 7 Step 5) must then say "fixtures not re-captured".

Otherwise:

```bash
RUN_LIVE_TESTS=true "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/testthat/capture_fixtures.R worms fishbase 2>&1 | grep -v "^Joining\|WoRMS: Found via" | tail -25
git status --short tests/testthat/fixtures
```

Expected:
- `Sections: worms, fishbase`;
- `-> success: TRUE` for the five WoRMS and two FishBase species;
- a first FishBase call that may take a few minutes (the parquet download);
- exactly seven modified fixtures: `worms_{gadus_morhua,clupea_harengus,mytilus_edulis,carcinus_maenas,phoca_vitulina}.rds` and `fishbase_{gadus_morhua,clupea_harengus}.rds`.

No `ecobase_*`, `sealifebase_*` or `traits_*` file may change. If one does, `git checkout -- <file>` it.

- [ ] **Step 6: Review the re-captured values (NETWORK results)**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('worms_mytilus_edulis','worms_gadus_morhua','worms_carcinus_maenas','worms_clupea_harengus','worms_phoca_vitulina')) { x <- readRDS(file.path('tests/testthat/fixtures', paste0(f, '.rds'))); cat(f, x\$success, x\$traits\$max_length_cm, x\$traits\$size_unit_source, x\$traits\$class, '\n') }; for (f in c('fishbase_gadus_morhua','fishbase_clupea_harengus')) { x <- readRDS(file.path('tests/testthat/fixtures', paste0(f, '.rds'))); cat(f, x\$success, x\$traits\$max_length_cm, x\$traits\$max_weight_g, '\n') }"
```

Expected (live values of 2026-09-29; a curator edit may move a number, and then the asserted bounds decide):
- `worms_mytilus_edulis TRUE 20 child Bivalvia`
- `worms_gadus_morhua TRUE 200 child Teleostei`
- `worms_carcinus_maenas TRUE 7.3 child Malacostraca`
- `worms_clupea_harengus TRUE 45 child Teleostei`
- `worms_phoca_vitulina TRUE 186 child Mammalia`
- `fishbase_gadus_morhua TRUE 200 96000`
- `fishbase_clupea_harengus TRUE 45 1050`

- [ ] **Step 7: Run the fixture tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_file('tests/testthat/test-trait-lookup-unit.R', reporter = 'silent', stop_on_failure = FALSE)); cat('tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), '\n')"
```

Expected: `tests 28 failing 0 skipped 0` after a re-capture, or `skipped 2` with the offline fallback.

- [ ] **Step 8: Run the new live tests (NETWORK, optional)**

```bash
RUN_LIVE_TESTS=true "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trait-lookup-live.R')"
```

Expected: the four new tests pass, and existing tests pass or skip as they do nightly. This step was not run in planning. If a new live test fails, **STOP - tell the user**. A Copepoda failure means WoRMS moved the rank: that is a vocabulary change for the user to decide, not a code fix here.

- [ ] **Step 9: Commit**

```bash
git add tests/testthat/capture_fixtures.R tests/testthat/test-trait-lookup-unit.R tests/testthat/test-trait-lookup-live.R
git add tests/testthat/fixtures/worms_gadus_morhua.rds tests/testthat/fixtures/worms_clupea_harengus.rds tests/testthat/fixtures/worms_mytilus_edulis.rds tests/testthat/fixtures/worms_carcinus_maenas.rds tests/testthat/fixtures/worms_phoca_vitulina.rds tests/testthat/fixtures/fishbase_gadus_morhua.rds tests/testthat/fixtures/fishbase_clupea_harengus.rds
git commit -m "$(cat <<'EOF'
test(traits): C-6a re-captured WoRMS/FishBase fixtures, fixture and live asserts

capture_fixtures.R needs RUN_LIVE_TESTS=true and takes section names, so
only the WoRMS and FishBase fixtures were re-captured (Mytilus 20 cm, cod
96000 g). The unit suite asserts both (actionable skip on a pre-C-6a
fixture); the live suite asserts Mytilus >= 10 cm, cod < 1e6 g, Pollachius
virens resolving as itself, and Copepoda still at class rank.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

With the offline fallback, leave the fixture paths out of `git add`. Say "fixtures not re-captured (offline)" in the message body instead of the second sentence.

---

### Task 5: Pre-C-6a cache envelopes are misses (User decision 1)

The user accepted this task on 2026-09-29 (Execution ruling 4).

**Files:**
- Modify: `R/functions/trait_lookup/harmonization.R`: add `TRAIT_LOOKUP_REVISION` and hash it in `harm_config_hash()`.
- Test: `tests/testthat/test-trait-lookup-correctness.R` (append)

**Interfaces:**
- Consumes: `harm_config_hash(cfg = get_harm_config())` (B1 / C-5).
- Produces: `TRAIT_LOOKUP_REVISION` (`2L`). Every `config_hash` stamped before this task differs from the new one.

- [ ] **Step 1: Append the failing test**

Append to `tests/testthat/test-trait-lookup-correctness.R`:

```r

# ---------------------------------------------------------------------------
# Cached envelopes written before these fixes are refreshed
# ---------------------------------------------------------------------------

test_that("harm_config_hash changes with the lookup revision, so pre-C-6a envelopes are misses", {
  expect_identical(TRAIT_LOOKUP_REVISION, 2L)
  old <- TRAIT_LOOKUP_REVISION
  withr::defer(assign("TRAIT_LOOKUP_REVISION", old, envir = globalenv()))
  before <- harm_config_hash(HARMONIZATION_CONFIG)
  assign("TRAIT_LOOKUP_REVISION", old - 1L, envir = globalenv())
  expect_false(identical(harm_config_hash(HARMONIZATION_CONFIG), before))
})
```

- [ ] **Step 2: Run to verify it fails**

Run the Task 1 Step 2 command. Expected: `tests 29 failing 1` (`TRAIT_LOOKUP_REVISION` not found; verified).

- [ ] **Step 3: Hash the lookup revision**

In `R/functions/trait_lookup/harmonization.R`, replace

```r
#' Hash of the effective harmonization config (trait-cache key, F72)
#'
```

with

```r
#' Revision of the raw values the trait lookups return
#'
#' harm_config_hash() hashes it in. Bump it when a lookup fix changes raw
#' values that cached envelopes already hold (sizes, weights, taxonomy), so
#' every cache/taxonomy envelope written before the fix is a miss and is
#' refreshed on first read instead of serving the old value for 30 days.
#' 2L: C-6a (WoRMS body-size units, FishBase grams, NA ranks).
TRAIT_LOOKUP_REVISION <- 2L


#' Hash of the effective harmonization config (trait-cache key, F72)
#'
```

and replace

```r
#' phylogenetic imputation, instead of serving old MB codes for 30 days.
#'
#' @param cfg Config list; defaults to this session's config.
#' @return Character(1) xxhash64 digest, or NULL when no config is loaded.
harm_config_hash <- function(cfg = get_harm_config()) {
  if (is.null(cfg)) return(NULL)
  cfg$last_modified <- NULL
  cfg$version <- NULL
  cfg$.trait_vocab <- get_trait_vocab()
```

with

```r
#' phylogenetic imputation, instead of serving old MB codes for 30 days.
#' TRAIT_LOOKUP_REVISION does the same for fixes to the raw lookup values.
#'
#' @param cfg Config list; defaults to this session's config.
#' @return Character(1) xxhash64 digest, or NULL when no config is loaded.
harm_config_hash <- function(cfg = get_harm_config()) {
  if (is.null(cfg)) return(NULL)
  cfg$last_modified <- NULL
  cfg$version <- NULL
  cfg$.trait_vocab <- get_trait_vocab()
  cfg$.lookup_revision <- TRAIT_LOOKUP_REVISION
```

- [ ] **Step 4: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/trait_lookup/harmonization.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('test-trait-lookup-correctness.R', 'test-trait-cache-config-hash.R', 'test-trait-vocabulary.R')) { r <- as.data.frame(testthat::test_file(file.path('tests/testthat', f), reporter = 'silent', stop_on_failure = FALSE)); cat(f, 'tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n') }"
```

Expected:
- `OK`;
- `test-trait-lookup-correctness.R tests 29 failing 0 skipped 0 passed 109`;
- `test-trait-cache-config-hash.R tests 9 failing 0`;
- `test-trait-vocabulary.R tests 58 failing 0`.

The existing hash tests compare hashes with each other, never with a literal, so they are unaffected (verified).

- [ ] **Step 5: Commit**

```bash
git add R/functions/trait_lookup/harmonization.R tests/testthat/test-trait-lookup-correctness.R
git commit -m "$(cat <<'EOF'
fix(traits): C-6a refresh cache envelopes written before the lookup fixes

harm_config_hash() hashes TRAIT_LOOKUP_REVISION (2L), so every
cache/taxonomy envelope written with the old WoRMS sizes is a miss and is
refreshed on first read instead of serving a 2 cm mussel for 30 days.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: Infaunal bivalves before the depth rule (User decision 3, option (b))

This is a vocabulary data change: one rule moves between two `TRAIT_VOCAB` groups. No code changes meaning, so `trait_vocab_version` stays 2L (Execution ruling 4b), and the offline-DB writer is unaffected (Execution ruling 3).

**Files:**
- Modify: `R/config/harmonization_config.R`: move the `infaunal_bivalves` rule from `taxon_rules$environmental` to the end of `taxon_rules$environmental_pelagic`, and update the comment.
- Modify: `R/functions/trait_lookup/harmonization.R`: the step 2 and step 4 comments in `harmonize_environmental_position()`.
- Test: `tests/testthat/test-trait-lookup-correctness.R` (append)

**Interfaces:**
- Consumes: `TRAIT_VOCAB$taxon_rules`, `apply_taxon_rules(taxonomy, trait, text)` and `harmonize_environmental_position(depth_min, depth_max, habitat_info, taxonomic_info)` (C-5); the `infaunal_bivalves` flag in `HARMONIZATION_CONFIG$taxonomic_rules`.
- Produces: `TRAIT_VOCAB$taxon_rules$environmental_pelagic` ending with the bivalve rule; `$environmental` with the fish rules only.

- [ ] **Step 1: Append the tests**

Append to `tests/testthat/test-trait-lookup-correctness.R`:

```r

# ---------------------------------------------------------------------------
# User decision (2026-09-29): infaunal bivalves before the depth rule
# ---------------------------------------------------------------------------

test_that("an infaunal bivalve with only a shallow depth range is endobenthic (EP4)", {
  mya <- list(phylum = "Mollusca", class = "Bivalvia", order = "Myida", family = "Myidae", genus = "Mya")
  # OBIS depth only (no habitat text): the depth rule used to return EP3 first.
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 10, taxonomic_info = mya), "EP4")
  # Explicit text still wins, and the switch still turns the rule off.
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 10, habitat_info = "epifaunal",
                                                    taxonomic_info = mya), "EP3")
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$infaunal_bivalves <- FALSE
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  session$userData$harm_config <- cfg
  shiny::withReactiveDomain(session, {
    expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 10, taxonomic_info = mya), "EP3")
  })
})

test_that("shallow and deep fish keep the depth rule before their order rules", {
  cod <- list(phylum = "Chordata", class = "Teleostei", order = "Gadiformes")
  herring <- list(phylum = "Chordata", class = "Teleostei", order = "Clupeiformes")
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 20, taxonomic_info = cod), "EP3")
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 20, taxonomic_info = herring), "EP3")
  expect_identical(harmonize_environmental_position(depth_min = 150, depth_max = 600, taxonomic_info = herring), "EP2")
  expect_identical(harmonize_environmental_position(taxonomic_info = herring), "EP1")
})
```

- [ ] **Step 2: Run to verify the bivalve test fails**

Run the Task 1 Step 2 command. Expected: `tests 31 failing 1` (verified). The Mya test gets `EP3` for the depth-only case; its other expectations already hold. The fish test passes: it is a guard against moving the fish rules too.

- [ ] **Step 3: Move the rule**

In `R/config/harmonization_config.R`, replace

```r
    # environmental_pelagic runs BEFORE the depth rule, environmental after it.
```

with

```r
    # environmental_pelagic runs BEFORE the depth rule, environmental after it.
    # Despite its name, environmental_pelagic also holds the infaunal-bivalve
    # rule (user decision 2026-09-29, C-6a): a shallow depth range says nothing
    # about living in the sediment, so it must not turn Mya or Macoma into EP3.
```

and replace

```r
        list(match = list(phylum = "^Cnidaria$", class = "^Hydrozoa$"), text = "medusa|pelagic", code = "EP1",
             flag = "zooplankton_pelagic")
      ),
      environmental = list(
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "EP4", flag = "infaunal_bivalves"),
        # Fish by order
```

with

```r
        list(match = list(phylum = "^Cnidaria$", class = "^Hydrozoa$"), text = "medusa|pelagic", code = "EP1",
             flag = "zooplankton_pelagic"),
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "EP4", flag = "infaunal_bivalves")
      ),
      environmental = list(
        # Fish by order
```

The two pre-depth groups' rules match disjoint taxa, so the bivalve rule's position at the end of the list changes nothing else.

- [ ] **Step 4: Update the cascade comments**

In `R/functions/trait_lookup/harmonization.R`, `harmonize_environmental_position()`, replace

```r
  # 2. Pelagic taxa (phyto- and zooplankton, medusae). Before the depth rule:
  #    a copepod caught at 10-20 m is pelagic, not epibenthic (F36).
```

with

```r
  # 2. Pelagic taxa (phyto- and zooplankton, medusae) and infaunal bivalves.
  #    Before the depth rule: a copepod caught at 10-20 m is pelagic, not
  #    epibenthic (F36), and a shallow Mya is endobenthic (C-6a).
```

and replace

```r
  # 4. Other taxonomic rules (infaunal bivalves, fish by order)
```

with

```r
  # 4. Other taxonomic rules (fish by order)
```

- [ ] **Step 5: Parse-check and run the tests**

```bash
for f in R/config/harmonization_config.R R/functions/trait_lookup/harmonization.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"; done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('test-trait-lookup-correctness.R', 'test-trait-vocabulary.R', 'test-hotfix.R', 'test-trait-lookup-species.R', 'test-layer1-quality.R', 'test-ui-inputs-have-handlers.R', 'test-harmonization-settings-server.R', 'test-harmonization-config-io.R', 'test-trait-cache-config-hash.R')) { r <- as.data.frame(testthat::test_file(file.path('tests/testthat', f), reporter = 'silent', stop_on_failure = FALSE)); cat(f, 'tests', nrow(r), 'failing', sum(r\$failed > 0 | r\$error), 'skipped', sum(r\$skipped), 'passed', sum(r\$passed), '\n') }"
```

Expected (verified on the scratch copy):
- two `OK` lines;
- `test-trait-lookup-correctness.R tests 31 failing 0 skipped 0 passed 116`;
- `failing 0` for the other eight files, with the same counts as master:
  - `test-trait-vocabulary.R` 58 tests;
  - `test-hotfix.R` 10, including its "EP returns EP4 for infaunal bivalves";
  - `test-trait-lookup-species.R` 33 (18 skipped);
  - `test-layer1-quality.R` 6;
  - `test-ui-inputs-have-handlers.R` 6: the `infaunal_bivalves` flag is still harvested;
  - `test-harmonization-settings-server.R` 29;
  - `test-harmonization-config-io.R` 26;
  - `test-trait-cache-config-hash.R` 9.

- [ ] **Step 6: Commit**

```bash
git add R/config/harmonization_config.R R/functions/trait_lookup/harmonization.R tests/testthat/test-trait-lookup-correctness.R
git commit -m "$(cat <<'EOF'
fix(traits): C-6a infaunal bivalves are classified before the depth rule

User decision (option b): the infaunal_bivalves taxon rule moves into the
pre-depth group of TRAIT_VOCAB, so a Mya or Macoma whose only EP evidence
is a shallow OBIS depth range is EP4, not EP3. Habitat text still wins,
the rule switch still works, and fish keep the depth rule before their
order rules. No trait_vocab_version bump (no code changes meaning); the
config hash, which includes the vocabulary, refreshes cached envelopes.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Convention note, full suite, push and PR

**Files:**
- Modify: `CONTRIBUTING.md` (one bullet)

**Interfaces:**
- Consumes: Tasks 1-6 and the BASELINE line from Task 0.
- Produces: the fix PR, squash-merged without a version bump (Execution ruling 1).

- [ ] **Step 1: Add the convention**

In `CONTRIBUTING.md`, "Trait-pipeline & concurrency patterns", replace

```
  then ignored until rebuilt or retrained, and the production offline DB
  must be rebuilt after the deploy.

## Commit Messages
```

with

```
  then ignored until rebuilt or retrained, and the production offline DB
  must be rebuilt after the deploy.
- **Normalise database fields where they are read; batch lookups go
  through the safe wrapper.** A rank or column a database does not report
  arrives as `character(0)`, and a zero-length value makes `&&` / `if`
  error. Read taxonomy and other scalar fields with `.scalar_chr(x)`
  (`R/functions/validation_utils.R`; NA for NULL, zero-length, NA or "").
  Loops over species call `lookup_species_traits_safely()`, never
  `lookup_species_traits()` directly, so one bad taxon becomes a warning
  and an error row instead of aborting the batch. When a lookup fix changes
  values the cache already holds, bump `TRAIT_LOOKUP_REVISION`
  (`harmonization.R`).

## Commit Messages
```

- [ ] **Step 2: Lint the new code (no new lints)**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('tests/testthat/test-trait-lookup-correctness.R', 'R/functions/validation_utils.R', 'R/functions/trait_lookup/database_lookups.R', 'R/functions/trait_lookup/harmonization.R', 'tests/testthat/capture_fixtures.R', 'tests/testthat/test-trait-lookup-unit.R', 'tests/testthat/test-trait-lookup-live.R', 'R/functions/taxonomic_api_utils.R', 'R/functions/trait_lookup/orchestrator.R', 'R/modules/trait_research_server.R')) { l <- lintr::lint(f, linters = list(lintr::line_length_linter(120), lintr::trailing_whitespace_linter(), lintr::assignment_linter())); cat(f, length(l), '\n') }"
```

Expected (verified): `0` for the first seven files. `taxonomic_api_utils.R` shows `7`, `orchestrator.R` `33` and `trait_research_server.R` `2`, all pre-existing (the same counts on master). Do not reformat those.

- [ ] **Step 3: Run the full suite**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('AFTER pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: `fail 0 error 0`.

- `pass` = BASELINE + about 116 from `test-trait-lookup-correctness.R` (Tasks 1-6), + 4 from the two fixture tests after a re-capture.
- `skip` = BASELINE, or BASELINE + 2 with the offline fallback, + 4 for the new live tests (they skip offline).

The full suite was not run in planning. If a file outside the ones this plan names fails, compare it against master before changing anything.

- [ ] **Step 4: Commit**

```bash
git add CONTRIBUTING.md
git commit -m "$(cat <<'EOF'
docs(contributing): normalise lookup fields with .scalar_chr(); batch lookups use the safe wrapper


Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

- [ ] **Step 5: Push and open the PR (STOP - ask the user before running)**

```bash
git push -u origin fix/c6a-trait-lookups
gh pr create --base master --title "fix(traits): C-6a trait lookups - WoRMS units, FishBase grams, name resolution, no batch abort" --body "$(cat <<'EOF'
Implements spec C (`docs/superpowers/specs/2026-09-26-fix-c-trait-pipeline-correctness-design.md`) PR C-6a: C2.1-C2.7 and the fixture re-capture.

- **F33 (lookup side):** missing WoRMS ranks become NA via `.scalar_chr()` instead of `character(0)`, which aborted the lookup; one failing species becomes a warning and an error row (`lookup_species_traits_safely()`) instead of aborting the Trait Research batch.
- **F32:** WoRMS body sizes are converted from each row's unit record (a 20 cm mussel was 2 cm; kg weight rows are no longer lengths).
- **F78:** FishBase weight stays in grams (cod was 96 t).
- **F79:** the AlgaeBase fallback reads the WoRMS tibble by column and warns on failure.
- **F9:** WoRMS classifications are "medium"; name overrides and the unmatched-class Fish default stay "low".
- **F10:** exact common names win; one species high, one of several in the region medium, else the first by name low with a warning. The region check uses `faoareas()`/`ecosystem()` (rfishbase 5 has no `distribution()`, so the old filter never ran). Region-keyed classify cache.
- **F11:** names are looked up as given before any singular form ("Pollachius virens" resolves).
- Fixtures: WoRMS and FishBase re-captured with `RUN_LIVE_TESTS=true capture_fixtures.R worms fishbase` (review the .rds diff: Mytilus 20 cm, cod 96000 g). <or: "not re-captured (offline); the two fixture asserts skip with the command">
- **Cache:** pre-C-6a `cache/taxonomy` envelopes are refreshed on first use (`TRAIT_LOOKUP_REVISION` in the config hash).
- **EP (user decision):** the infaunal-bivalve rule runs before the depth rule, so a Mya with only a shallow OBIS depth is EP4, not EP3. Fish are unchanged. No `trait_vocab_version` bump.

No offline-DB rebuild is needed (the writer is unchanged). Deviations, rulings on the routed review items and the user's decisions are in `docs/superpowers/plans/2026-09-29-c6a-trait-lookups.md`.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Fill in the `<...>` fixture line before running. Expected: a PR URL and green CI. The offline testthat job installs worrms, rfishbase and testthat (`ci.yml`), so it runs `test-trait-lookup-correctness.R` in full; if a runner lacks one of them, those tests skip with a reason rather than fail. The new live tests skip there and run nightly.

- [ ] **Step 6: Merge (STOP - ask the user)**

Squash-merge per Execution ruling 1 (no version bump in this PR). Then continue with Task 8 from the updated master.

---

### Task 8: Release 1.6.2 (separate release PR)

Run only after the Task 7 PR is merged. The version follows Execution ruling 2.

**Files:**
- Modify: `VERSION`, `R/config.R` (the `load_version_info()` fallback), `app.R` (header), `README.md`, `CHANGELOG.md`
- Create: `docs/releases/1.6.2-notes.md`

**Interfaces:**
- Consumes: the merged master; tags `v1.6.0` and `v1.6.1`.
- Produces: `VERSION=1.6.2`, `## [1.6.2] - <date>` at the CHANGELOG head, and tag `v1.6.2`.

- [ ] **Step 1: Branch and check tags**

```bash
git checkout master && git pull --ff-only
git tag -l "v1.6.*"
grep -n -m2 "^## \[" CHANGELOG.md
ls docs/releases/
git checkout -b release/1.6.2
```

Expected:
- tags `v1.6.0` and `v1.6.1`;
- the CHANGELOG head is `## [1.6.1] - 2026-09-29`;
- `docs/releases/` lists `1.5.0-results-changed.md`, `1.5.2-notes.md`, `1.5.3-notes.md`, `1.6.0-notes.md` and `1.6.1-notes.md`.

If `v1.6.1` is missing, **STOP - ask the user**: the generator would merge 1.6.1's commits into 1.6.2.

- [ ] **Step 2: Bump the version strings**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.6.2 --name "Trait Lookups"
sed -i 's/\r$//' VERSION app.R README.md
sed -i 's/^GIT_BRANCH=.*/GIT_BRANCH=master/' VERSION
```

`version_bump.R` writes `VERSION` with CRLF and records the current branch; the two `sed` lines fix both. It does not touch the `R/config.R` fallback. In `load_version_info()`'s `version_info <- list(...)`, set:
- `VERSION = "1.6.2"`;
- `VERSION_NAME = "Trait Lookups"`;
- `RELEASE_DATE = "<release date>"`;
- `PATCH = 2`.

Keep `MAJOR = 1`, `MINOR = 6` and `STATUS = "stable"`.

```bash
grep -n "^VERSION=\|^VERSION_NAME=\|^RELEASE_DATE=\|^MINOR=\|^PATCH=\|^GIT_BRANCH=" VERSION
grep -n 'VERSION = "\|VERSION_NAME = "\|RELEASE_DATE = "\|MINOR = \|PATCH = ' R/config.R | head -5
grep -n "CURRENT VERSION" app.R
grep -n "1\.6\.[0-9]" README.md
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config.R'); parse(file='app.R'); cat('OK\n')"
```

Expected:
- `VERSION=1.6.2`, `MINOR=6`, `PATCH=2`, `GIT_BRANCH=master` and today's `RELEASE_DATE`;
- the same values in `R/config.R`;
- `# CURRENT VERSION: v1.6.2 (...)`;
- README shows 1.6.2 and no 1.6.1;
- `OK`.

- [ ] **Step 3: Write the hand-written release notes**

Create `docs/releases/1.6.2-notes.md` with exactly:

```markdown
### Changed — trait lookup values corrected

- **WoRMS body sizes use the unit WoRMS gives.** Each WoRMS body-size record carries its unit, which was ignored:
  every invertebrate size was read as millimetres, so a 20 cm blue mussel became 2 cm. Sizes are now converted from
  their own unit, and weight records (kg) are no longer read as lengths. A size without a unit falls back to the old
  class rule, with a warning. Size classes (MS) taken from WoRMS change for invertebrates (blue mussel MS3 -> MS5).
- **FishBase weights are grams.** The maximum weight was multiplied by 1000 (a 96 t cod).
- **Scientific names are no longer shortened.** "Pollachius virens" was looked up as "Pollachius viren"; the name as
  given is now tried first, and the singular form only if nothing matches.
- **Common-name matches are graded.** An exact common name wins over names that merely contain it. One species is
  "high" confidence; one of several that occurs in the model's region is "medium"; otherwise the alphabetically first
  species is "low", with a warning. The region filter never ran before (it called a function rfishbase 5 no longer
  has); it now uses FishBase's FAO areas and ecosystems.
- **WoRMS classifications are "medium" confidence** (they were always "low", so the Ecopath import discarded them).
  A classification forced by the common name, or a class WoRMS names but EcoNeTool does not recognise, stays "low".
- **The AlgaeBase fallback works.** It misread the WoRMS response and failed for every taxon.
- **One failing species no longer aborts a Trait Research run.** It is listed with its error, and the rest of the
  batch completes.
- **Burrowing bivalves stay endobenthic (EP4) whatever their depth.** A bivalve whose only position evidence was a
  shallow depth range (Mya, Macoma, Cerastoderma) was classed as epibenthic (EP3). The bivalve rule now runs before
  the depth rule; explicit habitat text still wins, and fish are unchanged.

### Notes

- No offline-database rebuild is needed.
- Cached lookups (`cache/taxonomy/`) from 1.6.1 or earlier are refreshed automatically on first use.
- Classification caches are now keyed by region; the old `*.classify.rds` files are no longer read and can be deleted.
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
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.6.2
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"   # second run must print nothing
sed -i 's/\r$//' CHANGELOG.md
git diff CHANGELOG.md | grep '^-' | grep -v '^---'
grep -n -m2 "^## \[" CHANGELOG.md
grep -n "^### Changed\|^### Results changed\|^### Notes" CHANGELOG.md
tail -c 2 CHANGELOG.md | od -c
```

Expected:
- **First run:** it prints one `re-inserted ...` line per notes file whose section the generator dropped: 1.5.0, 1.5.2, 1.5.3, 1.6.0, 1.6.1 and 1.6.2. That is six lines when #18's release shipped `1.6.1-notes.md`; a file whose first line is still present is skipped silently.
- **Second run:** it prints nothing.
- **The `-` list:** it is empty, or holds only footer compare-links.
- **The head:** `## [1.6.2] - <date>`, then `## [1.6.1]`.
- **Section headings:** a `### Changed` line directly under `[1.6.2]` and one under `[1.6.0]`; "### Results changed" under `[1.5.0]`; "### Notes" under each release whose notes carry one.
- **Ending:** the file ends in exactly one `\n`.

If any other `-` line appears, `git checkout -- CHANGELOG.md`, paste `scripts/generate_changelog.R --preview --version 1.6.2` above the old head by hand, and re-run the helper.

- [ ] **Step 5: Commit**

```bash
git add VERSION R/config.R app.R README.md CHANGELOG.md docs/releases/1.6.2-notes.md
git commit -m "$(cat <<'EOF'
chore(release): 1.6.2 - trait lookups (C-6a)

Version 1.6.2 in VERSION, the R/config.R fallback, the app.R header and
README. CHANGELOG regenerated with every hand-written note re-inserted and
the new 1.6.2 "trait lookup values corrected" section, also kept in
docs/releases/1.6.2-notes.md.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

- [ ] **Step 6: Push, PR, merge, tag (STOP - ask the user before each)**

```bash
git push -u origin release/1.6.2
gh pr create --base master --title "chore(release): 1.6.2 - trait lookups" --body "$(cat <<'EOF'
Release PR for C-6a (trait lookups). Version strings, regenerated CHANGELOG with every hand-written note re-inserted, and the new "trait lookup values corrected" section (`docs/releases/1.6.2-notes.md`).

After merge: tag `v1.6.2` on the merge commit and deploy. No offline-DB rebuild is needed.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

After the user merges it:

```bash
git checkout master && git pull --ff-only
git tag -a v1.6.2 -m "1.6.2 - Trait Lookups" && git push origin v1.6.2
```

---

### Task 9: Deploy 1.6.2 (no offline-DB rebuild)

Every step touches production or the shared server. **Each is STOP: show the user the exact command and wait for confirmation.** Deploy from the merged, tagged master.

**Files:** none in the repo. Remote: `/home/razinka/EcoNeTool_staging/` and `/srv/shiny-server/EcoNeTool/`.

**Interfaces:**
- Consumes: `v1.6.2` on master.
- Produces: production on 1.6.2. The offline DB is untouched.

- [ ] **Step 1: Pre-deploy check (local, read-only)**

```bash
git checkout master && git pull --ff-only && git log -1 --oneline
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; cd ..
```

Expected: the 1.6.2 release commit is HEAD, and there are no errors. The script must run from inside `deployment/`.

- [ ] **Step 2: Upload to staging (STOP)**

```bash
powershell ./deploy-windows.ps1 -NoSudo
```

Expected: the script empties staging and uploads the code, together with the offline-DB build script and its tracked inputs, which it ships since #17. It skips `data/` by default. It never uploads `config/api_keys.*`, `config/harmonization_custom.json`, `.Renviron`, `*.bak` or `*safeBackup*`. Ignore any printed `sudo` / `rm -rf /srv/shiny-server/EcoNeTool/*` suggestion.

- [ ] **Step 3: Copy over the live tree and reload (STOP)**

```bash
ssh razinka@laguna.ku.lt "cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt"
```

Expected: silent success. `data/`, `cache/` (including `cache/offline_traits.db`) and `.Renviron` survive, because `cp -rT` deletes nothing.

- [ ] **Step 4: Verify the code (STOP - read-only)**

```bash
ssh razinka@laguna.ku.lt "cd /srv/shiny-server/EcoNeTool && grep -c 'worms_body_size_cm <- function' R/functions/trait_lookup/database_lookups.R; grep -c 'resolve_fishbase_name <- function' R/functions/taxonomic_api_utils.R; grep -c 'lookup_species_traits_safely(' R/modules/trait_research_server.R; grep -c 'TRAIT_LOOKUP_REVISION <- ' R/functions/trait_lookup/harmonization.R; grep -c 'also holds the infaunal-bivalve' R/config/harmonization_config.R; grep '^VERSION=' VERSION; stat -c %y restart.txt; ls -la cache/ | grep offline_traits"
curl -sL -o /dev/null -w '%{http_code}\n' http://laguna.ku.lt/EcoNeTool/
```

Expected:
- five `1` lines;
- `VERSION=1.6.2`;
- a fresh `restart.txt`;
- `offline_traits.db` present with its pre-deploy timestamp;
- `200`.

No rebuild follows (Execution ruling 3).

- [ ] **Step 5: Smoke test (user or browser automation, with the user's go-ahead)**

On https://laguna.ku.lt/EcoNeTool/:
1. **Trait Research**, manual input `Mytilus edulis` and `Pollachius virens`: the run completes. Mytilus has MS5 when its size comes from WoRMS; it may also be served from the offline DB. Pollachius virens resolves (its FishBase row is present in the raw details).
2. **Trait Research**, manual input `Mya arenaria`: EP is EP4 (it was EP3 when only OBIS depth decided). If it is served from the offline DB, the DB's own EP applies; the DB is not rebuilt, and its writer never used the depth cascade.
3. **Trait Research**, manual input `Gadus morhua` plus a nonsense line such as `Xx yy`: the run completes, and the summary shows `Failed: 0`, because a name that is not found is "no data", not an error.

---

## Self-Review

1. **Spec coverage.**
   - C2.1 F33: `.scalar_chr()` at the taxonomy extraction and before the size block -> Task 1. The harmonizer guards are covered by C-5 (deviation 1). The per-species batch `tryCatch` with the spec's warning and error row -> Task 3; the outer tryCatch is kept for setup.
   - C2.2 F32 (children Unit, `to_cm`, qualitative -> class fallback with the spec's warning, max, `size_unit_source`) -> Task 1 (deviations 3-4).
   - C2.3 F78 -> Task 1.
   - C2.4 F79 (NROW guard, `$phylum[1]`, warning plus `<<-`) -> Task 1.
   - C2.5 F9 (medium, overrides low, dead branch deleted) -> Task 2 (deviation 5).
   - C2.6 F10 (exact-first, graded confidence, warning text, `<clean_name>__<region|any>` key) -> Task 2 (deviations 6-7).
   - C2.7 F11 (original sci -> common -> singular) -> Task 2 (deviation 8).
   - **Section 5 `test-trait-lookup-correctness.R` rows:**
     - zero-length class through the harmonizers -> the C-5 test plus Task 1's end-to-end test;
     - the Mytilus child unit -> Task 1;
     - no child, Bivalvia, warning -> Task 1;
     - FishBase 96000 -> Task 1;
     - the `wm_records_name` tibble -> Task 1;
     - `query_worms` medium plus the enhanced filter -> Task 2;
     - "Atlantic cod" / "Arctic cod" high, ambiguous low plus warning plus region key -> Task 2;
     - Pollachius virens / Ammodytes original first -> Task 2;
     - the testServer batch -> Task 3;
     - the SHARK row is C-6b, not here.
   - **Fixture paragraph:** re-capture with `capture_fixtures.R` under `RUN_LIVE_TESTS=true`, diff reviewed, live asserts Mytilus ≥ 10 cm and cod < 1e6 g -> Task 4 (deviation 11).
   - **Section 6:** item 2 -> Tasks 0-7; release and deploy -> Tasks 8-9 (no rebuild, Execution ruling 3).
   - **Section 8:** criterion 2's lookup items ("a zero-length class does not crash", "Mytilus ≥ 10 cm, cod weight in grams", "Pollachius virens resolves") -> Tasks 1, 2 and 4. Criterion 5's "no batch abort" -> Task 3. Its other items (T_method, labels) belong to C-8.
   - **User decisions (2026-09-29):** 1 cache refresh -> Task 5; 2 any length dimension -> Task 1 unchanged (deviation 4); 3 option (b) EP order -> Task 6 (deviation 13, Execution rulings 3 and 4b); 4 F9 default low -> Task 2 (deviation 5).
2. **Placeholder scan.** Every code step carries complete code, identical to the files run on the scratch copy of master. The 28 Task 1-3 tests fail before and pass after; so do Task 5's test and Task 6's Mya test. Task 6's fish test is a guard that passes throughout. Values filled at run time: `<scratchpad>`, `<release date>`, and the `<...>` fixture line of the PR body (re-captured or not).
3. **Type consistency.**
   - `.scalar_chr()` (Task 1) is used in Tasks 1-2.
   - `to_cm()` and `worms_body_size_cm(attributes, class_name, species_name)` are defined and used in Task 1.
   - `resolve_fishbase_name()` returns `list(valid_name, match_confidence)`. It is consumed by `query_fishbase()` (Task 2) and by the live test (Task 4).
   - `lookup_species_traits_safely()` (Task 3) is used by the server, `batch_lookup_traits()` and CONTRIBUTING (Task 7).
   - `TRAIT_LOOKUP_REVISION` (Task 5) is named in CONTRIBUTING and in the Task 9 grep. The Task 6 config comment is grepped in Task 9 as well.
   - The per-task pass counts 49 / 86 / 107 / 109 / 116 (after Tasks 1 / 2 / 3 / 5 / 6) come from the per-test sums of the end-state run on the scratch copy.
4. **Review Focus.** Each of the six lines names the test that pins it (Tasks 1, 2, 3, 5 and 6).
