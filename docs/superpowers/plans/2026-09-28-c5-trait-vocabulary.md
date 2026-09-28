# C-5: Trait Vocabulary (F17, F18, F35, F36, F38, F70, F71, F77) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every place that assigns, labels, validates or prices an MS/FS/MB/EP/PR code uses one vocabulary - the food-web model's - and codes written in the old vocabulary (live offline DB, `cache/taxonomy/*.rds`, the shipped ML model's MB predictions) are never served against the new matrices. Ship as its own minor release 1.6.0, deploy, and rebuild the production offline DB.

**Architecture:** `R/config/harmonization_config.R` gains a `TRAIT_VOCAB` constant (version 2, labels for all five traits, default MB/EP/PR text patterns, pattern precedence, ordered taxon rules) with accessors `get_trait_vocab()`, `current_trait_vocab_version()`, `trait_codes()`, `trait_definitions()`, `trait_code_label()`. `R/functions/trait_lookup/harmonization.R` gains a boundary-aware pattern engine (`trait_patterns()`, `classify_by_patterns()`) and a rule engine (`apply_taxon_rules()`); the live cascades, the fuzzy ontology harmonizers, the offline-DB writer, the local BVOL/SpeciesEnriched mappers, the UI legends and the help tables all go through them, and a static guard test forbids private vocabulary regexes. The model gains PR1 and MS1/MS6 columns, a recalibrated MB1 (sessile consumer) row, a strict `p > threshold` link test and code validation from the vocabulary. The DB writer stamps `metadata.trait_vocab_version`; `lookup_offline_traits()` skips a DB with another version; cache envelopes carry `trait_vocab_version` and `read_cache_field(..., vocab_version =)` treats a mismatch as a miss; `harm_config_hash()` hashes `TRAIT_VOCAB` in (B's F72 key); an ML model not trained on v2 labels predicts no MB.

**Tech Stack:** R 4.4.1, shiny 1.11.1 (`MockShinySession`, `withReactiveDomain`), testthat 3.3.2, withr, DBI/RSQLite, processx, jsonlite, digest, htmltools. Windows dev box (Git Bash / PowerShell), Linux deploy target (laguna.ku.lt, shiny-server).

**Spec:** `docs/superpowers/specs/2026-09-26-fix-c-trait-pipeline-correctness-design.md` - section 4 C1 (C1.1-C1.7), C3.5 last bullet (vocab gate), C3.7 last bullet (envelope vocab check + F72 contribution), section 5 `test-trait-vocabulary.R` table and the vocab rows of the `test-trait-provenance.R` table ("fixture DB with `trait_vocab_version = 1`", "envelope with an old `trait_vocab_version`"), section 6 Rollout item 1 (PR C-5) and items 5-8, section 8 items 1-4 and 7; plus `docs/superpowers/specs/2026-09-26-fix-overview.md` (review questions 2-5, shared rules).

> **Execution rulings (user decisions, 2026-09-28) - these override the tasks below where they conflict:**
> 1. **Release flow:** the fix PR (Tasks 0-6) is squash-merged WITHOUT a version bump or CHANGELOG regeneration. The release then runs as its own PR cut from the updated master (Task 7): `scripts/version_bump.R`, CRLF -> LF, `GIT_BRANCH=master`, the `R/config.R` fallback, `scripts/generate_changelog.R`, re-insert `docs/releases/1.5.0-results-changed.md`, `1.5.2-notes.md` and `1.5.3-notes.md` under their headers and add the new `docs/releases/1.6.0-notes.md`, exactly one trailing newline, merge, then tag.
> 2. **Version (decided):** C-5 ships alone as **1.6.0**, with the "trait codes changed" CHANGELOG section written to `docs/releases/1.6.0-notes.md`; C-6a, C-6b and C-8 ship as 1.6.x. C-5 is the PR that changes codes, the link threshold and the model matrices, and the one that makes a production DB rebuild mandatory, so the MINOR bump marks exactly that boundary. The one "trait codes changed" bullet C-5 does not own - the confidence label bands (0.34/0.67) - goes into C-8's 1.6.x notes.
> 3. **Strict threshold + MB1 recalibration (decided):** keep the strict `p > threshold` (spec C1.6) AND set `MB_MB["MB1", c("MB2", "MB3", "MB4", "MB5")]` (rows = consumer, so: sessile consumer x mobile prey) from 0.05 to **0.10**, so at the default 0.05 a sessile consumer keeps its non-sessile prey. All other cells unchanged. This is a user-approved deviation from spec section 3's "no re-tuning" non-goal (deviation 18) and a calibration placeholder, stated in the CHANGELOG notes.
> 4. **MAREDAT (decided):** keep the current group -> MB/PR mapping (deviation 12).
> 5. **ML (decided):** MB predictions stay off, with a single warning per process, until C-8 retrains the models (deviation 16).
> 6. Every outward step (push, PR, merge, tag, anything on laguna) is **STOP - ask the user**. Production `data/` must never be deleted.

## Global Constraints

- Branch `fix/c5-trait-vocabulary`, cut from `master` at `6db5082` (v1.5.3) or later. Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- Spec C1.1: "Vocabulary is not user-overridable. The labels, `pattern_precedence`, `taxon_rules` and `trait_vocab_version` live in a `TRAIT_VOCAB` constant ... read via `get_trait_vocab()`, never from a session config or a saved JSON. For `*_patterns` (which users may tune), use `modifyList(TRAIT_VOCAB$patterns[[k]], get_harm_config()[[k]] %||% list())`."
- Canonical codes (spec C1.1 table): MB1 Sessile, MB2 Passive floater / drifter, MB3 Crawler-burrower, MB4 Facultative / limited swimmer, MB5 Obligate swimmer; EP1 Pelagic, EP2 Benthopelagic, EP3 Epibenthic, EP4 Endobenthic / infaunal; PR0 None / soft body, PR1 Mucus / cuticle, PR2 Tube, PR3 Burrow refuge, PR4 Thin exoskeleton, PR5 Soft shell, PR6 Hard shell, PR7 Spines / ossicle plates, PR8 Armoured. MS7 "is never prey ... and needs no matrix column".
- Spec C1.2: each pattern gets "a leading boundary only, `(?<![a-z])(?:alt)` (perl)"; "returns the first matching code, or `NA_character_`". C1.7: "NA/empty text returns NA with no warning"; an invalid pattern emits `warning("[harmonization] invalid <trait> pattern for <code>: ...")` and is skipped; `apply_taxon_rules` "treats NULL or zero-length ranks as absent".
- User decision (2026-09-28): `MB_MB["MB1", c("MB2", "MB3", "MB4", "MB5")] = 0.10`; `MB_MB["MB1", "MB1"]` stays 0.95; every other matrix cell stays as it is.
- Spec C1.6: "`PR_MS` gains a PR1 row equal to the PR0 row"; "`EP_MS` and `PR_MS` gain an MS1 column (copy of MS2) and an MS6 column (copy of MS5)"; "The hard-coded `0.05` fallback ... is deleted"; "The link test becomes strictly `p > threshold`. The UI default threshold stays 0.05."; "The existing self-loop skip ... is kept and gets a test."
- Spec C3.5 last bullet, verbatim warning: `warning("[offline] DB vocab v%s != config v%s; rebuild required, offline DB skipped")` "once per process", returning NULL.
- Spec C3.7 last bullet: "`read_cache_field` treats `envelope$trait_vocab_version != cfg$trait_vocab_version` as stale. `cfg$trait_vocab_version <- 2L` is included in B's F72 config hash."
- B1 rules stay: read configs only through `get_harm_config()` (exceptions: the build script and the validator/loader base); `validate_harmonization_config(cfg) -> list(ok, errors, config)` must accept every section this PR adds.
- Out of scope here (later C PRs; do not touch): F26 (`harmonize_protection` default PR0 -> NA) and `harmonize_mobility`'s MB4 default; `PR_source`/`T_method`/confidence (C-8); `.scalar_chr()` and the per-species batch `tryCatch` (C-6a); SHARK (C-6b); the offline full-hit that returns default-harmonized codes to a custom-config session (C-8, CodeRabbit follow-up); `lookup_offline_traits()`'s wd-relative default `db_path` (F24, C-8); `scripts/train_trait_models.R` (C-8).
- CLAUDE.md conventions: `warning()` not `message()` in error handlers; `<<-` in error closures; `app_path()` for runtime paths; `skip_if()` / `skip_if_not_installed()`, never `if (cond) expect_*()`; `<-`; 120-char lines; no tabs or trailing whitespace. Parse-check every edited `.R` file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='<path>'); cat('OK\n')"`.
- Tests: one file `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/<file>')"`; full suite `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"` in the FOREGROUND (about 10 min). One R process at a time (16 GB RAM; background jobs get killed). `tests/run_all_tests.R` needs `dggridR`; do not use it.
- Verification basis: every code block below was applied to a scratch copy of master and run (end state, re-verified after the MB1 recalibration: `test-trait-vocabulary.R` 181 pass; full suite +183 pass, no new failures). The new tests' fail-before results were run against master. The "existing files FAIL 0" lines at the Task 1-4 boundaries were checked by reasoning, not by replaying each intermediate state: if a listed existing file fails at a boundary, first check whether a later step of the same task (or the next task) resolves it before investigating.
- Write R files and multi-line replacements with the Write/Edit tools only, never shell heredocs (heredocs have dropped backslashes in this harness). In R source every regex backslash is doubled (`"\\w"`); the code blocks below are exactly what goes into the file.
- `git add` explicit paths only. Never stage `WBGIFSV5ISSUE70.pdf`, anything under `config/`, `cache/`, or a `*-laguna-safeBackup-*` copy. Never create, delete, move or read `R/config-laguna-safeBackup-0001-CONTAINS-freshwaterecology-API-KEY.R.bak`; never touch `cache/offline_traits-laguna-safeBackup-0001.db`.

## Deviations from the spec (and why)

1. **Taxon-rule shape** is `list(match = list(<taxonomy field> = <regex>, ...), code, flag, text, override_text)`, not `list(rank, pattern, code)`. Several rules need a conjunction (phylum + class, class + order) or a non-rank field (`living_habit` for Annelida tubes, `feeding_mode` for photosynthesizers); without that the C1.5 guard would still find `grepl("tube", ...)` in `harmonize_protection`. `flag` carries the existing `taxonomic_rules` switch (checked through `is_rule_enabled()`), `text` the Hydrozoa medusa condition.
2. **Mobility precedence is MB1, MB4, MB5, MB3, MB2** (spec: MB1, MB5, MB4, ...). "limited_swimmer" and "facultative swimmer" contain "swim"; MB5 first would turn all 94 BIOTIC `limited_swimmer` rows into obligate swimmers (verified on `data/biotic_traits.csv`).
3. **Protection precedence puts PR2 before PR6** (spec: PR6 before PR2). The 19 BIOTIC `calcareous_tube` rows (serpulids - the PR2 label's own example) would otherwise become PR6 via "calcareous".
4. **Pattern details beyond the spec's text:** multi-word alternatives use `.?` because the data use underscores (`limited_swimmer`, `tube_dweller`, `free_living`); stems (`attach`, `infaun`, `endobenth`, `chitin`) so "Sedentary, temporary attachment" and "chitinous" (174 BIOTIC rows) still match; prefixed alternatives `(holo|mero|zoo|phyto)?plankton` and `(epi|meso|bathy|abysso)?pelagic`, because the leading boundary would otherwise block "zooplankton" and "mesopelagic"; PR8 keeps `^carapace$` and qualified carapaces instead of a bare `carapace`, so "thin carapace" stays PR4 and the ontology's "bivalve_carapace" PR6; `calci\\w*.exoskeleton` -> PR8 (82 BIOTIC decapods, previously PR4); PR0 keeps v1's extra alternatives (`unprotected|jellyfish|cephalopod|crustose|cushion|stalked`) but drops `no shell|no armor`, which precedence makes unreachable ("no shell" now reads as PR6; see Review Focus). EP3 adds `epilith|epiflor|epiphyt|epizo|crevice` (SpeciesEnriched and BIOTIC labels) and EP4 `buried|lithotom`.
5. **EP order is spec-literal:** text -> *pelagic* taxon rules -> depth -> the remaining taxon rules (infaunal bivalves, fish by order) -> default EP3. Moving all taxon rules before depth would also change shallow fish and bivalves, which the spec does not ask for.
6. **Labels carry `label`, `description`, `examples`** (spec: `name, description, examples`): every existing reader and test uses `$label`.
7. **Legacy-pattern neutralisation (addition).** A JSON exported before C-5 carries the whole config, including the v1 MB/EP/PR default patterns verbatim. `validate_harmonization_config()` merges it over the defaults, so a stale server default or import would silently restore v1 behaviour for every key that kept its name. `trait_patterns()` ignores (a) keys the vocabulary does not define (e.g. `MB2_burrower`) and (b) values identical to the recorded v1 default (`TRAIT_VOCAB$legacy_patterns`). A genuinely customised value is honoured.
8. **`cnidarians_sessile`:** the key stays in `taxonomic_rules` (JSON compatibility) and `validate_harmonization_config()` - the single load/import/save point - warns when it is FALSE; its checkbox is removed (overview "dead UI: remove"), and the two settings-server tests that used it as an arbitrary example rule switch to `bivalves_sessile`. `echinoderms_calcium_plates` keeps its name and now gates the PR7/PR1 rules.
9. **`offline_db_schema_sql()` lives in `R/functions/offline_db_rebuild.R`**, not in the build script: sourcing the script runs a whole build (it takes the lock and opens the tmp DB). The file is already sourced by both `app.R` and the script.
10. **`read_cache_field()` gets an explicit `vocab_version = NULL` (opt-in)**, not a default that calls the vocabulary: `classify_species_api` envelopes share the reader, `validation_utils.R` is sourced before the config, and the batch reader must compute it in the parent process like `config_hash`. The three trait readers pass it; B1's static test strings are updated in the same task.
11. **F72 contribution:** `harm_config_hash()` hashes `get_trait_vocab()` into the config JSON (as `.trait_vocab`) rather than adding `trait_vocab_version` to `HARMONIZATION_CONFIG`. A config key could be pinned by a JSON file (`validate_harmonization_config()` keeps every key the defaults have); `TRAIT_VOCAB` cannot. It also covers phylogenetic imputation, which compares `config_hash` itself.
12. **MAREDAT group -> MB/PR mapping is unchanged** (spec lists "copepod/cladoceran MB" among the regexes to replace). Its inputs are taxon labels ("copepoda", "cladocera"), which `classify_by_patterns()` has no text to match, and the table contains no vocabulary words, so the C1.5 guard passes. Kept by user decision (Execution ruling 4).
13. **SpeciesEnriched EP and PR also go through `classify_by_patterns()`** (spec C1.4 names only its MB lines): the C1.5 guard's scope includes that function, and its EP table sent bare "benthic" to EP4 and shells to PR3. `body_flexibility` is dropped: its values are bending angles ("None (less than 10 degrees)"), which "none" would have read as PR0.
14. **The FS warning in `validate_trait_data()`** drops FS3 instead of relabelling it: FS3 omnivores ARE consumers (`FS_MS` has an FS3 row). The warning now names only FS0.
15. **`construct_trait_foodweb()` calls `validate_trait_data()` and stops on invalid codes** - the spec's "An unknown code now errors in validation before construction".
16. **Additions:** the ML MB gate (Task 5; the shipped `models/trait_ml_models.rds` has pre-v2 MB labels and no version stamp, and `scripts/train_trait_models.R` cannot retrain it - it sources the long-gone `R/functions/trait_lookup.R` - so ML MB predictions stay off until C-8 fixes training); the orchestrator's console labels via `trait_code_label()` (its inline maps said MB2 = "Limited Movement" in one branch and "Burrower" in the other).
17. **Finding, not a deviation:** the spec's appendix says F70's "self-loops at threshold 0" is not reproduced. It is: with `>=`, `ifelse(prob_matrix >= 0, 1, 0)` sets the diagonal (and every zero pair) to 1 (verified on master). The strict `>` fixes it; Task 3 pins it.
18. **User-approved re-tuning (deviation from spec section 3, "Re-tuning the numeric values of ... MB_MB ..." is a non-goal):** `MB_MB["MB1", MB2..MB5]` goes from 0.05 to 0.10. Rows are the consumer, so this is "sessile consumer eats mobile prey". Without it, the strict threshold at the default 0.05 cut every sessile suspension feeder (mussels, barnacles, sponges) off from drifting plankton, and the bundled "simple" example kept 3 links, none to its producer. With it (re-verified): the "simple" example has **6 links at the default**, including Phytoplankton -> Benthic_filter_feeder (p = 0.10), and A's orientation tests in `test-edge-contract.R` pass unchanged at the default threshold - no workaround needed. 0.10 is a calibration placeholder, stated in the 1.6.0 notes. Task 3 pins it.
19. **"No bare `0.05` literal left in `trait_foodweb.R` outside the matrices"** (spec section 5) is checked on `calc_interaction_probability()`'s body only: the spec itself keeps `threshold = 0.05` as the default of `construct_trait_foodweb()` and `trait_foodweb_to_igraph()`.
20. `trait_help_content.R` is not sourced by `app.R` (only listed by `scripts/generate_api_reference.R`); it is updated per spec C1.1 anyway and tested by sourcing it.

## Review Focus

- **A server default or imported JSON saved before C-5** (old MB keys `MB2_burrower`..., v1 EP/PR patterns verbatim, `cnidarians_sessile = FALSE`, no `*_labels` for MB/EP/MS): the app starts, `trait_codes()` is unchanged, no v1 meaning comes back, and the retired rule warns. Pinned by Task 1 test "a pre-C-5 session config cannot drop codes or restore old meanings" and Task 2 test "cnidarians_sessile is retired: setting it FALSE warns".
- **Between deploy and the offline-DB rebuild:** the stale live DB is skipped with exactly one warning per R process and lookups fall back to the live APIs; once rebuilt it is served again without a restart (the gate reads `metadata` on every lookup). Pinned by Task 5 tests "an offline DB from another vocabulary is skipped with one warning (C3.5)" and "an offline DB in the current vocabulary is served".
- **`cache/taxonomy/*.rds` written by <= 1.5.3:** a miss on first read (different config hash and no `trait_vocab_version`), refreshed once, and never a phylogenetic-imputation relative. Pinned by Task 1 test "harm_config_hash changes when the trait vocabulary changes" and Task 5 test "read_cache_field treats another or a missing vocab version as stale".
- **WoRMS taxonomy with zero-length or NA fields** (`class = character(0)`): no harmonizer aborts. Pinned by Task 2 test "a zero-length taxonomy field does not abort the harmonizers (F33)".
- **Messy real-world text** (underscores, mixed case, several terms in one field such as "Crawler or Walker, Mobile, Swimmer" or "Epifaunal, Infaunal"): precedence, not position in the string, decides. Pinned by Task 1 tests "BIOTIC living-habit labels map to EP4 / EP3", "protection text: ..." and "mobility text: ...". Known limitation, not pinned: negations ("no shell") read as the positive term; none occur in the bundled data.

## User decisions (2026-09-28; formerly open questions)

1. **Threshold:** keep strict `>`; recalibrate `MB_MB["MB1", MB2..MB5]` to 0.10 (Execution ruling 3, deviation 18). Other matrix floors remain: e.g. `EP_MS["EP4", MS1/MS5/MS6]` = 0.05, so the "simple" example's EP4 fish still do not eat MS1 phytoplankton at the default.
2. **Version:** 1.6.0 for C-5 alone; notes in `docs/releases/1.6.0-notes.md`; later C PRs are 1.6.x (Execution ruling 2).
3. **MAREDAT:** mapping kept (deviation 12).
4. **ML:** MB predictions stay off with a single warning until C-8 retrains (deviation 16).

## Interfaces produced (C-6a, C-8 and later tasks rely on these)

| Name | Signature / value | Where |
|---|---|---|
| `TRAIT_VOCAB` | list(`trait_vocab_version` = 2L, `labels`, `patterns`, `pattern_precedence`, `taxon_rules`, `legacy_patterns`) | `R/config/harmonization_config.R` |
| `get_trait_vocab()` | `-> TRAIT_VOCAB` | same |
| `current_trait_vocab_version()` | `-> integer(1)` (2L) | same |
| `trait_codes(trait)` | `trait` in "MS","FS","MB","EP","PR" `-> character()`; stops "trait must be one of: ..." | same |
| `trait_definitions()` | `-> list(MS = c(MS1 = "<label>"), ...)` (named chr per trait) | same |
| `trait_code_label(code)` | `-> character(1)`, NA for NA/unknown | same |
| `trait_patterns(trait)` | `trait` in "mobility","environmental","protection","foraging" `-> named list key -> regex` | `R/functions/trait_lookup/harmonization.R` |
| `classify_by_patterns(text, trait)` | `-> code or NA_character_` | same |
| `apply_taxon_rules(taxonomy, trait, text = NULL, override_only = FALSE)` | `trait` in "mobility","environmental_pelagic","environmental","protection" `-> code or NA_character_` | same |
| `offline_db_schema_sql()` | `-> character()` of CREATE statements | `R/functions/offline_db_rebuild.R` |
| `reset_offline_vocab_gate()` | resets the once-per-process vocab warning (tests) | `R/functions/trait_lookup/orchestrator.R` |
| `read_cache_field(cache_file, field, max_age_days = 30, config_hash = NULL, vocab_version = NULL)` | non-NULL `vocab_version`: a different or missing `envelope$trait_vocab_version` is a miss | `R/functions/validation_utils.R` |
| `make_offline_db_fixture(rows = NULL, vocab_version = 2L, env = parent.frame())` | `-> path` of a temp SQLite DB with the writer's schema | `tests/testthat/helper-fixtures.R` |

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `R/config/harmonization_config.R` | Modify (Tasks 1, 2) | `TRAIT_VOCAB` + accessors; config label/pattern sections point at it; retired-rule warning |
| `R/functions/trait_lookup/harmonization.R` | Modify (Tasks 1, 2) | Pattern engine, rule engine, fuzzy + live MB/EP/PR cascades, vocab in the config hash |
| `R/ui/harmonization_settings_ui.R` | Modify (Task 2) | Rule checkboxes = the taxon-rule flags |
| `R/functions/trait_foodweb.R` | Modify (Task 3) | PR1 row, MS1/MS6 columns, MB1 row 0.10 (user-approved), no fallbacks, strict threshold, vocabulary validation, derived `TRAIT_DEFINITIONS` |
| `R/functions/local_trait_databases.R` | Modify (Task 4) | BVOL MB2; SpeciesEnriched MB/EP/PR via the engine |
| `scripts/initialization/build_offline_trait_db.R` | Modify (Tasks 4, 5) | Engine for BIOTIC/ontology/SpeciesEnriched MB/EP/PR, PTDB MB2; shared schema; `trait_vocab_version` metadata |
| `R/ui/trait_research_ui.R` | Modify (Task 4) | FS/MB/EP/PR legends from the vocabulary |
| `R/functions/trait_help_content.R` | Modify (Task 4) | Help tables from the vocabulary |
| `R/functions/trait_lookup/orchestrator.R` | Modify (Tasks 4, 5) | Console labels; offline vocab gate; envelope `trait_vocab_version`; reader passes `vocab_version` |
| `R/functions/offline_db_rebuild.R` | Modify (Task 5) | `offline_db_schema_sql()` |
| `R/functions/validation_utils.R` | Modify (Task 5) | `read_cache_field(..., vocab_version =)` |
| `R/modules/trait_research_server.R`, `R/functions/parallel_lookup.R` | Modify (Task 5) | Pass `vocab_version` |
| `R/functions/ml_trait_prediction.R` | Modify (Task 5) | No MB predictions from a pre-v2 model |
| `tests/testthat/test-trait-vocabulary.R` | Create (Task 1), append (Tasks 2-5) | All C-5 tests |
| `tests/testthat/helper-fixtures.R` | Modify (Task 5) | `make_offline_db_fixture()` |
| `tests/testthat/test-ui-inputs-have-handlers.R` | Modify (Task 2) | Harvest rule flags from `TRAIT_VOCAB$taxon_rules` |
| `tests/testthat/test-harmonization-settings-server.R` | Modify (Task 2) | Example rule `cnidarians_sessile` -> `bivalves_sessile` |
| `tests/testthat/test-trait-cache-config-hash.R` | Modify (Task 5) | Envelope stamp; B1 static strings |
| `CONTRIBUTING.md` | Modify (Task 6) | Convention: vocabulary only in `TRAIT_VOCAB` |
| `VERSION`, `R/config.R`, `app.R`, `README.md`, `CHANGELOG.md`, `docs/releases/1.6.0-notes.md` | Modify/Create (Task 7, release PR) | 1.6.0 |

Not touched (noted for reviewers): the FS inline label maps in the orchestrator's console messages (FS vocabulary unchanged); `foodweb_construction_server.R`'s example datasets (their codes are valid; the "simple" example's traits are odd, e.g. fish at EP4 - User decision 1); `CLAUDE.md` still describes the FS/PR legends as `(get_harm_config() %||% HARMONIZATION_CONFIG)$<labels>` (true now only for RS/TT/ST; the user may update it).

---

### Task 0: Branch, preconditions and baseline

**Files:** none modified.

**Interfaces:**
- Consumes: master with B1/B3 merged (`validate_harmonization_config`, `harm_config_hash`, `read_cache_field(..., config_hash =)`, `finalize_offline_db_build`).
- Produces: branch `fix/c5-trait-vocabulary`; a recorded `BASELINE pass N fail 0 skip M` line for Task 6.

- [ ] **Step 1: Confirm preconditions and create the branch**

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git checkout master && git pull --ff-only
grep -n "^VERSION=" VERSION
git tag -l "v1.5.*"
grep -n "^finalize_offline_db_build <- function\|^harm_config_hash <- function\|^read_cache_field <- function" R/functions/offline_db_rebuild.R R/functions/trait_lookup/harmonization.R R/functions/validation_utils.R
git checkout -b fix/c5-trait-vocabulary
```

Expected: `VERSION=1.5.3`; tags `v1.5.0` .. `v1.5.3`; the three definitions printed. If any is missing, **STOP** and tell the user.

- [ ] **Step 2: Record the baseline**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('BASELINE pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: `fail 0 ... error 0` (about `pass 2195 ... skip 51`). Write the line into your notes. If the baseline is red, stop and report.

---

### Task 1: The vocabulary and the pattern engine (F17, F35, F77, F72 contribution)

**Files:**
- Modify: `R/config/harmonization_config.R` (insert `TRAIT_VOCAB` + accessors before `HARMONIZATION_CONFIG`; replace the `foraging_labels` .. `protection_labels` block)
- Modify: `R/functions/trait_lookup/harmonization.R` (`harm_config_hash`; `harmonize_fuzzy_mobility`; `harmonize_fuzzy_habitat`; replace `get_config_pattern`)
- Create: `tests/testthat/test-trait-vocabulary.R`

**Interfaces:**
- Consumes: `get_harm_config()`, `harm_config_hash()`, `validate_harmonization_config()`, `%||%`.
- Produces: `TRAIT_VOCAB` (including `taxon_rules`, consumed by Task 2), `get_trait_vocab()`, `current_trait_vocab_version()`, `trait_codes()`, `trait_definitions()`, `trait_code_label()`, `trait_patterns()`, `classify_by_patterns()`, `get_config_pattern()` (thin wrapper). `HARMONIZATION_CONFIG` gains `size_labels`, `mobility_labels`, `environmental_labels`; its `mobility_patterns` keys become `MB1_sessile`, `MB2_drifter`, `MB3_crawler_burrower`, `MB4_facultative_swimmer`, `MB5_obligate_swimmer` (EP/PR key names unchanged).

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-trait-vocabulary.R` with exactly this content:

```r
# Sub-project C, PR C-5 (spec C1, C3.5 last bullet, C3.7 last bullet): one
# trait vocabulary. The same MB/EP/PR code used to mean different things in
# the live harmonizer, the fuzzy ontology harmonizer, the offline-DB writer,
# the local BVOL/SpeciesEnriched mappers, the UI legend and the food-web
# model (F17). These tests pin the vocabulary (TRAIT_VOCAB), the pattern
# engine, the taxon rules, the model matrices and the gates that stop codes
# from the old vocabulary being served.

source_app_dependencies()

# Run `code` with `cfg` as this session's harmonization config.
with_session_config <- function(cfg, code) {
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  session$userData$harm_config <- cfg
  shiny::withReactiveDomain(session, code)
}

# ---------------------------------------------------------------------------
# Task 1 - the vocabulary and the pattern engine
# ---------------------------------------------------------------------------

test_that("trait codes and labels come from the vocabulary (MB2 = passive floater)", {
  expect_identical(trait_codes("MB"), paste0("MB", 1:5))
  expect_identical(trait_codes("EP"), paste0("EP", 1:4))
  expect_identical(trait_codes("PR"), paste0("PR", 0:8))
  expect_identical(trait_codes("FS"), paste0("FS", 0:7))
  expect_identical(trait_codes("MS"), paste0("MS", 1:7))
  expect_error(trait_codes("XX"), "trait must be one of")
  expect_identical(unname(trait_definitions()$MB),
                   c("Sessile", "Passive floater / drifter", "Crawler-burrower",
                     "Facultative / limited swimmer", "Obligate swimmer"))
  expect_identical(trait_code_label("PR7"), "Spines / ossicle plates")
  expect_identical(trait_code_label("FS0"), "Primary producer / none")
  expect_true(is.na(trait_code_label("MB9")))
  expect_true(is.na(trait_code_label(NA)))
})

test_that("the config's label and pattern sections are the vocabulary's and pass the validator", {
  expect_identical(HARMONIZATION_CONFIG$mobility_labels, TRAIT_VOCAB$labels$MB)
  expect_identical(HARMONIZATION_CONFIG$environmental_labels, TRAIT_VOCAB$labels$EP)
  expect_identical(HARMONIZATION_CONFIG$size_labels, TRAIT_VOCAB$labels$MS)
  expect_identical(HARMONIZATION_CONFIG$mobility_patterns, TRAIT_VOCAB$patterns$mobility)
  v <- validate_harmonization_config(HARMONIZATION_CONFIG)
  expect_true(v$ok, info = paste(v$errors, collapse = "; "))
})

test_that("pattern precedence lists exactly the pattern codes", {
  for (trait in c("mobility", "environmental", "protection")) {
    codes <- sub("_.*$", "", names(TRAIT_VOCAB$patterns[[trait]]))
    expect_setequal(TRAIT_VOCAB$pattern_precedence[[trait]], codes)
  }
  expect_setequal(TRAIT_VOCAB$pattern_precedence$foraging,
                  sub("_.*$", "", names(HARMONIZATION_CONFIG$foraging_patterns)))
})

test_that("environmental text: leading boundary and precedence (F35, F77, F36)", {
  expect_identical(classify_by_patterns("benthopelagic", "environmental"), "EP2")
  for (zone in c("subtidal", "sublittoral", "intertidal", "littoral", "eulittoral")) {
    expect_true(is.na(classify_by_patterns(zone, "environmental")), info = zone)
  }
  expect_identical(classify_by_patterns("benthic surface", "environmental"), "EP3")
  expect_true(is.na(classify_by_patterns("subsurface deposit", "environmental")))
  expect_identical(classify_by_patterns("burrowing", "environmental"), "EP4")
  expect_identical(classify_by_patterns("benthic", "environmental"), "EP3")
  expect_identical(classify_by_patterns("mesopelagic", "environmental"), "EP1")
})

test_that("BIOTIC living-habit labels map to EP4 / EP3", {
  habits <- c("Burrow dwelling" = "EP4", "Attached" = "EP3", "Tube dwelling" = "EP3", "Free living" = "EP3",
              burrower = "EP4", attached = "EP3", tube_dweller = "EP3", free_living = "EP3",
              crevice_dweller = "EP3", epizoic = "EP3")
  for (h in names(habits)) {
    expect_identical(classify_by_patterns(h, "environmental"), habits[[h]], info = h)
  }
})

test_that("protection text: soft shell, soft, exoskeleton, heavy exoskeleton", {
  expect_identical(classify_by_patterns("soft shell", "protection"), "PR5")
  expect_identical(classify_by_patterns("soft", "protection"), "PR0")
  expect_identical(classify_by_patterns("exoskeleton", "protection"), "PR4")
  expect_identical(classify_by_patterns("heavy exoskeleton", "protection"), "PR8")
  expect_identical(classify_by_patterns("chitinous", "protection"), "PR4")
  expect_identical(classify_by_patterns("calcareous_tube", "protection"), "PR2")
  expect_identical(classify_by_patterns("calcium_exoskeleton", "protection"), "PR8")
  expect_identical(classify_by_patterns("mucus", "protection"), "PR1")
})

test_that("mobility text: stems keep matching, specific swimmers beat generic ones", {
  expect_identical(classify_by_patterns("burrowing", "mobility"), "MB3")
  expect_identical(classify_by_patterns("planktonic", "mobility"), "MB2")
  expect_identical(classify_by_patterns("floater", "mobility"), "MB2")
  expect_identical(classify_by_patterns("limited_swimmer", "mobility"), "MB4")
  expect_identical(classify_by_patterns("swimmer", "mobility"), "MB5")
  expect_identical(classify_by_patterns("Sedentary, temporary attachment", "mobility"), "MB1")
})

test_that("NA, NULL and empty text give NA without a warning", {
  expect_no_warning(expect_true(is.na(classify_by_patterns(NULL, "mobility"))))
  expect_no_warning(expect_true(is.na(classify_by_patterns(NA, "environmental"))))
  expect_no_warning(expect_true(is.na(classify_by_patterns(c("", "  "), "protection"))))
})

test_that("an invalid session pattern warns and is skipped", {
  cfg <- HARMONIZATION_CONFIG
  cfg$environmental_patterns$EP4_endobenthic <- "(("
  with_session_config(cfg, {
    expect_warning(res <- classify_by_patterns("burrowing", "environmental"),
                   "\\[harmonization\\] invalid environmental pattern for EP4")
    expect_true(is.na(res))
  })
})

test_that("a pre-C-5 session config cannot drop codes or restore old meanings", {
  old <- HARMONIZATION_CONFIG
  old$mobility_labels <- NULL
  old$environmental_labels <- NULL
  old$size_labels <- NULL
  # The v1 mobility section, as a JSON exported before C-5 carries it.
  old$mobility_patterns <- list(
    MB1_sessile = "sessile|attached|fixed|cemented|anchored|immobile",
    MB2_burrower = "burrow|infauna|endobenthic|burrowing|sediment dweller",
    MB3_crawler = "crawl|creep|benthic|epibenthic|slow moving|sluggish|limited.?movement",
    MB4_swimmer_limited = "slow swim|limited swim|weak swim|drift|plankton|limited.?swim|float",
    MB5_swimmer = "swim|pelagic|nektonic|fast|active|mobile|free-swimming"
  )
  old$environmental_patterns$EP3_epibenthic <- "epibenthic|epifauna|surface dwelling|on substrate|^surface$"
  with_session_config(old, {
    expect_identical(trait_codes("MB"), paste0("MB", 1:5))
    expect_identical(classify_by_patterns("burrowing", "mobility"), "MB3")      # not the old MB2
    expect_identical(classify_by_patterns("planktonic drifter", "mobility"), "MB2")  # not the old MB4
    expect_identical(classify_by_patterns("attached", "environmental"), "EP3") # v2 default, not v1
  })
})

test_that("a genuinely customised pattern is honoured", {
  cfg <- HARMONIZATION_CONFIG
  cfg$environmental_patterns$EP3_epibenthic <- "reef"
  with_session_config(cfg, {
    expect_identical(classify_by_patterns("coral reef", "environmental"), "EP3")
    expect_true(is.na(classify_by_patterns("attached", "environmental")))
  })
})

test_that("the vocabulary cannot be pinned by a JSON config", {
  v <- validate_harmonization_config(list(trait_vocab_version = 1L, pattern_precedence = list()))
  expect_null(v$config$trait_vocab_version)
  expect_null(v$config$pattern_precedence)
})

test_that("harm_config_hash changes when the trait vocabulary changes (F72 contribution)", {
  old <- TRAIT_VOCAB
  withr::defer(assign("TRAIT_VOCAB", old, envir = globalenv()))
  before <- harm_config_hash(HARMONIZATION_CONFIG)
  bumped <- old
  bumped$trait_vocab_version <- old$trait_vocab_version + 1L
  assign("TRAIT_VOCAB", bumped, envir = globalenv())
  expect_false(identical(harm_config_hash(HARMONIZATION_CONFIG), before))
})

test_that("the fuzzy ontology harmonizers use the same vocabulary", {
  ont <- function(category, name, modality) {
    data.frame(trait_category = category, trait_name = name, trait_modality = modality,
               trait_score = 3, ontology_id = NA, source = "test", notes = NA, stringsAsFactors = FALSE)
  }
  expect_identical(harmonize_fuzzy_habitat(ont("habitat", "zone", "benthopelagic"))$class, "EP2")
  expect_true(is.na(harmonize_fuzzy_habitat(ont("habitat", "zone", "intertidal"))$class))
  expect_true(is.na(harmonize_fuzzy_habitat(ont("habitat", "zone", "subtidal"))$class))
  expect_identical(harmonize_fuzzy_mobility(ont("life_history", "mobility", "floater"))$class, "MB2")
  expect_identical(harmonize_fuzzy_mobility(ont("life_history", "mobility", "burrower"))$class, "MB3")
})
```

- [ ] **Step 2: Run it to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"`

Expected: FAIL. 13 of the 14 tests fail or error (`could not find function "trait_codes"` / `"classify_by_patterns"`, `object 'TRAIT_VOCAB' not found`). Only "the vocabulary cannot be pinned by a JSON config" passes (it pins existing validator behaviour).

- [ ] **Step 3: Add `TRAIT_VOCAB` and its accessors**

In `R/config/harmonization_config.R`, replace

```r
# Default Harmonization Configuration
HARMONIZATION_CONFIG <- list(
```

with

```r
# =============================================================================
# TRAIT VOCABULARY - the single source of truth (NOT user-overridable)
# =============================================================================
# The food-web model (MB_MB / EP_MS / PR_MS in R/functions/trait_foodweb.R)
# prices these codes. Before vocab v2 (deep analysis 2026-09, F17) one MB code
# meant different things in six places: the live harmonizer, the fuzzy
# ontology harmonizer, the offline-DB writer, the local BVOL/SpeciesEnriched
# mappers and the UI legend each had their own table. Everything that assigns,
# labels or validates an MS/FS/MB/EP/PR code now reads this constant through
# get_trait_vocab() / trait_codes(). A session config or saved JSON may tune
# the default *_patterns (merged in trait_patterns(), harmonization.R), never
# the codes, labels, precedence or taxon rules.
#
# Bump trait_vocab_version whenever a code changes meaning. Offline DBs
# (metadata.trait_vocab_version) and cache envelopes stamped with another
# version are then ignored until rebuilt / refreshed, so stale codes are never
# priced by the new matrices.
TRAIT_VOCAB <- local({
  lab <- function(label, description, examples) {
    list(label = label, description = description, examples = examples)
  }
  list(
    trait_vocab_version = 2L,

    labels = list(
      MS = list(
        MS1 = lab("XS (Extra Small)", "< 0.1 cm", "Bacteria, picoplankton, small diatoms"),
        MS2 = lab("S (Small)", "0.1 - 1 cm", "Copepods, copepod nauplii, large diatoms"),
        MS3 = lab("SM (Small-Medium)", "1 - 5 cm", "Amphipods, krill, larval fish"),
        MS4 = lab("M (Medium)", "5 - 20 cm", "Shrimp, gobies, small crabs"),
        MS5 = lab("ML (Medium-Large)", "20 - 50 cm", "Herring, mackerel, plaice"),
        MS6 = lab("L (Large)", "50 - 150 cm", "Cod, tuna, large fish"),
        MS7 = lab("XL (Extra Large - rarely prey)", "> 150 cm", "Sharks, marine mammals")
      ),
      FS = list(
        FS0 = lab("Primary producer / none", "Photosynthesis or chemosynthesis; never a consumer",
                  "Phytoplankton, diatoms, macroalgae"),
        FS1 = lab("Predator / Carnivore", "Active pursuit of live prey",
                  "Active hunters: piscivorous fish, cephalopods"),
        FS2 = lab("Scavenger / Detritivore", "Dead or moribund organisms, detritus",
                  "Carrion feeders, detritus feeders"),
        FS3 = lab("Omnivore", "Mixed diet (plants and animals)", "Mixed-diet generalists"),
        FS4 = lab("Grazer / Herbivore", "Algae or plant consumption",
                  "Algae scrapers, browsers, herbivorous fish"),
        FS5 = lab("Deposit feeder", "Sediment organic matter", "Sediment-ingesting infauna, lugworms"),
        FS6 = lab("Filter / Suspension feeder", "Suspended particles",
                  "Bivalves, planktivorous fish, sponges"),
        FS7 = lab("Xylophagous", "Wood boring", "Wood-borers: shipworms (Teredinidae), gribbles")
      ),
      MB = list(
        MB1 = lab("Sessile", "Permanently attached, no locomotion",
                  "Barnacles, mussels, sponges, sea anemones, hydroid colonies"),
        MB2 = lab("Passive floater / drifter", "Moves with the water (plankton, medusae)",
                  "Phytoplankton, jellyfish, salps, ctenophores"),
        MB3 = lab("Crawler-burrower", "Benthic locomotion, including infaunal burrowers",
                  "Crabs, sea stars, snails, lugworms, burrowing clams"),
        MB4 = lab("Facultative / limited swimmer", "Swims occasionally, rests on or near the bottom",
                  "Flatfish, shrimp, mysids"),
        MB5 = lab("Obligate swimmer", "Continuous active swimming", "Most fish, squid, marine mammals")
      ),
      EP = list(
        EP1 = lab("Pelagic", "Water column, no substrate contact", "Plankton, herring, jellyfish"),
        EP2 = lab("Benthopelagic", "Near the bottom, swims in the water column", "Cod, whiting, demersal fish"),
        EP3 = lab("Epibenthic", "On the sediment or rock surface (incl. attached and tube-dwelling taxa)",
                  "Sea stars, crabs, mussels, tube worms"),
        EP4 = lab("Endobenthic / Infaunal", "Within the sediment (burrowing)",
                  "Burrowing bivalves, lugworms, burrowing shrimp")
      ),
      PR = list(
        PR0 = lab("None / Soft body", "No protective structure", "Jellyfish, naked sea slugs, most fish"),
        PR1 = lab("Mucus / Cuticle", "Mucus coat, cuticle or leathery body wall",
                  "Hagfish, sea cucumbers, some larvae"),
        PR2 = lab("Tube", "Protective tube or case", "Tube worms (Polychaeta), serpulids"),
        PR3 = lab("Burrow refuge", "Permanent burrow used as a refuge", "Burrowing shrimp, permanent-burrow dwellers"),
        PR4 = lab("Thin exoskeleton", "Thin chitinous exoskeleton", "Copepods, amphipods, small arthropods"),
        PR5 = lab("Soft shell", "Thin calcium carbonate shell", "Moulting crabs, juvenile bivalves"),
        PR6 = lab("Hard shell", "Thick calcium carbonate shell or test", "Mussels, snails, barnacles"),
        PR7 = lab("Spines / ossicle plates", "Spines, spicules or calcareous ossicle plates",
                  "Sea urchins, sea stars, brittle stars, sponges"),
        PR8 = lab("Armoured", "Heavy carapace or armour", "Crabs, lobsters, sturgeon")
      )
    ),

    # Default text patterns (case-insensitive, perl). classify_by_patterns()
    # wraps each pattern as (?<![a-z])(?:<pattern>): every alternative gets a
    # LEADING word boundary only, so stems still match ("burrow" matches
    # "burrowing", "float" matches "floater") while "tidal" no longer matches
    # inside "subtidal" or "pelagic" inside "benthopelagic". Multi-word
    # alternatives use ".?" because the data use underscores
    # ("limited_swimmer", "tube_dweller", "free_living").
    # Zonation words (subtidal, sublittoral, intertidal, littoral) appear in
    # NO environmental pattern on purpose: they describe a depth zone, not the
    # position relative to the substrate, so taxonomy or depth decides.
    patterns = list(
      mobility = list(
        MB1_sessile = "sessile|attach|cemented|fixed|anchored|immobile|colonial hydroid",
        MB2_drifter = "drift|(holo|mero|zoo|phyto|ichthyo)?plankton|float|passive|medusa",
        MB3_crawler_burrower = paste0("crawl|creep|walk|burrow|infaun|endobenth|tube.?dwell|",
                                      "limited.?movement|slow.?moving|sluggish"),
        MB4_facultative_swimmer = "limited.?swim|facultative.?swim|weak.?swim|slow.?swim|swim\\w*.{0,20}occasional",
        MB5_obligate_swimmer = "swim|nekton|active.?swim"
      ),
      environmental = list(
        EP1_pelagic = paste0("(epi|meso|bathy|abysso)?pelagic|water.?column|(holo|mero|zoo|phyto)?plankton|",
                             "nekton|open.?water|midwater|neust"),
        EP2_benthopelagic = "bentho.?pelagic|benthic.pelagic|demersal|near.?bottom|hyperbenth",
        EP3_epibenthic = paste0("epibenth|epifaun|epilith|epiflor|epiphyt|epizo|benthic|benthos|bottom|seabed|",
                                "^surface$|surface.?dwell|on.?substrate|attached|sessile|tube|free.?living|crevice"),
        EP4_endobenthic = "endobenth|infaun|burrow|interstitial|within.?sediment|buried|lithotom"
      ),
      protection = list(
        PR0_none = "^soft$|soft.?bod(y|ied)|naked|none|unprotected|jellyfish|cephalopod|crustose|cushion|stalked",
        PR1_mucus = "mucus|slime|cuticle|cuticular|hagfish|tunic|leathery",
        PR2_tube = "tube|tubicol|parchment",
        PR3_burrow = "burrow",
        PR4_exoskeleton = "exoskeleton|chitin|thin.?carapace|small.?arthropod",
        PR5_soft_shell = "soft.?shell|thin.?shell|weak.?shell|partial.?shell|flexible.?shell|cartilage",
        PR6_hard_shell = "shell|calcif|calcareous|calcium|bivalve|test|barnacle",
        PR7_spines = "spine|spiny|spicule|prickle|thorn|ossicle|urchin",
        PR8_armoured = paste0("armou?r|heavy.?carapace|thick.?carapace|hard.?carapace|crab.?carapace|",
                              "^carapace$|heavy.?exoskeleton|calci\\w*.exoskeleton|lobster")
      )
    ),

    # Codes are tested in this order; the first match wins. Specific before
    # generic where one phrase legitimately holds two terms: "benthic
    # surface" -> EP3 before EP1, "soft shell" -> PR5 before PR6's "shell",
    # "limited_swimmer" -> MB4 before MB5's "swim", "calcareous tube" -> PR2
    # before PR6's "calcareous".
    pattern_precedence = list(
      environmental = c("EP4", "EP2", "EP3", "EP1"),
      protection = c("PR8", "PR7", "PR5", "PR2", "PR6", "PR4", "PR3", "PR1", "PR0"),
      mobility = c("MB1", "MB4", "MB5", "MB3", "MB2"),
      foraging = c("FS0", "FS1", "FS2", "FS3", "FS4", "FS5", "FS6", "FS7")
    ),

    # Ordered taxonomic rules for apply_taxon_rules(). A rule fires when every
    # `match` entry (taxonomy field -> regex, case-insensitive) matches, its
    # optional `text` regex matches the trait text, and at least one of its
    # `flag`s (taxonomic_rules switches in the harmonization settings) is on.
    # `override_text = TRUE` rules win over text patterns (protection only).
    # environmental_pelagic runs BEFORE the depth rule, environmental after it.
    taxon_rules = list(
      mobility = list(
        list(match = list(class = "Actinopteri|Elasmobranchii|Teleostei"), code = "MB5",
             flag = "fish_obligate_swimmers"),
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "MB1", flag = "bivalves_sessile"),
        list(match = list(phylum = "^Mollusca$", class = "^Cephalopoda$"), code = "MB5",
             flag = "cephalopods_swimmers"),
        list(match = list(phylum = "^Mollusca$", class = "^Gastropoda$"), code = "MB3"),
        list(match = list(phylum = "^Arthropoda$", class = "Copepoda"), code = "MB5"),
        list(match = list(phylum = "^Arthropoda$", class = "Malacostraca"), code = "MB4"),
        # Cnidaria by class (F38): medusae drift, polyps are sessile. Other or
        # unknown Cnidaria get no taxon code.
        list(match = list(phylum = "^Cnidaria$", class = "^(Scyphozoa|Cubozoa)$"), code = "MB2"),
        list(match = list(phylum = "^Ctenophora$"), code = "MB2"),
        list(match = list(phylum = "^Cnidaria$", class = "^Hydrozoa$"), text = "medusa|pelagic", code = "MB2"),
        list(match = list(phylum = "^Cnidaria$", class = "^(Anthozoa|Staurozoa|Hydrozoa)$"), code = "MB1"),
        list(match = list(phylum = "^Porifera$"), code = "MB1")
      ),
      environmental_pelagic = list(
        list(match = list(feeding_mode = "photosyn"), code = "EP1", flag = "phytoplankton_pelagic"),
        list(match = list(class = "Bacillariophyceae|Dinophyceae|Prymnesiophyceae"), code = "EP1",
             flag = "phytoplankton_pelagic"),
        list(match = list(class = "Copepoda|Cladocera|Branchiopoda|Appendicularia|Thaliacea"), code = "EP1",
             flag = "zooplankton_pelagic"),
        list(match = list(phylum = "^(Chaetognatha|Ctenophora)$"), code = "EP1", flag = "zooplankton_pelagic"),
        list(match = list(phylum = "^Cnidaria$", class = "^(Scyphozoa|Cubozoa)$"), code = "EP1",
             flag = "zooplankton_pelagic"),
        list(match = list(phylum = "^Cnidaria$", class = "^Hydrozoa$"), text = "medusa|pelagic", code = "EP1",
             flag = "zooplankton_pelagic")
      ),
      environmental = list(
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "EP4", flag = "infaunal_bivalves"),
        # Fish by order: defaulting all fish to EP1 mislabelled every flatfish,
        # goby, eel and sandeel; unknown orders fall back to EP2.
        list(match = list(class = "Actinopteri|Teleostei",
                          order = paste0("Pleuronectiformes|Gobiiformes|Anguilliformes|Scorpaeniformes|",
                                         "Lophiiformes|Ophidiiformes")),
             code = "EP3"),
        list(match = list(class = "Actinopteri|Teleostei",
                          order = "Clupeiformes|Scombriformes|Beloniformes|Carangiformes|Atheriniformes"),
             code = "EP1"),
        list(match = list(class = "Actinopteri|Teleostei",
                          order = "Gadiformes|Perciformes|Aulopiformes|Stomiiformes|Myctophiformes"),
             code = "EP2"),
        list(match = list(class = "Actinopteri|Teleostei"), code = "EP2")
      ),
      protection = list(
        # Echinoderm ossicles beat any text: an urchin described as having
        # "calcareous plates" is PR7, not PR6 (F38).
        list(match = list(phylum = "^Echinodermata$", class = "^(Echinoidea|Asteroidea|Ophiuroidea|Crinoidea)$"),
             code = "PR7", flag = "echinoderms_calcium_plates", override_text = TRUE),
        list(match = list(phylum = "^Echinodermata$", class = "^Holothuroidea$"),
             code = "PR1", flag = "echinoderms_calcium_plates", override_text = TRUE),
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "PR6", flag = "bivalves_hard_shell"),
        list(match = list(phylum = "^Mollusca$", class = "^Gastropoda$"), code = "PR6",
             flag = "gastropods_hard_shell"),
        list(match = list(phylum = "^Mollusca$", class = "^Cephalopoda$"), code = "PR0"),
        list(match = list(phylum = "^Mollusca$"), code = "PR6",
             flag = c("bivalves_hard_shell", "gastropods_hard_shell")),
        list(match = list(phylum = "^Arthropoda$", class = "Malacostraca"), code = "PR8",
             flag = "crustaceans_exoskeleton"),
        # Copepods, cladocerans, ostracods and other small arthropods.
        list(match = list(phylum = "^Arthropoda$"), code = "PR4"),
        list(match = list(phylum = "^Cnidaria$"), code = "PR0"),
        list(match = list(phylum = "^Annelida$", living_habit = "tube"), code = "PR2"),
        list(match = list(phylum = "^Annelida$"), code = "PR0"),
        list(match = list(class = "Actinopteri|Teleostei"), code = "PR0"),
        list(match = list(phylum = "^Porifera$"), code = "PR7")
      )
    ),

    # The pre-v2 default patterns of the keys v2 kept. A server default or
    # imported JSON saved before v2 carries these verbatim (the export writes
    # the whole config); trait_patterns() treats such a value as "not
    # customised" so the v2 default applies instead of silently restoring the
    # v1 behaviour. Historical values: never edit.
    legacy_patterns = list(
      mobility = list(
        MB1_sessile = "sessile|attached|fixed|cemented|anchored|immobile"
      ),
      environmental = list(
        EP1_pelagic = "pelagic|water column|planktonic|nektonic|open water",
        EP2_benthopelagic = "benthopelagic|demersal|near bottom|benthic-pelagic",
        EP3_epibenthic = "epibenthic|epifauna|surface dwelling|on substrate|^surface$",
        EP4_endobenthic = "endobenthic|infauna|burrowing|within sediment|interstitial"
      ),
      protection = list(
        PR0_none = "soft.?bod|naked|unprotected|no shell|no armor|jellyfish|cephalopod|^soft$|crustose|cushion|stalked",
        PR1_mucus = "mucus|slime|cuticle|cuticular|hagfish|tunic",
        PR2_tube = "tube|tube.?dwell|calcareous tube|parchment tube",
        PR3_burrow = "deep burrow|permanent burrow|burrow refuge",
        PR4_exoskeleton = "exoskeleton|chitinous|thin carapace|small arthropod",
        PR5_soft_shell = "soft.?shell|partial shell|flexible shell|cartilage|thin shell",
        PR6_hard_shell = "shell|calcified|calcareous|bivalve shell|gastropod shell|hard carapace|test|barnacle",
        PR7_spines = "spine|spiny|spicule|prickle|thorn|ossicle|urchin",
        PR8_armoured = "armoured|armored|heavy carapace|thick carapace|lobster|crab carapace"
      )
    )
  )
})

#' The trait vocabulary (codes, labels, default patterns, precedence, taxon rules)
#'
#' Never a session value: the vocabulary is what the food-web model prices.
#' @return The TRAIT_VOCAB list.
get_trait_vocab <- function() {
  TRAIT_VOCAB
}

#' Vocabulary version stamped into offline DBs and cache envelopes
#' @return integer(1)
current_trait_vocab_version <- function() {
  get_trait_vocab()$trait_vocab_version
}

#' Valid codes for one trait, in order
#'
#' @param trait One of "MS", "FS", "MB", "EP", "PR".
#' @return Character vector, e.g. c("MB1", ..., "MB5").
trait_codes <- function(trait) {
  labels <- get_trait_vocab()$labels
  if (!is.character(trait) || length(trait) != 1L || !trait %in% names(labels)) {
    stop(sprintf("trait must be one of: %s", paste(names(labels), collapse = ", ")), call. = FALSE)
  }
  names(labels[[trait]])
}

#' Named character vectors of code labels per trait (TRAIT_DEFINITIONS)
#' @return list(MS = c(MS1 = "..."), FS = ..., MB = ..., EP = ..., PR = ...)
trait_definitions <- function() {
  lapply(get_trait_vocab()$labels, function(codes) vapply(codes, function(x) x$label, character(1)))
}

#' Human-readable label of one code, e.g. "MB2" -> "Passive floater / drifter"
#' @param code A trait code.
#' @return character(1); NA for NA or an unknown code.
trait_code_label <- function(code) {
  if (length(code) != 1L || is.na(code)) return(NA_character_)
  trait <- sub("[0-9]+$", "", as.character(code))
  info <- get_trait_vocab()$labels[[trait]][[as.character(code)]]
  if (is.null(info)) NA_character_ else info$label
}

# Default Harmonization Configuration
HARMONIZATION_CONFIG <- list(
```

- [ ] **Step 4: Point the config's label and pattern sections at the vocabulary**

In the same file, replace this block (from the `# Human-readable FS labels.` comment through the end of `protection_labels`):

```r
  # Human-readable FS labels. Single source of truth — same data-driven
  # UI pattern as protection_labels (PR1b) and reproductive/temperature/salinity
  # labels (PR8b Phase B). Pre-P4 the UI hard-coded a table with the wrong
  # ordering (FS1=Herbivore, FS2=Omnivore, FS3=Predator, FS4=Scavenger),
  # mismatching foraging_patterns (FS1=predator, FS2=scavenger, FS3=omnivore,
  # FS4=grazer/herbivore) and silently producing wrong harmonized codes.
  foraging_labels = list(
    FS0 = list(label = "Primary producer",
               examples = "Phytoplankton, diatoms, macroalgae"),
    FS1 = list(label = "Predator / Carnivore",
               examples = "Active hunters: piscivorous fish, cephalopods"),
    FS2 = list(label = "Scavenger / Detritivore",
               examples = "Carrion feeders, detritus feeders"),
    FS3 = list(label = "Omnivore",
               examples = "Mixed-diet generalists"),
    FS4 = list(label = "Grazer / Herbivore",
               examples = "Algae scrapers, browsers, herbivorous fish"),
    FS5 = list(label = "Deposit feeder",
               examples = "Sediment-ingesting infauna, lugworms"),
    FS6 = list(label = "Filter / Suspension feeder",
               examples = "Bivalves, planktivorous fish, sponges"),
    FS7 = list(label = "Xylophagous",
               examples = "Wood-borers: shipworms (Teredinidae), gribbles")
  ),

  # MOBILITY PATTERNS
  mobility_patterns = list(
    MB1_sessile = "sessile|attached|fixed|cemented|anchored|immobile",
    MB2_burrower = "burrow|infauna|endobenthic|burrowing|sediment dweller",
    MB3_crawler = "crawl|creep|benthic|epibenthic|slow moving|sluggish|limited.?movement",
    MB4_swimmer_limited = "slow swim|limited swim|weak swim|drift|plankton|limited.?swim|float",
    MB5_swimmer = "swim|pelagic|nektonic|fast|active|mobile|free-swimming"
  ),

  # ENVIRONMENTAL POSITION PATTERNS
  environmental_patterns = list(
    EP1_pelagic = "pelagic|water column|planktonic|nektonic|open water",
    EP2_benthopelagic = "benthopelagic|demersal|near bottom|benthic-pelagic",
    # Btrait sediment-position label "Surface" -> epibenthic. Anchored ^surface$
    # (the bare label) so it cannot match "Subsurface_deposit", which is a
    # FEEDING label and must reach FS5, not EP3.
    EP3_epibenthic = "epibenthic|epifauna|surface dwelling|on substrate|^surface$",
    EP4_endobenthic = "endobenthic|infauna|burrowing|within sediment|interstitial"
  ),

  # PROTECTION MECHANISM PATTERNS (9-level PR0-PR8, matching harmonize_protection)
  # PR1 (mucus/cuticle) was missing pre-PR1b — UI listed it but config and
  # harmonize_protection had no branch, so any "mucus"-flagged species got
  # NA. Added between PR0_none and PR2_tube to fill the gap.
  protection_patterns = list(
    PR0_none = "soft.?bod|naked|unprotected|no shell|no armor|jellyfish|cephalopod|^soft$|crustose|cushion|stalked",
    # Btrait morphology label "Tunic" (the leathery ascidian covering) -> PR1.
    PR1_mucus = "mucus|slime|cuticle|cuticular|hagfish|tunic",
    PR2_tube = "tube|tube.?dwell|calcareous tube|parchment tube",
    PR3_burrow = "deep burrow|permanent burrow|burrow refuge",
    PR4_exoskeleton = "exoskeleton|chitinous|thin carapace|small arthropod",
    PR5_soft_shell = "soft.?shell|partial shell|flexible shell|cartilage|thin shell",
    PR6_hard_shell = "shell|calcified|calcareous|bivalve shell|gastropod shell|hard carapace|test|barnacle",
    PR7_spines = "spine|spiny|spicule|prickle|thorn|ossicle|urchin",
    PR8_armoured = "armoured|armored|heavy carapace|thick carapace|lobster|crab carapace"
  ),

  # Human-readable PR labels. Single source of truth — the UI renders this
  # via R/ui/trait_research_ui.R, so the on-screen legend can no longer
  # drift from the harmonization rules. Pre-PR1b the UI hard-coded a table
  # that disagreed with config for PR2/3/4/7 (e.g., UI PR2 = "Soft tissue"
  # vs config PR2 = "Tube"; UI PR7 = "Scales" vs config PR7 = "Spines").
  protection_labels = list(
    PR0 = list(label = "None / Soft body",  examples = "Jellyfish, naked sea slugs, cephalopods"),
    PR1 = list(label = "Mucus / Cuticle",   examples = "Hagfish, some larvae"),
    PR2 = list(label = "Tube",              examples = "Tube worms (Polychaeta), serpulids"),
    PR3 = list(label = "Burrow refuge",     examples = "Permanent-burrow dwellers"),
    PR4 = list(label = "Thin exoskeleton",  examples = "Copepods, small arthropods"),
    PR5 = list(label = "Soft shell",        examples = "Molting crabs, juvenile bivalves"),
    PR6 = list(label = "Hard shell",        examples = "Mussels, snails, barnacles"),
    PR7 = list(label = "Spines",            examples = "Sea urchins, spiny fish"),
    PR8 = list(label = "Armoured",          examples = "Lobsters, sturgeon, heavy carapace")
  ),
```

with

```r
  # Code labels: copies of TRAIT_VOCAB$labels, kept here so an exported
  # config documents them. Readers (UI legends, TRAIT_DEFINITIONS) use
  # get_trait_vocab(), never these copies: labels are not user-overridable.
  size_labels = TRAIT_VOCAB$labels$MS,
  foraging_labels = TRAIT_VOCAB$labels$FS,
  mobility_labels = TRAIT_VOCAB$labels$MB,
  environmental_labels = TRAIT_VOCAB$labels$EP,
  protection_labels = TRAIT_VOCAB$labels$PR,

  # MB / EP / PR PATTERNS: the tunable defaults. trait_patterns() merges a
  # session or JSON value over TRAIT_VOCAB$patterns key by key.
  mobility_patterns = TRAIT_VOCAB$patterns$mobility,
  environmental_patterns = TRAIT_VOCAB$patterns$environmental,
  protection_patterns = TRAIT_VOCAB$patterns$protection,
```

(The `# FORAGING STRATEGY PATTERNS` block above it and `# REPRODUCTIVE STRATEGY PATTERNS` below it stay as they are.)

- [ ] **Step 5: Hash the vocabulary into the config hash**

In `R/functions/trait_lookup/harmonization.R`, replace

```r
#' (they do not change any code). The JSON text is hashed rather than the R
#' object, so 150L after a JSON round trip hashes like 150.
#'
#' @param cfg Config list; defaults to this session's config.
#' @return Character(1) xxhash64 digest, or NULL when no config is loaded.
harm_config_hash <- function(cfg = get_harm_config()) {
  if (is.null(cfg)) return(NULL)
  cfg$last_modified <- NULL
  cfg$version <- NULL
```

with

```r
#' (they do not change any code). The JSON text is hashed rather than the R
#' object, so 150L after a JSON round trip hashes like 150.
#'
#' The trait vocabulary (TRAIT_VOCAB: version, default patterns, precedence,
#' taxon rules) is hashed in too. It is not part of any config, so a JSON file
#' cannot pin it, but it changes the codes just as much: bumping
#' trait_vocab_version (or editing a default pattern) turns every envelope
#' written under the old vocabulary into a miss for every reader and for
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

- [ ] **Step 6: Route the fuzzy ontology harmonizers through the vocabulary**

In `harmonize_fuzzy_mobility()`, replace

```r
  # Map ontology modality to MB class
  modality <- tolower(primary$trait_modality)

  mb_class <- NA_character_

  # MB1: Sessile
  if (grepl("sessile|attached|fixed", modality)) {
    mb_class <- "MB1"

  # MB2: Burrower
  } else if (grepl("burrow|infauna|tube.dwell", modality)) {
    mb_class <- "MB2"

  # MB3: Crawler/Floater
  } else if (grepl("crawl|creep|walk|benthic.mobile|floater|drift", modality)) {
    mb_class <- "MB3"

  # MB4: Limited swimmer
  } else if (grepl("limited.swim|facultative.swim|weak.swim", modality)) {
    mb_class <- "MB4"

  # MB5: Swimmer
  } else if (grepl("swimmer|pelagic|nekt", modality)) {
    mb_class <- "MB5"
  }
```

with

```r
  # Map ontology modality to MB class through the shared vocabulary
  # (TRAIT_VOCAB), so the fuzzy path cannot drift from the live cascade.
  mb_class <- classify_by_patterns(primary$trait_modality, "mobility")
```

In `harmonize_fuzzy_habitat()`, replace

```r
  # Map ontology modality to EP class
  modality <- tolower(primary$trait_modality)

  ep_class <- NA_character_

  # EP1: Pelagic
  if (grepl("pelagic|water.column|planktonic", modality)) {
    ep_class <- "EP1"

  # EP2: Benthopelagic
  } else if (grepl("benthopel|demersal|near.bottom", modality)) {
    ep_class <- "EP2"

  # EP3: Epibenthic (on seabed surface)
  } else if (grepl("benthic|subtidal|offshore|deep|epibenthic|epifauna", modality) &&
             !grepl("intertidal|tidal|littoral|infauna|endobenthic", modality)) {
    ep_class <- "EP3"

  # EP4: Endobenthic/Infaunal (within sediment)
  } else if (grepl("intertidal|tidal|littoral|eulittoral|infauna|endobenthic|burrowing", modality)) {
    ep_class <- "EP4"
  }
```

with

```r
  # Map ontology modality to EP class through the shared vocabulary. Zonation
  # modalities (intertidal, subtidal) give NA on purpose: they are depth
  # zones, not a position relative to the substrate (F35, F77).
  ep_class <- classify_by_patterns(primary$trait_modality, "environmental")
```

- [ ] **Step 7: Add the pattern engine**

In the same file, replace

```r
#' Get Pattern from Configuration
#'
#' @param pattern_name String name (e.g., "MB1_sessile", "FS1_predator")
#' @param pattern_type Type: "mobility", "foraging", "environmental", "protection"
#' @return Regular expression pattern string, or NULL if not found
get_config_pattern <- function(pattern_name, pattern_type = "mobility") {
  cfg <- get_harm_config()
  if (is.null(cfg)) return(NULL)

  pattern_list <- switch(pattern_type,
    "mobility" = cfg$mobility_patterns,
    "foraging" = cfg$foraging_patterns,
    "environmental" = cfg$environmental_patterns,
    "protection" = cfg$protection_patterns,
    NULL
  )
  pattern_list[[pattern_name]]
}
```

with

```r
#' Effective text patterns for one trait (vocabulary defaults + session tuning)
#'
#' For mobility / environmental / protection the defaults are
#' TRAIT_VOCAB$patterns; the session config (get_harm_config()) may override
#' them key by key. Only keys the vocabulary defines are honoured, so a stale
#' key from a pre-v2 JSON (e.g. MB2_burrower) can never resurrect the old
#' meaning, and a value identical to the pre-v2 default of a kept key is
#' treated as "not customised". Foraging patterns are entirely config-owned.
#'
#' @param trait "mobility", "environmental", "protection" or "foraging".
#' @return Named list key -> regex (keys like "MB1_sessile"), or NULL.
trait_patterns <- function(trait) {
  section <- paste0(trait, "_patterns")
  cfg <- get_harm_config() %||% list()
  vocab <- get_trait_vocab()
  defaults <- vocab$patterns[[trait]]
  if (is.null(defaults)) return(cfg[[section]])
  user <- cfg[[section]]
  if (!is.list(user) || length(user) == 0L) return(defaults)
  legacy <- vocab$legacy_patterns[[trait]] %||% list()
  keep <- Filter(function(k) {
    v <- user[[k]]
    is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v) && !identical(v, legacy[[k]])
  }, intersect(names(user), names(defaults)))
  utils::modifyList(defaults, user[keep])
}


#' Get Pattern from Configuration
#'
#' Thin wrapper kept for callers of the pre-v2 API.
#'
#' @param pattern_name String name (e.g., "MB1_sessile", "FS1_predator")
#' @param pattern_type Type: "mobility", "foraging", "environmental", "protection"
#' @return Regular expression pattern string, or NULL if not found
get_config_pattern <- function(pattern_name, pattern_type = "mobility") {
  if (!pattern_type %in% c("mobility", "foraging", "environmental", "protection")) return(NULL)
  trait_patterns(pattern_type)[[pattern_name]]
}


#' Classify free text into a trait code using the vocabulary patterns
#'
#' Lower-cases the text and tests the codes in
#' get_trait_vocab()$pattern_precedence[[trait]] order; the first match wins.
#' Each pattern is wrapped as (?<![a-z])(?:<pattern>) (perl): a LEADING
#' boundary only, so stems keep working ("burrow" matches "burrowing") but
#' "tidal" no longer matches inside "subtidal" (F77), "pelagic" inside
#' "benthopelagic" (F35) or "surface" inside "subsurface".
#'
#' @param text Character vector (collapsed with spaces); NULL / NA / "" allowed.
#' @param trait "mobility", "environmental", "protection" or "foraging".
#' @return The code (e.g. "EP2"), or NA_character_ when nothing matches. An
#'   invalid pattern warns ("[harmonization] invalid <trait> pattern for
#'   <code>: ...") and is skipped.
classify_by_patterns <- function(text, trait) {
  if (length(text) == 0L) return(NA_character_)
  text <- as.character(unlist(text))
  text <- text[!is.na(text)]
  if (length(text) == 0L) return(NA_character_)
  txt <- tolower(paste(text, collapse = " "))
  if (!nzchar(trimws(txt))) return(NA_character_)

  pats <- trait_patterns(trait)
  if (length(pats) == 0L) return(NA_character_)
  codes <- sub("_.*$", "", names(pats))
  precedence <- get_trait_vocab()$pattern_precedence[[trait]] %||% unique(codes)
  for (code in precedence) {
    for (key in names(pats)[codes == code]) {
      hit <- tryCatch(
        suppressWarnings(grepl(paste0("(?<![a-z])(?:", pats[[key]], ")"), txt, perl = TRUE)),
        error = function(e) {
          warning(sprintf("[harmonization] invalid %s pattern for %s: %s",
                          trait, code, conditionMessage(e)), call. = FALSE)
          FALSE
        }
      )
      if (isTRUE(hit)) return(code)
    }
  }
  NA_character_
}
```

- [ ] **Step 8: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config/harmonization_config.R'); parse(file='R/functions/trait_lookup/harmonization.R'); parse(file='tests/testthat/test-trait-vocabulary.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); for (f in c('test-harmonization-config-io.R', 'test-trait-cache-config-hash.R', 'test-offline-traits.R', 'test-layer1-quality.R', 'test-trait-lookup-species.R', 'test-harmonization-settings-server.R')) testthat::test_file(file.path('tests/testthat', f))"
```

Expected: `OK`; `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 77 ]` for the new file (verified; package "built under R 4.4.3" warnings outside `test_that()` may appear and are not failures); the six existing files with `FAIL 0` (the harmonize_mobility/EP/PR cascades are still the old code in this task; the JSON round-trip hash test still passes because both sides hash the same vocabulary).

- [ ] **Step 9: Commit**

```bash
git add R/config/harmonization_config.R R/functions/trait_lookup/harmonization.R tests/testthat/test-trait-vocabulary.R
git commit -m "$(cat <<'EOF'
feat(traits): one trait vocabulary (TRAIT_VOCAB) and a boundary-aware pattern engine (C-5)

TRAIT_VOCAB in harmonization_config.R is the single source of truth for the
MS/FS/MB/EP/PR codes (the food-web model's meanings: MB2 = passive floater),
their labels, the default MB/EP/PR text patterns, their precedence and the
taxon rules. classify_by_patterns() adds a leading word boundary, so
"subtidal" no longer reads as tidal -> EP4 (F77) and "benthopelagic" no
longer as pelagic -> EP1 (F35). The fuzzy ontology harmonizers use it.
trait_patterns() ignores stale pre-v2 keys and verbatim v1 defaults from
old JSON files. harm_config_hash() now hashes the vocabulary in (F72).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Taxon rules as data; the live MB/EP/PR cascades (F36, F38; F33 in the harmonizers)

**Files:**
- Modify: `R/functions/trait_lookup/harmonization.R` (insert `apply_taxon_rules()`; replace the bodies of `harmonize_mobility()`, `harmonize_environmental_position()`, `harmonize_protection()`)
- Modify: `R/config/harmonization_config.R` (`validate_harmonization_config()`: retired-rule warning)
- Modify: `R/ui/harmonization_settings_ui.R` (`CONSUMED_TAXONOMIC_RULES`, Mobility Rules checkboxes)
- Modify: `tests/testthat/test-ui-inputs-have-handlers.R` (first test), `tests/testthat/test-harmonization-settings-server.R` (example rule)
- Test: `tests/testthat/test-trait-vocabulary.R` (append)

**Interfaces:**
- Consumes: `TRAIT_VOCAB$taxon_rules`, `classify_by_patterns()` (Task 1), `is_rule_enabled()`.
- Produces: `apply_taxon_rules(taxonomy, trait, text = NULL, override_only = FALSE)`. `harmonize_mobility()`: text -> taxon rules -> "MB4". `harmonize_environmental_position()`: text -> `environmental_pelagic` rules -> depth (<50 m EP3, >200 m EP2) -> `environmental` rules -> "EP3". `harmonize_protection()`: override rules -> text -> taxon rules -> "PR0". `CONSUMED_TAXONOMIC_RULES` = the 10 flags of `TRAIT_VOCAB$taxon_rules`.

- [ ] **Step 1: Append the failing tests**

Append exactly this block to the end of `tests/testthat/test-trait-vocabulary.R`:

```r

# ---------------------------------------------------------------------------
# Task 2 - taxon rules in the live cascades
# ---------------------------------------------------------------------------

test_that("Cnidaria are classified by class (F38)", {
  expect_identical(harmonize_mobility(taxonomic_info = list(phylum = "Cnidaria", class = "Scyphozoa")), "MB2")
  expect_identical(harmonize_mobility(taxonomic_info = list(phylum = "Cnidaria", class = "Anthozoa")), "MB1")
  expect_identical(harmonize_mobility(taxonomic_info = list(phylum = "Cnidaria", class = "Hydrozoa")), "MB1")
  expect_identical(harmonize_mobility(mobility_info = "pelagic",
                                      taxonomic_info = list(phylum = "Cnidaria", class = "Hydrozoa")), "MB2")
  expect_true(is.na(apply_taxon_rules(list(phylum = "Cnidaria", class = "Unknownia"), "mobility")))
})

test_that("echinoderm rules target PR7 / PR1 and outrank text (F38)", {
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Echinodermata", class = "Echinoidea")),
                   "PR7")
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Echinodermata", class = "Holothuroidea")),
                   "PR1")
  expect_identical(harmonize_protection("calcareous plates",
                                        list(phylum = "Echinodermata", class = "Echinoidea")), "PR7")
  expect_identical(harmonize_protection("calcareous plates"), "PR6")
})

test_that("small arthropods get PR4, Malacostraca PR8", {
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Arthropoda", class = "Copepoda")), "PR4")
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Arthropoda", class = "Ostracoda")), "PR4")
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Arthropoda", class = "Malacostraca")),
                   "PR8")
})

test_that("a copepod at 10-20 m with no habitat text is pelagic (F36)", {
  expect_identical(harmonize_environmental_position(depth_min = 10, depth_max = 20,
                                                    taxonomic_info = list(phylum = "Arthropoda",
                                                                          class = "Copepoda")),
                   "EP1")
})

test_that("the environmental default is epibenthic, as its comment says", {
  expect_identical(harmonize_environmental_position(), "EP3")
})

test_that("a zero-length taxonomy field does not abort the harmonizers (F33)", {
  tax <- list(phylum = "Nematoda", class = character(0))
  expect_no_error(harmonize_protection(NULL, tax))
  expect_no_error(harmonize_mobility(NULL, NULL, tax))
  expect_no_error(harmonize_environmental_position(taxonomic_info = tax))
  expect_no_error(harmonize_mobility(NULL, NULL, list(phylum = "Cnidaria", class = character(0))))
})

test_that("a disabled rule flag switches its taxon rule off", {
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$echinoderms_calcium_plates <- FALSE
  with_session_config(cfg, {
    expect_identical(harmonize_protection("calcareous plates",
                                          list(phylum = "Echinodermata", class = "Echinoidea")), "PR6")
  })
})

test_that("cnidarians_sessile is retired: setting it FALSE warns", {
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$cnidarians_sessile <- FALSE
  expect_warning(validate_harmonization_config(cfg), "cnidarians_sessile is retired")
})
```

- [ ] **Step 2: Run to verify the new tests fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"`

Expected: FAIL. 6 of the 8 new tests fail or error (Scyphozoa gives MB1 under the old `cnidarians_sessile` branch and `apply_taxon_rules` does not exist; Echinoidea gives PR5; the copepod gives EP3 from the depth rule; the default is EP2; `class = character(0)` makes an `&&` guard error; no retired-rule warning). "small arthropods get PR4, Malacostraca PR8" and "a disabled rule flag switches its taxon rule off" already pass (regression pins). The 14 Task 1 tests still pass.

- [ ] **Step 3: Add the rule engine**

In `R/functions/trait_lookup/harmonization.R`, replace the end of `classify_by_patterns()`

```r
      if (isTRUE(hit)) return(code)
    }
  }
  NA_character_
}
```

with

```r
      if (isTRUE(hit)) return(code)
    }
  }
  NA_character_
}


#' First taxonomic rule that fires for a taxon
#'
#' Rules live in get_trait_vocab()$taxon_rules[[trait]] (ordered). A rule fires
#' when every `match` field of the taxonomy matches its regex
#' (case-insensitive), its optional `text` regex matches the trait text, and
#' at least one of its `flag`s is enabled (is_rule_enabled()). Taxonomy fields
#' that are NULL, NA or zero-length count as absent, so a WoRMS record with
#' class = character(0) cannot raise "missing value where TRUE/FALSE needed".
#'
#' @param taxonomy List or one-row data frame (phylum, class, order, ...), or NULL.
#' @param trait "mobility", "environmental_pelagic", "environmental" or "protection".
#' @param text Optional trait text (for rules with a `text` condition).
#' @param override_only Only consider rules with override_text = TRUE.
#' @return The rule's code, or NA_character_.
apply_taxon_rules <- function(taxonomy, trait, text = NULL, override_only = FALSE) {
  if (is.null(taxonomy) || !is.list(taxonomy)) return(NA_character_)
  field <- function(name) {
    v <- taxonomy[[name]]
    if (length(v) == 0L) return(NA_character_)
    v <- as.character(unlist(v))[1]
    if (is.na(v) || !nzchar(v)) NA_character_ else v
  }
  text_lower <- tolower(paste(as.character(unlist(text))[!is.na(unlist(text))], collapse = " "))
  for (rule in get_trait_vocab()$taxon_rules[[trait]]) {
    if (override_only && !isTRUE(rule$override_text)) next
    if (!is.null(rule$flag) && !any(vapply(rule$flag, is_rule_enabled, logical(1)))) next
    if (!is.null(rule$text) && !isTRUE(grepl(rule$text, text_lower, perl = TRUE))) next
    matched <- all(vapply(names(rule$match), function(rank) {
      v <- field(rank)
      isTRUE(!is.na(v) && grepl(rule$match[[rank]], v, ignore.case = TRUE))
    }, logical(1)))
    if (matched) return(rule$code)
  }
  NA_character_
}
```

- [ ] **Step 4: Rewrite `harmonize_mobility()`**

Replace the whole function body, i.e. replace

```r
harmonize_mobility <- function(mobility_info = NULL, body_shape = NULL, taxonomic_info = NULL) {

  if (!is.null(mobility_info)) {
    mobility_lower <- tolower(paste(mobility_info, collapse = " "))

    # Get patterns from configuration
    pattern_sessile <- get_config_pattern("MB1_sessile", "mobility")
    pattern_burrower <- get_config_pattern("MB2_burrower", "mobility")
    pattern_crawler <- get_config_pattern("MB3_crawler", "mobility")
    pattern_swimmer_limited <- get_config_pattern("MB4_swimmer_limited", "mobility")
    pattern_swimmer <- get_config_pattern("MB5_swimmer", "mobility")

    if (!is.null(pattern_sessile) && grepl(pattern_sessile, mobility_lower, ignore.case = TRUE)) {
      return("MB1")  # Sessile
    }

    if (!is.null(pattern_burrower) && grepl(pattern_burrower, mobility_lower, ignore.case = TRUE)) {
      return("MB2")  # Burrower
    }

    if (!is.null(pattern_crawler) && grepl(pattern_crawler, mobility_lower, ignore.case = TRUE)) {
      return("MB3")  # Crawler
    }

    if (!is.null(pattern_swimmer_limited) && grepl(pattern_swimmer_limited, mobility_lower, ignore.case = TRUE)) {
      return("MB4")  # Limited swimmer
    }

    if (!is.null(pattern_swimmer) && grepl(pattern_swimmer, mobility_lower, ignore.case = TRUE)) {
      return("MB5")  # Obligate swimmer
    }
  }

  # Use taxonomic inference (configurable rules)
  if (!is.null(taxonomic_info)) {
    phylum <- taxonomic_info$phylum
    class <- taxonomic_info$class

    # Fish are typically obligate swimmers (if rule enabled)
    if (is_rule_enabled("fish_obligate_swimmers")) {
      if (!is.null(class) && grepl("Actinopteri|Elasmobranchii|Teleostei", class)) {
        return("MB5")
      }
    }

    # Molluscs
    if (!is.null(phylum) && phylum == "Mollusca") {
      if (!is.null(class)) {
        # Bivalves sessile (if rule enabled)
        if (class == "Bivalvia" && is_rule_enabled("bivalves_sessile")) {
          return("MB1")
        }
        # Cephalopods swimmers (if rule enabled)
        if (class == "Cephalopoda" && is_rule_enabled("cephalopods_swimmers")) {
          return("MB5")
        }
        if (class == "Gastropoda") return("MB3")  # Snails crawl
      }
    }

    # Arthropods
    if (!is.null(phylum) && phylum == "Arthropoda") {
      if (!is.null(class)) {
        if (grepl("Copepoda", class)) return("MB5")  # Copepods swim
        if (grepl("Malacostraca", class)) return("MB4")  # Crabs/shrimp
      }
    }

    # Cnidarians (jellyfish) - sessile if rule enabled
    if (!is.null(phylum) && phylum == "Cnidaria") {
      if (is_rule_enabled("cnidarians_sessile")) {
        return("MB1")
      }
      return("MB2")  # Passive floaters
    }

    # Porifera (sponges)
    if (!is.null(phylum) && phylum == "Porifera") {
      return("MB1")  # Sessile
    }
  }

  # Default: facultative swimmer
  return("MB4")
}
```

with

```r
harmonize_mobility <- function(mobility_info = NULL, body_shape = NULL, taxonomic_info = NULL) {

  # 1. Explicit text, through the shared vocabulary patterns.
  code <- classify_by_patterns(mobility_info, "mobility")
  if (!is.na(code)) return(code)

  # 2. Taxonomic rules (TRAIT_VOCAB$taxon_rules$mobility, switchable in the
  #    harmonization settings).
  code <- apply_taxon_rules(taxonomic_info, "mobility", text = mobility_info)
  if (!is.na(code)) return(code)

  # Default: facultative swimmer
  return("MB4")
}
```

- [ ] **Step 5: Rewrite `harmonize_environmental_position()`**

Replace

```r
                                            habitat_info = NULL, taxonomic_info = NULL) {

  # Use habitat information if available
  if (!is.null(habitat_info)) {
    habitat_lower <- tolower(paste(habitat_info, collapse = " "))

    if (grepl("pelagic|planktonic|surface|midwater", habitat_lower) &&
        !grepl("benthopelagic|epibenthic", habitat_lower)) {
      return("EP1")  # Pelagic
    }

    if (grepl("benthopelagic|demersal|near.bottom", habitat_lower)) {
      return("EP2")  # Benthopelagic
    }

    if (grepl("epibenthic|epifauna|benthic surface|bottom", habitat_lower)) {
      return("EP3")  # Epibenthic
    }

    if (grepl("infauna|buried|sediment interior|endobenthic", habitat_lower)) {
      return("EP4")  # Endobenthic/Infaunal
    }
  }

  # Use depth range
  if (!is.null(depth_min) && !is.null(depth_max)) {
    avg_depth <- (depth_min + depth_max) / 2

    # Very shallow species are likely epibenthic or infaunal
    if (avg_depth < 50) {
      # Check if burrowing
      if (!is.null(habitat_info) && grepl("burrow", tolower(paste(habitat_info, collapse = " ")))) {
        return("EP4")
      }
      return("EP3")
    }

    # Deep species often benthopelagic
    if (avg_depth > 200) {
      return("EP2")
    }
  }

  # Taxonomic inference (configurable rules)
  if (!is.null(taxonomic_info)) {
    phylum <- taxonomic_info$phylum
    class <- taxonomic_info$class

    # Phytoplankton (if rule enabled)
    if (is_rule_enabled("phytoplankton_pelagic")) {
      if (!is.null(taxonomic_info$feeding_mode) &&
          grepl("photosyn", tolower(taxonomic_info$feeding_mode))) {
        return("EP1")  # Pelagic (need light)
      }
      # Also check for phytoplankton classes
      if (!is.null(class) && grepl("Bacillariophyceae|Dinophyceae|Prymnesiophyceae", class)) {
        return("EP1")
      }
    }

    # Zooplankton (if rule enabled)
    if (is_rule_enabled("zooplankton_pelagic")) {
      if (!is.null(class) && grepl("Copepoda|Cladocera", class)) {
        return("EP1")  # Pelagic
      }
    }

    # Many molluscs are epibenthic or infaunal
    if (!is.null(phylum) && phylum == "Mollusca") {
      if (!is.null(class) && class == "Bivalvia") {
        # Some bivalves are infaunal (if rule enabled)
        if (is_rule_enabled("infaunal_bivalves")) {
          return("EP4")
        }
      }
    }

    # Fish - dispatch by order/family. Defaulting all fish to EP1 (pelagic)
    # mislabels every flatfish, goby, eel, and sandeel; trait-validator
    # critic flagged this as the wrong default. Order-level lookup covers
    # the most common European marine taxa; unknown orders fall back to
    # benthopelagic (EP2), the safer middle ground.
    if (!is.null(class) && grepl("Actinopteri|Teleostei", class)) {
      order <- taxonomic_info$order
      if (!is.null(order) && nzchar(order)) {
        # Predominantly bottom-dwelling
        if (grepl("Pleuronectiformes|Gobiiformes|Anguilliformes|Scorpaeniformes|Lophiiformes|Ophidiiformes",
                  order, ignore.case = TRUE)) {
          return("EP3")
        }
        # Predominantly pelagic
        if (grepl("Clupeiformes|Scombriformes|Beloniformes|Carangiformes|Atheriniformes",
                  order, ignore.case = TRUE)) {
          return("EP1")
        }
        # Demersal / benthopelagic (mobile but bottom-associated)
        if (grepl("Gadiformes|Perciformes|Aulopiformes|Stomiiformes|Myctophiformes",
                  order, ignore.case = TRUE)) {
          return("EP2")
        }
      }
      return("EP2")  # Unknown fish order: benthopelagic, not pelagic
    }
  }

  # Default: epibenthic (conservative)
  return("EP2")
}
```

with

```r
                                            habitat_info = NULL, taxonomic_info = NULL) {

  # 1. Explicit habitat text, through the shared vocabulary patterns.
  code <- classify_by_patterns(habitat_info, "environmental")
  if (!is.na(code)) return(code)

  # 2. Pelagic taxa (phyto- and zooplankton, medusae). Before the depth rule:
  #    a copepod caught at 10-20 m is pelagic, not epibenthic (F36).
  code <- apply_taxon_rules(taxonomic_info, "environmental_pelagic", text = habitat_info)
  if (!is.na(code)) return(code)

  # 3. Depth range
  avg_depth <- suppressWarnings(mean(as.numeric(c(depth_min[1], depth_max[1]))))
  if (length(depth_min) > 0 && length(depth_max) > 0 && isTRUE(is.finite(avg_depth))) {
    # Very shallow species are likely epibenthic (burrowers were caught by
    # the habitat text in step 1)
    if (avg_depth < 50) return("EP3")
    # Deep species often benthopelagic
    if (avg_depth > 200) return("EP2")
  }

  # 4. Other taxonomic rules (infaunal bivalves, fish by order)
  code <- apply_taxon_rules(taxonomic_info, "environmental", text = habitat_info)
  if (!is.na(code)) return(code)

  # Default: epibenthic (conservative)
  return("EP3")
}
```

- [ ] **Step 6: Rewrite `harmonize_protection()`**

Replace

```r
harmonize_protection <- function(skeleton_info = NULL, taxonomic_info = NULL) {

  if (!is.null(skeleton_info)) {
    skeleton_lower <- tolower(paste(skeleton_info, collapse = " "))

    # PR1 must precede PR0's "soft" check so "soft mucus" doesn't fall
    # through to PR0; mucus implies actual protective coating.
    if (grepl("mucus|slime|cuticle|cuticular|hagfish", skeleton_lower)) {
      return("PR1")  # Mucus / cuticle
    }

    if (grepl("none|soft|naked", skeleton_lower)) {
      return("PR0")  # No protection
    }

    if (grepl("tube", skeleton_lower)) {
      return("PR2")  # Tube
    }

    if (grepl("burrow", skeleton_lower)) {
      return("PR3")  # Burrow
    }

    # PR4 — chitinous / thin exoskeleton, must precede PR8's broader
    # "exoskeleton" match. Pre-PR1b this branch was missing entirely.
    if (grepl("chitinous|thin.*exoskeleton|small arthropod|thin.*carapace", skeleton_lower)) {
      return("PR4")  # Thin exoskeleton
    }

    if (grepl("thin.*shell|soft.*shell|weak.*shell", skeleton_lower)) {
      return("PR5")  # Soft shell
    }

    if (grepl("shell|calcareous|calcium", skeleton_lower)) {
      return("PR6")  # Hard shell
    }

    if (grepl("spine|spiny|setae", skeleton_lower)) {
      return("PR7")  # Few spines
    }

    if (grepl("armou?r|exoskeleton|carapace|heavily", skeleton_lower)) {
      return("PR8")  # Armoured (heavy)
    }
  }

  # Taxonomic inference (configurable rules)
  if (!is.null(taxonomic_info)) {
    phylum <- taxonomic_info$phylum
    class <- taxonomic_info$class

    # Molluscs
    if (!is.null(phylum) && phylum == "Mollusca") {
      if (!is.null(class)) {
        # Bivalves hard shell (if rule enabled)
        if (class == "Bivalvia" && is_rule_enabled("bivalves_hard_shell")) {
          return("PR6")
        }
        # Gastropods hard shell (if rule enabled)
        if (class == "Gastropoda" && is_rule_enabled("gastropods_hard_shell")) {
          return("PR6")
        }
        if (class == "Cephalopoda") return("PR0")  # Soft-bodied
      }
      # Default hard shell for molluscs (if any shell rule enabled)
      if (is_rule_enabled("bivalves_hard_shell") || is_rule_enabled("gastropods_hard_shell")) {
        return("PR6")
      }
    }

    # Arthropods
    if (!is.null(phylum) && phylum == "Arthropoda") {
      # Crustaceans exoskeleton (if rule enabled)
      if (is_rule_enabled("crustaceans_exoskeleton")) {
        if (!is.null(class) && grepl("Malacostraca", class)) {
          return("PR8")  # Crabs/lobsters are armoured
        }
        return("PR4")  # Exoskeleton for crustaceans
      }
      return("PR5")  # Soft exoskeleton for small arthropods
    }

    # Echinoderms (if rule enabled)
    if (!is.null(phylum) && phylum == "Echinodermata") {
      if (is_rule_enabled("echinoderms_calcium_plates")) {
        return("PR5")  # Calcium plates
      }
      return("PR7")  # Spiny
    }

    # Cnidarians
    if (!is.null(phylum) && phylum == "Cnidaria") {
      return("PR0")  # Soft-bodied
    }

    # Annelids
    if (!is.null(phylum) && phylum == "Annelida") {
      # Check if tube-dwelling
      if (!is.null(taxonomic_info$living_habit) &&
          grepl("tube", tolower(taxonomic_info$living_habit))) {
        return("PR2")
      }
      return("PR0")  # Soft-bodied
    }

    # Fish
    if (!is.null(class) && grepl("Actinopteri|Teleostei", class)) {
      return("PR0")  # No hard protection
    }

    # Porifera
    if (!is.null(phylum) && phylum == "Porifera") {
      return("PR7")  # Spicules
    }
  }

  # Default: no protection
  return("PR0")
}
```

with

```r
harmonize_protection <- function(skeleton_info = NULL, taxonomic_info = NULL) {

  # 1. Taxon rules that outrank any text (echinoderm ossicles, F38).
  code <- apply_taxon_rules(taxonomic_info, "protection", text = skeleton_info, override_only = TRUE)
  if (!is.na(code)) return(code)

  # 2. Explicit text, through the shared vocabulary patterns.
  code <- classify_by_patterns(skeleton_info, "protection")
  if (!is.na(code)) return(code)

  # 3. Taxonomic rules (TRAIT_VOCAB$taxon_rules$protection).
  code <- apply_taxon_rules(taxonomic_info, "protection", text = skeleton_info)
  if (!is.na(code)) return(code)

  # Default: no protection
  return("PR0")
}
```

- [ ] **Step 7: Retire `cnidarians_sessile` in the validator**

In `R/config/harmonization_config.R`, `validate_harmonization_config()`, replace

```r
    errors <- c(errors, sprintf("taxonomic_rules: must be TRUE or FALSE: %s",
                                paste(names(rules)[!is_flag], collapse = ", ")))
  }
```

with

```r
    errors <- c(errors, sprintf("taxonomic_rules: must be TRUE or FALSE: %s",
                                paste(names(rules)[!is_flag], collapse = ", ")))
  }
  # Retired in trait vocab v2 (F38): Cnidaria are classified by class
  # (medusae MB2, polyps MB1). The key stays so older JSON files still load.
  if (isFALSE(rules$cnidarians_sessile)) {
    warning(paste0("[harmonization] taxonomic_rules$cnidarians_sessile is retired and has no effect: ",
                   "Cnidaria are classified by class (medusae MB2, polyps MB1)"), call. = FALSE)
  }
```

- [ ] **Step 8: Rule checkboxes = the taxon-rule flags**

In `R/ui/harmonization_settings_ui.R`, replace

```r
# The taxonomic rules the harmonize_* code actually reads through
# is_rule_enabled() (R/functions/trait_lookup/harmonization.R). One checkbox
# per rule, inputId harm_rule_<name>; the server wires the same vector.
# tests/testthat/test-ui-inputs-have-handlers.R fails if this list and the
# is_rule_enabled() calls drift apart.
CONSUMED_TAXONOMIC_RULES <- c(
  fish_obligate_swimmers = "Fish -> MB5 (swimmer)",
  cephalopods_swimmers = "Cephalopods -> MB5 (swimmer)",
  bivalves_sessile = "Bivalves -> MB1 (sessile)",
  cnidarians_sessile = "Cnidarians -> MB1 (sessile)",
  phytoplankton_pelagic = "Phytoplankton -> EP1 (pelagic)",
  zooplankton_pelagic = "Copepods / cladocerans -> EP1 (pelagic)",
  infaunal_bivalves = "Bivalves -> EP4 (endobenthic)",
  bivalves_hard_shell = "Bivalves -> PR6 (hard shell)",
  gastropods_hard_shell = "Gastropods -> PR6 (hard shell)",
  crustaceans_exoskeleton = "Crustaceans -> PR8 / PR4 (exoskeleton)",
  echinoderms_calcium_plates = "Echinoderms -> PR5 (calcium plates)"
)
```

with

```r
# The taxonomic rules the harmonize_* code actually reads: the `flag`s of
# TRAIT_VOCAB$taxon_rules (R/config/harmonization_config.R), checked by
# apply_taxon_rules() through is_rule_enabled(). One checkbox per rule,
# inputId harm_rule_<name>; the server wires the same vector.
# tests/testthat/test-ui-inputs-have-handlers.R fails if this list and the
# flags drift apart. cnidarians_sessile is retired (vocab v2 classifies
# Cnidaria by class), so it has no checkbox.
CONSUMED_TAXONOMIC_RULES <- c(
  fish_obligate_swimmers = "Fish -> MB5 (obligate swimmer)",
  cephalopods_swimmers = "Cephalopods -> MB5 (obligate swimmer)",
  bivalves_sessile = "Bivalves -> MB1 (sessile)",
  phytoplankton_pelagic = "Phytoplankton -> EP1 (pelagic)",
  zooplankton_pelagic = "Zooplankton (copepods, cladocerans, salps, medusae) -> EP1 (pelagic)",
  infaunal_bivalves = "Bivalves -> EP4 (endobenthic)",
  bivalves_hard_shell = "Bivalves -> PR6 (hard shell)",
  gastropods_hard_shell = "Gastropods -> PR6 (hard shell)",
  crustaceans_exoskeleton = "Malacostraca -> PR8 (armoured)",
  echinoderms_calcium_plates = "Echinoderms -> PR7 (ossicle plates), sea cucumbers -> PR1"
)
```

and replace

```r
            harm_rule_checkboxes(c("fish_obligate_swimmers", "cephalopods_swimmers",
                                   "bivalves_sessile", "cnidarians_sessile"))
```

with

```r
            harm_rule_checkboxes(c("fish_obligate_swimmers", "cephalopods_swimmers",
                                   "bivalves_sessile"))
```

- [ ] **Step 9: Update the two existing guards that named the old rules**

In `tests/testthat/test-ui-inputs-have-handlers.R`, replace

```r
test_that("the rule checkboxes are exactly the rules the harmonize_* code reads", {
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  files <- list.files(file.path(app_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- code[!startsWith(trimws(code), "#")]
  calls <- regmatches(code, gregexpr('is_rule_enabled\\("[A-Za-z0-9_]+"\\)', code))
  read_rules <- unique(sub('is_rule_enabled\\("(.*)"\\)', "\\1", unlist(calls)))

  expect_gt(length(read_rules), 5L)
  expect_setequal(names(get0("CONSUMED_TAXONOMIC_RULES", ifnotfound = character())), read_rules)
})
```

with

```r
test_that("the rule checkboxes are exactly the rules the harmonize_* code reads", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  files <- list.files(file.path(app_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- code[!startsWith(trimws(code), "#")]
  calls <- regmatches(code, gregexpr('is_rule_enabled\\("[A-Za-z0-9_]+"\\)', code))
  literal_rules <- sub('is_rule_enabled\\("(.*)"\\)', "\\1", unlist(calls))
  # Trait vocab v2: the MB/EP/PR rules are data (TRAIT_VOCAB$taxon_rules)
  # and apply_taxon_rules() checks each rule's `flag` via is_rule_enabled().
  flag_rules <- unlist(lapply(TRAIT_VOCAB$taxon_rules, function(rules) lapply(rules, `[[`, "flag")))
  read_rules <- unique(c(literal_rules, flag_rules))

  expect_gt(length(read_rules), 5L)
  expect_setequal(names(get0("CONSUMED_TAXONOMIC_RULES", ifnotfound = character())), read_rules)
})
```

In `tests/testthat/test-harmonization-settings-server.R`, replace every occurrence of `cnidarians_sessile` with `bivalves_sessile` (Edit tool, `replace_all: true`). It occurs only in the tests "the widgets are seeded from the config at start, not from UI literals" and "the start-up TRUE echo of a rule checkbox does not re-enable a rule the config disabled", which use it as an arbitrary example rule; its checkbox no longer exists and setting it FALSE now warns.

Check: `grep -c "cnidarians_sessile" tests/testthat/test-harmonization-settings-server.R R/ui/harmonization_settings_ui.R R/functions/trait_lookup/harmonization.R` -> `0` for all three.

- [ ] **Step 10: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/functions/trait_lookup/harmonization.R', 'R/config/harmonization_config.R', 'R/ui/harmonization_settings_ui.R', 'tests/testthat/test-ui-inputs-have-handlers.R', 'tests/testthat/test-harmonization-settings-server.R', 'tests/testthat/test-trait-vocabulary.R')) parse(file = f); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); for (f in c('test-ui-inputs-have-handlers.R', 'test-harmonization-settings-server.R', 'test-hotfix.R', 'test-trait-lookup-species.R', 'test-offline-traits.R', 'test-trait-lookup-unit.R', 'test-trait-routing.R', 'test-deep-analysis-fixes.R')) testthat::test_file(file.path('tests/testthat', f))"
```

Expected: `OK`; `PASS 97` for the vocabulary file (77 + 20); every listed file `FAIL 0` (verified at the end state: ui-inputs 49, settings-server 109, hotfix 16, species 96 pass).

- [ ] **Step 11: Commit**

```bash
git add R/functions/trait_lookup/harmonization.R R/config/harmonization_config.R R/ui/harmonization_settings_ui.R tests/testthat/test-ui-inputs-have-handlers.R tests/testthat/test-harmonization-settings-server.R tests/testthat/test-trait-vocabulary.R
git commit -m "$(cat <<'EOF'
fix(traits): taxon rules as data; MB/EP/PR cascades use the vocabulary (C-5)

harmonize_mobility / _environmental_position / _protection now run text
(classify_by_patterns) then the ordered TRAIT_VOCAB taxon rules through
apply_taxon_rules(). Cnidaria follow their class (Scyphozoa MB2, Anthozoa
MB1, F38); echinoderms get PR7 (Holothuroidea PR1) and outrank text;
small arthropods PR4; a copepod at 10-20 m is pelagic because pelagic taxa
now precede the depth rule (F36); the EP default is EP3 as documented.
Zero-length taxonomy fields no longer abort a lookup (F33, harmonizer half).
cnidarians_sessile is retired: no checkbox, and FALSE warns at load.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: The model - PR1, MS1/MS6 columns, MB1 recalibration, strict threshold, vocabulary validation (F70, F71)

**Files:**
- Modify: `R/functions/trait_foodweb.R` (MB_MB MB1 row; EP_MS, PR_MS, TRAIT_DEFINITIONS; `calc_interaction_probability`; `construct_trait_foodweb`; `trait_foodweb_to_igraph`; `validate_trait_data`; `create_trait_template`)
- Test: `tests/testthat/test-trait-vocabulary.R` (append). `tests/testthat/test-edge-contract.R` is NOT changed: with the MB1 recalibration A's orientation tests pass at the default threshold (re-verified).

**Interfaces:**
- Consumes: `trait_codes()`, `trait_definitions()` (Task 1). `trait_foodweb.R` now requires `harmonization_config.R` to be sourced first (app.R: line 43 before line 130; every testthat consumer already does).
- Produces: `MB_MB["MB1", MB2..MB5] == 0.10`; `EP_MS` 4 x 6 and `PR_MS` 9 x 6 (`MS1`..`MS6`); `TRAIT_DEFINITIONS == trait_definitions()`; links need `p > threshold`; `construct_trait_foodweb()` stops with "Invalid trait data: ..." on codes outside the vocabulary.

- [ ] **Step 1: Append the failing tests**

Append exactly this block to the end of `tests/testthat/test-trait-vocabulary.R`:

```r

# ---------------------------------------------------------------------------
# Task 3 - the model matrices (F71, F70)
# ---------------------------------------------------------------------------

local({
  source(file.path(get_app_root(), "R/functions/trait_foodweb.R"), local = FALSE)
})

test_that("TRAIT_DEFINITIONS is derived from the vocabulary labels", {
  for (trait in c("MS", "FS", "MB", "EP", "PR")) {
    expect_identical(names(TRAIT_DEFINITIONS[[trait]]), trait_codes(trait), info = trait)
  }
  expect_identical(TRAIT_DEFINITIONS, trait_definitions())
  expect_identical(TRAIT_DEFINITIONS$MB[["MB2"]], "Passive floater / drifter")
})

test_that("PR1 and PR4 rows validate (F71)", {
  d <- data.frame(species = c("a", "b"), MS = "MS3", FS = "FS1", MB = "MB3", EP = "EP3", PR = c("PR1", "PR4"),
                  stringsAsFactors = FALSE)
  expect_true(validate_trait_data(d)$valid)
})

test_that("PR_MS has a PR1 row equal to PR0, and MS1 / MS6 columns copy their neighbours", {
  expect_identical(rownames(PR_MS), trait_codes("PR"))
  expect_identical(PR_MS["PR1", ], PR_MS["PR0", ])
  for (m in list(EP_MS, PR_MS)) {
    expect_identical(colnames(m), paste0("MS", 1:6))
    expect_identical(m[, "MS1"], m[, "MS2"])
    expect_identical(m[, "MS6"], m[, "MS5"])
  }
})

test_that("an MS3 FS6 consumer eating MS1 prey gets the minimum of the five matrix cells (F70)", {
  consumer <- c(MS = "MS3", FS = "FS6", MB = "MB3", EP = "EP3")
  resource <- c(MS = "MS1", MB = "MB2", EP = "EP1", PR = "PR0")
  expected <- min(MS_MS["MS3", "MS1"], FS_MS["FS6", "MS1"], MB_MB["MB3", "MB2"],
                  EP_MS["EP3", "MS1"], PR_MS["PR0", "MS1"])
  expect_equal(calc_interaction_probability(consumer, resource), expected)
  expect_gt(expected, 0.05)
})

test_that("no hard-coded fallback probability is left in calc_interaction_probability (F70)", {
  src <- paste(deparse(body(calc_interaction_probability)), collapse = "\n")
  expect_false(grepl("0\\.05|0\\.5\\b", src))
})

test_that("a pair exactly at the threshold is not linked (strict >)", {
  d <- data.frame(species = c("pred", "prey"), MS = c("MS4", "MS3"), FS = c("FS1", "FS0"),
                  MB = c("MB5", "MB3"), EP = c("EP2", "EP3"), PR = c("PR0", "PR0"), stringsAsFactors = FALSE)
  p <- construct_trait_foodweb(d, threshold = 0, return_probs = TRUE)["pred", "prey"]
  expect_gt(p, 0)
  expect_equal(construct_trait_foodweb(d, threshold = p)["pred", "prey"], 0)
  expect_equal(construct_trait_foodweb(d, threshold = p - 0.01)["pred", "prey"], 1)
  g <- trait_foodweb_to_igraph(d, threshold = p)
  expect_equal(igraph::ecount(g), 0)
})

test_that("at the default threshold a sessile consumer keeps its mobile prey (MB1 row recalibrated)", {
  expect_identical(unname(MB_MB["MB1", c("MB2", "MB3", "MB4", "MB5")]), rep(0.10, 4))
  expect_identical(MB_MB["MB1", "MB1"], 0.95)
  # A mussel-like filter feeder and drifting phytoplankton ("simple" example pair)
  d <- data.frame(species = c("filter_feeder", "phyto"), MS = c("MS3", "MS1"), FS = c("FS6", "FS0"),
                  MB = c("MB1", "MB2"), EP = c("EP2", "EP4"), PR = c("PR6", "PR0"), stringsAsFactors = FALSE)
  expect_equal(construct_trait_foodweb(d)["filter_feeder", "phyto"], 1)
  expect_equal(construct_trait_foodweb(d, return_probs = TRUE)["filter_feeder", "phyto"], 0.10)
})

test_that("there are no self-loops at any threshold", {
  d <- data.frame(species = c("a", "b", "c"), MS = c("MS4", "MS4", "MS3"), FS = c("FS1", "FS3", "FS6"),
                  MB = c("MB5", "MB4", "MB2"), EP = c("EP2", "EP2", "EP1"), PR = c("PR0", "PR1", "PR4"),
                  stringsAsFactors = FALSE)
  for (thr in c(0, 0.05, 0.5)) {
    expect_true(all(diag(construct_trait_foodweb(d, threshold = thr)) == 0), info = thr)
  }
  expect_true(all(diag(construct_trait_foodweb(d, return_probs = TRUE)) == 0))
})

test_that("an unknown code is an error, not a silent floor value", {
  d <- data.frame(species = c("a", "b"), MS = "MS3", FS = "FS1", MB = "MB3", EP = "EP3", PR = c("PR0", "PR9"),
                  stringsAsFactors = FALSE)
  expect_error(construct_trait_foodweb(d), "Invalid PR codes")
})

test_that("the template only uses vocabulary codes", {
  set.seed(1)
  expect_true(validate_trait_data(create_trait_template(30))$valid)
})
```

- [ ] **Step 2: Run to verify the new tests fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"`

Expected: FAIL. 9 of the 10 new tests fail: TRAIT_DEFINITIONS lacks PR1 and has the old labels; `valid_PR` lacks PR1/PR4; PR_MS has no PR1 row and no MS1/MS6 columns; MS1 prey gets the 0.05 fallback; the body still contains `0.05`/`0.50`; with `>=` the pair at `p` is linked; `MB_MB["MB1", MB2..MB5]` is still 0.05 (the filter feeder's p is 0.05, not 0.10); at threshold 0 `ifelse(prob_matrix >= 0, 1, 0)` puts 1 on the diagonal (the self-loop test fails - finding 17); "PR9" silently gets 0.50. "the template only uses vocabulary codes" already passes (pin).

- [ ] **Step 3: Recalibrate the MB1 row, replace the EP_MS / PR_MS matrices and TRAIT_DEFINITIONS**

In `R/functions/trait_foodweb.R`, replace

```r
#' Mobility × Mobility interaction probabilities
#' Consumer mobility (rows) × Resource mobility (columns)
MB_MB <- matrix(
  c(
    0.95, 0.05, 0.05, 0.05, 0.05,  # MB1 Sessile
```

with

```r
#' Mobility × Mobility interaction probabilities
#' Consumer mobility (rows) × Resource mobility (columns)
#' MB1 (sessile consumer) × MB2-MB5 (mobile prey) is 0.10, a calibration
#' placeholder (user-approved, C-5): at 0.05 it sat on the "implausible"
#' floor, and the strict p > threshold test then cut every sessile suspension
#' feeder off from drifting plankton.
MB_MB <- matrix(
  c(
    0.95, 0.10, 0.10, 0.10, 0.10,  # MB1 Sessile
```

(Rows are the consumer, columns the resource - check the `dimnames` `Consumer_MB` / `Resource_MB` just below; the other four rows stay unchanged.)

Then replace

```r
#' Environmental position × Prey size interaction probabilities
#' Consumer environmental position (rows) × Resource size (columns)
EP_MS <- matrix(
  c(
    0.95, 0.80, 0.50, 0.05,  # EP1 Pelagic (high access to pelagic prey)
    0.80, 0.95, 0.80, 0.50,  # EP2 Benthopelagic
    0.50, 0.80, 0.95, 0.80,  # EP3 Epibenthic
    0.05, 0.50, 0.80, 0.05   # EP4 Endobenthic/Infaunal
  ),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    Consumer_EP = c("EP1", "EP2", "EP3", "EP4"),
    Resource_MS = c("MS2", "MS3", "MS4", "MS5")
  )
)

#' Protection × Prey size interaction probabilities
#' Resource protection (rows) × Resource size (columns)
#' Note: This affects resource vulnerability
PR_MS <- matrix(
  c(
    0.95, 0.95, 0.80, 0.50,  # PR0 None
    0.80, 0.80, 0.50, 0.05,  # PR2 Tube
    0.80, 0.80, 0.50, 0.05,  # PR3 Burrow
    0.50, 0.50, 0.80, 0.50,  # PR4 Exoskeleton (thin carapace)
    0.80, 0.50, 0.50, 0.20,  # PR5 Soft shell
    0.50, 0.50, 0.80, 0.50,  # PR6 Hard shell
    0.20, 0.20, 0.50, 0.20,  # PR7 Few spines
    0.20, 0.20, 0.50, 0.20   # PR8 Armoured
  ),
  nrow = 8, byrow = TRUE,
  dimnames = list(
    Resource_PR = c("PR0", "PR2", "PR3", "PR4", "PR5", "PR6", "PR7", "PR8"),
    Resource_MS = c("MS2", "MS3", "MS4", "MS5")
  )
)

# ============================================================================
# TRAIT DEFINITIONS
# ============================================================================

#' Trait value definitions for reference
TRAIT_DEFINITIONS <- list(
  MS = c(
    MS1 = "XS (Extra Small)",
    MS2 = "S (Small)",
    MS3 = "SM (Small-Medium)",
    MS4 = "M (Medium)",
    MS5 = "ML (Medium-Large)",
    MS6 = "L (Large)",
    MS7 = "XL (Extra Large - rarely prey)"
  ),
  FS = c(
    FS0 = "None",
    FS1 = "Predator",
    FS2 = "Scavenger",
    FS3 = "Omnivore",
    FS4 = "Grazer",
    FS5 = "Deposit feeder",
    FS6 = "Filter feeder",
    FS7 = "Xylophagous (wood borer)"
  ),
  MB = c(
    MB1 = "Sessile",
    MB2 = "Passive floater",
    MB3 = "Crawler-burrower",
    MB4 = "Facultative swimmer",
    MB5 = "Obligate swimmer"
  ),
  EP = c(
    EP1 = "Pelagic",
    EP2 = "Benthopelagic",
    EP3 = "Epibenthic",
    EP4 = "Endobenthic/Infaunal"
  ),
  PR = c(
    PR0 = "None",
    PR2 = "Tube",
    PR3 = "Burrow",
    PR4 = "Exoskeleton",
    PR5 = "Soft shell",
    PR6 = "Hard shell",
    PR7 = "Few spines",
    PR8 = "Armoured"
  )
)
```

with

```r
#' Environmental position × Prey size interaction probabilities
#' Consumer environmental position (rows) × Resource size (columns)
#' MS1 copies MS2 and MS6 copies MS5 (nearest-neighbour placeholders until
#' calibrated, F70); before, both fell to a hard-coded 0.05 floor.
EP_MS <- matrix(
  c(
    0.95, 0.95, 0.80, 0.50, 0.05, 0.05,  # EP1 Pelagic (high access to pelagic prey)
    0.80, 0.80, 0.95, 0.80, 0.50, 0.50,  # EP2 Benthopelagic
    0.50, 0.50, 0.80, 0.95, 0.80, 0.80,  # EP3 Epibenthic
    0.05, 0.05, 0.50, 0.80, 0.05, 0.05   # EP4 Endobenthic/Infaunal
  ),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    Consumer_EP = c("EP1", "EP2", "EP3", "EP4"),
    Resource_MS = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6")
  )
)

#' Protection × Prey size interaction probabilities
#' Resource protection (rows) × Resource size (columns)
#' Note: This affects resource vulnerability. PR1 (mucus / cuticle) copies
#' PR0: it gives negligible size-structured protection against gape-limited
#' predators (F71). MS1 copies MS2 and MS6 copies MS5 (F70 placeholders).
PR_MS <- matrix(
  c(
    0.95, 0.95, 0.95, 0.80, 0.50, 0.50,  # PR0 None
    0.95, 0.95, 0.95, 0.80, 0.50, 0.50,  # PR1 Mucus / cuticle
    0.80, 0.80, 0.80, 0.50, 0.05, 0.05,  # PR2 Tube
    0.80, 0.80, 0.80, 0.50, 0.05, 0.05,  # PR3 Burrow
    0.50, 0.50, 0.50, 0.80, 0.50, 0.50,  # PR4 Exoskeleton (thin carapace)
    0.80, 0.80, 0.50, 0.50, 0.20, 0.20,  # PR5 Soft shell
    0.50, 0.50, 0.50, 0.80, 0.50, 0.50,  # PR6 Hard shell
    0.20, 0.20, 0.20, 0.50, 0.20, 0.20,  # PR7 Spines / ossicle plates
    0.20, 0.20, 0.20, 0.50, 0.20, 0.20   # PR8 Armoured
  ),
  nrow = 9, byrow = TRUE,
  dimnames = list(
    Resource_PR = c("PR0", "PR1", "PR2", "PR3", "PR4", "PR5", "PR6", "PR7", "PR8"),
    Resource_MS = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6")
  )
)

# ============================================================================
# TRAIT DEFINITIONS
# ============================================================================

#' Trait value definitions for reference: derived from the vocabulary labels
#' (TRAIT_VOCAB in R/config/harmonization_config.R, which must be sourced
#' first), so the model, validation, the UI legends and the harmonizers share
#' one set of codes.
TRAIT_DEFINITIONS <- trait_definitions()
```

- [ ] **Step 4: Delete the fallbacks in `calc_interaction_probability()`**

Replace

```r
  # 4. Environmental position × prey size (EP × MS)
  if (c_EP %in% rownames(EP_MS) && r_MS %in% colnames(EP_MS)) {
    probs <- c(probs, EP_MS[c_EP, r_MS])
  } else {
    # EP_MS only covers MS2-MS5, so MS1 and MS6 need special handling
    if (r_MS %in% c("MS1", "MS6")) {
      # Use lower probability for extreme sizes
      probs <- c(probs, 0.05)
    } else {
      return(0)
    }
  }

  # 5. Protection × prey size (PR × MS)
  if (r_PR %in% rownames(PR_MS) && r_MS %in% colnames(PR_MS)) {
    probs <- c(probs, PR_MS[r_PR, r_MS])
  } else {
    # PR_MS only covers MS2-MS5
    if (r_MS %in% c("MS1", "MS6")) {
      # Use default probability
      probs <- c(probs, 0.50)
    } else if (!r_PR %in% rownames(PR_MS)) {
      # PR5 (soft shell) not in matrix - treat as moderate protection
      probs <- c(probs, 0.50)
    } else {
      return(0)
    }
  }
```

with

```r
  # 4. Environmental position × prey size (EP × MS). EP_MS covers MS1-MS6;
  # an unknown code is rejected by validate_trait_data() before construction.
  if (c_EP %in% rownames(EP_MS) && r_MS %in% colnames(EP_MS)) {
    probs <- c(probs, EP_MS[c_EP, r_MS])
  } else {
    return(0)
  }

  # 5. Protection × prey size (PR × MS). PR_MS covers PR0-PR8 × MS1-MS6.
  if (r_PR %in% rownames(PR_MS) && r_MS %in% colnames(PR_MS)) {
    probs <- c(probs, PR_MS[r_PR, r_MS])
  } else {
    return(0)
  }
```

- [ ] **Step 5: Validate before construction; strict threshold**

Replace

```r
#' @param species_data Data frame with columns: species, MS, FS, MB, EP, PR
#' @param threshold Minimum probability threshold for link (default 0.05)
#' @param return_probs If TRUE, return probability matrix instead of binary adjacency
#' @return Adjacency matrix (rows = consumers, columns = resources)
#' @export
construct_trait_foodweb <- function(species_data, threshold = 0.05, return_probs = FALSE) {

  # Validate input
  required_cols <- c("species", "MS", "FS", "MB", "EP", "PR")
  if (!all(required_cols %in% colnames(species_data))) {
    stop(paste("species_data must contain columns:", paste(required_cols, collapse = ", ")))
  }
```

with

```r
#' @param species_data Data frame with columns: species, MS, FS, MB, EP, PR
#' @param threshold A link needs a probability strictly above this (default
#'   0.05, the matrices' "implausible" floor, so floor-valued pairs are not
#'   linked).
#' @param return_probs If TRUE, return probability matrix instead of binary adjacency
#' @return Adjacency matrix (rows = consumers, columns = resources)
#' @export
construct_trait_foodweb <- function(species_data, threshold = 0.05, return_probs = FALSE) {

  # Validate input
  required_cols <- c("species", "MS", "FS", "MB", "EP", "PR")
  if (!all(required_cols %in% colnames(species_data))) {
    stop(paste("species_data must contain columns:", paste(required_cols, collapse = ", ")))
  }
  # An unknown code used to fall to a silent floor value; now it is an error.
  validation <- validate_trait_data(species_data)
  if (!validation$valid) {
    stop(paste("Invalid trait data:", paste(validation$messages, collapse = "; ")), call. = FALSE)
  }
```

Replace

```r
    # Apply threshold
    adjacency <- ifelse(prob_matrix >= threshold, 1, 0)
```

with

```r
    # Apply threshold (strict: a pair AT the threshold is not linked)
    adjacency <- ifelse(prob_matrix > threshold, 1, 0)
```

In `trait_foodweb_to_igraph()`, replace

```r
  edges <- which(prob_matrix >= threshold, arr.ind = TRUE)
```

with

```r
  edges <- which(prob_matrix > threshold, arr.ind = TRUE)
```

- [ ] **Step 6: Validation and the template read the vocabulary**

In `validate_trait_data()`, replace

```r
  # Validate trait codes
  valid_MS <- paste0("MS", 1:7)
  valid_FS <- paste0("FS", c(0:7))
  valid_MB <- paste0("MB", 1:5)
  valid_EP <- paste0("EP", 1:4)
  valid_PR <- paste0("PR", c(0, 2, 3, 5, 6, 7, 8))
```

with

```r
  # Validate trait codes against the vocabulary (PR1 and PR4 were missing
  # from the old literal list, F71)
  valid_MS <- trait_codes("MS")
  valid_FS <- trait_codes("FS")
  valid_MB <- trait_codes("MB")
  valid_EP <- trait_codes("EP")
  valid_PR <- trait_codes("PR")
```

and replace

```r
  if (any(species_data$FS %in% c("FS0", "FS3"), na.rm = TRUE)) {
    messages <- c(messages, "Warning: FS0 (None) and FS3 (Parasite) will not appear as consumers")
  }
```

with

```r
  if (any(species_data$FS == "FS0", na.rm = TRUE)) {
    messages <- c(messages, "Warning: FS0 (Primary producer / none) species will not appear as consumers")
  }
```

In `create_trait_template()`, replace

```r
    MS = sample(paste0("MS", 1:6), n_species, replace = TRUE),
    FS = sample(paste0("FS", c(1, 2, 4, 5, 6)), n_species, replace = TRUE),
    MB = sample(paste0("MB", 1:5), n_species, replace = TRUE),
    EP = sample(paste0("EP", 1:4), n_species, replace = TRUE),
    PR = sample(paste0("PR", c(0, 2, 3, 5, 6, 7, 8)), n_species, replace = TRUE),
```

with

```r
    MS = sample(setdiff(trait_codes("MS"), "MS7"), n_species, replace = TRUE),
    FS = sample(setdiff(trait_codes("FS"), c("FS0", "FS3", "FS7")), n_species, replace = TRUE),
    MB = sample(trait_codes("MB"), n_species, replace = TRUE),
    EP = sample(trait_codes("EP"), n_species, replace = TRUE),
    PR = sample(trait_codes("PR"), n_species, replace = TRUE),
```

- [ ] **Step 7: Parse-check and run the tests (A's regression net at the default threshold)**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/functions/trait_foodweb.R', 'tests/testthat/test-trait-vocabulary.R')) parse(file = f); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); for (f in c('test-edge-contract.R', 'test-comprehensive.R', 'test-layer1-quality.R', 'test-import-attributes.R', 'test-example-datasets.R', 'test-mti-keystoneness.R', 'test-trophic-levels.R')) testthat::test_file(file.path('tests/testthat', f))"
```

Expected: `OK`; `PASS 130` for the vocabulary file (97 + 33); the seven A/regression files `FAIL 0` with `test-edge-contract.R` unchanged (re-verified at the end state: edge-contract 72 in the scratch copy, comprehensive 44, layer1 20, mti 35, trophic 23). At the DEFAULT threshold the "simple" example now has 6 links: Small_fish -> Predatory_fish, Benthic_filter_feeder -> Predatory_fish, Benthic_filter_feeder -> Small_fish, and Small_fish / Zooplankton / Phytoplankton -> Benthic_filter_feeder (p = 0.10 each), so A's N3 and F-4 assertions `assert_prey_to_predator(g, "Phytoplankton", "Benthic_filter_feeder")` and `sum(out["Phytoplankton", ]) > 0` hold. (Without the MB1 recalibration they fail: only the first three links survive.)

- [ ] **Step 8: Commit**

```bash
git add R/functions/trait_foodweb.R tests/testthat/test-trait-vocabulary.R
git commit -m "$(cat <<'EOF'
fix(foodweb): PR1 in the model, MS1/MS6 columns, MB1 recalibration, strict threshold (C-5)

PR_MS gains a PR1 row (copy of PR0) and EP_MS/PR_MS gain MS1/MS6 columns
(copies of MS2/MS5); the hard-coded 0.05/0.50 fallbacks are gone (F70).
MB_MB's sessile-consumer row goes from 0.05 to 0.10 for mobile prey
(user-approved calibration placeholder), so sessile suspension feeders keep
drifting prey under the strict threshold.
validate_trait_data() and create_trait_template() read trait_codes(), so
PR1 and PR4 validate (F71), and construct_trait_foodweb() stops on an
unknown code instead of pricing it at a floor. Links need p > threshold:
a pair at the 0.05 floor is no longer linked, and threshold 0 no longer
links every pair including the diagonal. TRAIT_DEFINITIONS is derived from
the vocabulary labels.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Every other consumer reads the vocabulary; static guard (F17, F18, C1.5)

**Files:**
- Modify: `R/functions/local_trait_databases.R` (`harmonize_bvol_traits` MB; `harmonize_species_enriched_traits` MB/EP/PR)
- Modify: `scripts/initialization/build_offline_trait_db.R` (`mobility_to_mb`, `protection_to_pr`; BIOTIC EP; PTDB MB; SpeciesEnriched EP)
- Modify: `R/ui/trait_research_ui.R` (new `trait_vocab_legend_rows()`; FS/MB/EP/PR tables)
- Modify: `R/functions/trait_help_content.R` (new `trait_help_table_rows()`; FS/MB/EP/PR tables)
- Modify: `R/functions/trait_lookup/orchestrator.R` (MB/EP/PR console labels)
- Test: `tests/testthat/test-trait-vocabulary.R` (append)

**Interfaces:**
- Consumes: `classify_by_patterns()`, `apply_taxon_rules()`, `get_trait_vocab()`, `trait_code_label()`.
- Produces: `trait_vocab_legend_rows(trait, detail = "examples") -> tags$tbody`; `trait_help_table_rows(trait) -> character(1)`. The build script's MB/EP/PR codes match the live harmonizer's for the same text.

- [ ] **Step 1: Append the failing tests**

Append exactly this block to the end of `tests/testthat/test-trait-vocabulary.R`:

```r

# ---------------------------------------------------------------------------
# Task 4 - every other consumer reads the vocabulary
# ---------------------------------------------------------------------------

test_that("BVOL phytoplankton drift (MB2)", {
  h <- harmonize_bvol_traits(list(size_cm = 0.002, trophy = "AU"))
  expect_identical(h$MB, "MB2")
})

test_that("SpeciesEnriched mobility / position / growth form use the vocabulary", {
  h <- harmonize_species_enriched_traits(list(
    size_cm = 3, feeding_method = "filter feeder", mobility = "Crawler or Walker",
    environmental_position = "Epibenthic", body_flexibility = "None (less than 10 degrees)",
    growth_form = "Bivalved"
  ))
  expect_identical(h$MB, "MB3")
  expect_identical(h$EP, "EP3")
  expect_identical(h$PR, "PR6")
  h2 <- harmonize_species_enriched_traits(list(
    size_cm = NA, feeding_method = "", mobility = "Burrower", environmental_position = "Infaunal",
    body_flexibility = "High (greater than 45 degrees)", growth_form = NA
  ))
  expect_identical(h2$MB, "MB3")
  expect_identical(h2$EP, "EP4")
  expect_null(h2[["PR"]])  # [[ ]]: `$PR` would partial-match PR_source
})

test_that("the Trait Research legends are built from the vocabulary", {
  suppressPackageStartupMessages(library(shiny))
  source(file.path(get_app_root(), "R/ui/trait_research_ui.R"), local = FALSE)
  mb <- as.character(trait_vocab_legend_rows("MB"))
  expect_match(mb, "Passive floater / drifter", fixed = TRUE)
  expect_false(grepl("Limited movement", mb, fixed = TRUE))
  ui_src <- readLines(file.path(get_app_root(), "R/ui/trait_research_ui.R"), warn = FALSE)
  expect_false(any(grepl('tags\\$td\\("(MB|EP|PR|FS)[0-9]"\\)', ui_src)))
})

test_that("the help tables are built from the vocabulary and list FS7 and PR1", {
  source(file.path(get_app_root(), "R/functions/trait_help_content.R"), local = FALSE)
  html <- as.character(generate_trait_help_dimensions())
  expect_match(html, "<strong>FS7</strong>", fixed = TRUE)
  expect_match(html, "<strong>PR1</strong>", fixed = TRUE)
  expect_match(html, "Crawler-burrower", fixed = TRUE)
  expect_false(grepl("<td>Limited</td>", html, fixed = TRUE))
})

test_that("the orchestrator's console labels come from the vocabulary", {
  orch <- readLines(file.path(get_app_root(), "R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  expect_false(any(grepl("(mb|ep|pr)_labels <- c\\(", orch)))
  expect_true(any(grepl("trait_code_label(result$MB)", orch, fixed = TRUE)))
})

# C1.5 static guard: no habitat / mobility / protection vocabulary inside a
# grepl("...") literal in the MB/EP/PR classification code. Returning a code
# ("MB2") stays legal; matching text against a private regex does not.
VOCAB_WORDS <- paste0("burrow|pelagic|benth|tidal|littoral|surface|shell|soft|exoskeleton|spine|",
                      "sessile|drift|swim|crawl|tube|attached")

grepl_literals <- function(code) {
  code <- paste(code, collapse = "\n")
  hits <- regmatches(code, gregexpr('grepl\\(\\s*"[^"]*"', code))[[1]]
  sub('"$', "", sub('^grepl\\(\\s*"', "", hits))
}

test_that("no vocabulary regex is left in the MB/EP/PR cascades (C1.5)", {
  fns <- c("harmonize_mobility", "harmonize_environmental_position", "harmonize_protection",
           "harmonize_fuzzy_mobility", "harmonize_fuzzy_habitat", "classify_by_patterns", "apply_taxon_rules",
           "harmonize_bvol_traits", "harmonize_species_enriched_traits")
  for (fn in fns) {
    leaks <- grep(VOCAB_WORDS, grepl_literals(deparse(body(get(fn)))), value = TRUE, ignore.case = TRUE)
    expect_identical(leaks, character(0), info = fn)
  }
  build <- readLines(file.path(get_app_root(), "scripts/initialization/build_offline_trait_db.R"), warn = FALSE)
  build <- build[!startsWith(trimws(build), "#")]
  leaks <- grep(VOCAB_WORDS, grepl_literals(build), value = TRUE, ignore.case = TRUE)
  expect_identical(leaks, character(0), info = "build_offline_trait_db.R")
})
```

- [ ] **Step 2: Run to verify the new tests fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"`

Expected: FAIL. All 6 new tests fail or error: BVOL gives MB4; SpeciesEnriched "Crawler or Walker" gives MB2, "Bivalved" no PR; `trait_vocab_legend_rows` does not exist; the help has no FS7/PR1 row; the orchestrator still defines `mb_labels <- c(`; the guard reports `grepl("tube|attached|free", habit)`-style leaks in `harmonize_species_enriched_traits` and the build script. The 32 earlier tests still pass.

- [ ] **Step 3: Local BVOL / SpeciesEnriched mappers**

In `R/functions/local_trait_databases.R`, replace

```r
  # MB: Mobility (phytoplankton are generally planktonic)
  harmonized$MB <- "MB4"  # Floater/drifter
```

with

```r
  # MB: Mobility. Phytoplankton drift: MB2 (passive floater / drifter) in the
  # model vocabulary. Flagellar motility is far below the size scale of MB4.
  harmonized$MB <- "MB2"
```

and replace

```r
  # MB: Mobility (from mobility field)
  mobility <- tolower(species_traits$mobility)

  if (grepl("crawl|walk", mobility)) {
    harmonized$MB <- "MB2"  # Crawler
    harmonized$MB_confidence <- 0.90
  } else if (grepl("burrow", mobility)) {
    harmonized$MB <- "MB3"  # Burrower
    harmonized$MB_confidence <- 0.90
  } else if (grepl("swim", mobility)) {
    harmonized$MB <- "MB5"  # Swimmer
    harmonized$MB_confidence <- 0.90
  } else if (grepl("sessile|attach", mobility)) {
    harmonized$MB <- "MB1"  # Sessile
    harmonized$MB_confidence <- 0.95
  }
  harmonized$MB_source <- "SpeciesEnriched_mobility"

  # EP: Environmental Position
  env_pos <- tolower(species_traits$environmental_position)

  if (grepl("pelagic|water column", env_pos) && !grepl("benthopelagic", env_pos)) {
    harmonized$EP <- "EP1"  # Pelagic
    harmonized$EP_confidence <- 0.90
  } else if (grepl("benthopelagic|demersal", env_pos)) {
    harmonized$EP <- "EP2"  # Benthopelagic
    harmonized$EP_confidence <- 0.90
  } else if (grepl("epibenthic|epifauna|on surface", env_pos)) {
    harmonized$EP <- "EP3"  # Epibenthic
    harmonized$EP_confidence <- 0.90
  } else if (grepl("infauna|endobenthic|burrowing|benthic|seabed|bottom", env_pos)) {
    harmonized$EP <- "EP4"  # Endobenthic/Infaunal
    harmonized$EP_confidence <- 0.90
  }
  harmonized$EP_source <- "SpeciesEnriched_position"

  # PR: Protection (from body_flexibility or growth_form)
  flexibility <- tolower(species_traits$body_flexibility)
  growth_form <- tolower(species_traits$growth_form)

  if (grepl("shell|test|exoskeleton", growth_form)) {
    harmonized$PR <- "PR3"  # Hard shell/exoskeleton
    harmonized$PR_confidence <- 0.95
  } else if (grepl("soft|flexible", flexibility)) {
    harmonized$PR <- "PR0"  # No protection
    harmonized$PR_confidence <- 0.85
  }
  harmonized$PR_source <- "SpeciesEnriched_morphology"
```

with

```r
  # MB / EP / PR through the shared vocabulary (TRAIT_VOCAB). The old local
  # table had Crawler = MB2 and Burrower = MB3, bare "benthic" = EP4 and
  # shells = PR3 (burrow refuge), none of which the food-web model means.
  mb <- classify_by_patterns(species_traits$mobility, "mobility")
  if (!is.na(mb)) {
    harmonized$MB <- mb
    harmonized$MB_confidence <- 0.90
  }
  harmonized$MB_source <- "SpeciesEnriched_mobility"

  ep <- classify_by_patterns(species_traits$environmental_position, "environmental")
  if (!is.na(ep)) {
    harmonized$EP <- ep
    harmonized$EP_confidence <- 0.90
  }
  harmonized$EP_source <- "SpeciesEnriched_position"

  # PR from growth form. body_flexibility is not protection (its values are
  # bending angles such as "None (less than 10 degrees)"), so it is not used.
  pr <- classify_by_patterns(species_traits$growth_form, "protection")
  if (!is.na(pr)) {
    harmonized$PR <- pr
    harmonized$PR_confidence <- 0.90
  }
  harmonized$PR_source <- "SpeciesEnriched_morphology"
```

- [ ] **Step 4: The offline-DB writer's classification blocks (F18)**

In `scripts/initialization/build_offline_trait_db.R`, replace

```r
mobility_to_mb <- function(mode) {
  if (is.na(mode) || is.null(mode) || nchar(trimws(mode)) == 0) return(NA_character_)
  mode_lower <- tolower(trimws(mode))
  for (mb_name in names(HARMONIZATION_CONFIG$mobility_patterns)) {
    if (grepl(HARMONIZATION_CONFIG$mobility_patterns[[mb_name]], mode_lower))
      return(sub("_.*", "", mb_name))
  }
  warning("Unrecognized mobility mode: '", mode, "'")
  NA_character_
}

protection_to_pr <- function(mode) {
  if (is.na(mode) || is.null(mode) || nchar(trimws(mode)) == 0) return(NA_character_)
  mode_lower <- tolower(trimws(mode))
  for (pr_name in names(HARMONIZATION_CONFIG$protection_patterns)) {
    if (grepl(HARMONIZATION_CONFIG$protection_patterns[[pr_name]], mode_lower))
      return(sub("_.*", "", pr_name))
  }
  warning("Unrecognized protection mode: '", mode, "'")
  NA_character_
}
```

with

```r
# MB / PR through the shared vocabulary (classify_by_patterns(), precedence
# and leading-boundary matching), so the offline DB and the live harmonizer
# give one code for one text.
mobility_to_mb <- function(mode) {
  if (length(mode) == 0 || is.na(mode) || nchar(trimws(mode)) == 0) return(NA_character_)
  code <- classify_by_patterns(mode, "mobility")
  if (is.na(code)) warning("Unrecognized mobility mode: '", mode, "'")
  code
}

protection_to_pr <- function(mode) {
  if (length(mode) == 0 || is.na(mode) || nchar(trimws(mode)) == 0) return(NA_character_)
  code <- classify_by_patterns(mode, "protection")
  if (is.na(code)) warning("Unrecognized protection mode: '", mode, "'")
  code
}
```

Replace (BIOTIC block)

```r
        # EP from Living_habit if available
        ep_val <- NA_character_
        habit_col <- safe_grep_col("living.?habit", names(biotic))
        if (!is.null(habit_col)) {
          habit <- tolower(trimws(biotic[[habit_col]][i] %||% ""))
          if (grepl("burrow", habit)) {
            ep_val <- "EP3"
          } else if (grepl("tube|attached|free", habit)) {
            ep_val <- "EP2"
          }
        }
```

with

```r
        # EP from Living_habit if available. Through the vocabulary: burrower
        # -> EP4; attached / tube_dweller / free_living / crevice_dweller /
        # epizoic -> EP3 (F18: the inline regex sent burrowers to EP3 and
        # the rest to EP2, so the live DB had 0 BIOTIC rows at EP4).
        ep_val <- NA_character_
        habit_col <- safe_grep_col("living.?habit", names(biotic))
        if (!is.null(habit_col)) {
          ep_val <- classify_by_patterns(biotic[[habit_col]][i], "environmental")
        }
```

Replace (PTDB block)

```r
        # MB from motility
        motile_col <- safe_grep_col("motil", names(ptdb))
        mb_val <- NA_character_
        if (!is.null(motile_col)) {
          motility <- tolower(trimws(ptdb[[motile_col]][i] %||% ""))
          mb_val <- if (grepl("motile|yes|true", motility) && !grepl("non", motility)) "MB4"
                    else "MB2"
        } else {
          mb_val <- "MB2"
        }
```

with

```r
        # MB: phytoplankton drift, motile or not: MB2 (passive floater /
        # drifter). Flagellar motility is far below the size scale of the
        # model's MB4 (facultative swimmer), which motile cells used to get.
        mb_val <- "MB2"
```

Replace (SpeciesEnriched block)

```r
        ep_val <- NA_character_
        if (!is.null(ep_col)) {
          ep_raw <- enriched[[ep_col]][i] %||% NA_character_
          if (!is.na(ep_raw) && nchar(trimws(ep_raw)) > 0) {
            ep_lower <- tolower(trimws(ep_raw))
            ep_val <- NA_character_
            for (ep_name in names(HARMONIZATION_CONFIG$environmental_patterns)) {
              if (grepl(HARMONIZATION_CONFIG$environmental_patterns[[ep_name]], ep_lower)) {
                ep_val <- sub("_.*", "", ep_name)
                break
              }
            }
          }
        }
```

with

```r
        ep_val <- NA_character_
        if (!is.null(ep_col)) {
          ep_val <- classify_by_patterns(enriched[[ep_col]][i], "environmental")
        }
```

(The MAREDAT group mapping is left as it is - deviation 12. The ontology block already goes through `harmonize_fuzzy_*()` and `protection_to_pr()`.)

- [ ] **Step 5: Trait Research legends from the vocabulary**

In `R/ui/trait_research_ui.R`, replace

```r
# UI for Trait Research Module
# Split into three tabs: Species List/Lookup Status, Found Traits, Trait Code Reference

trait_research_ui <- function() {
```

with

```r
# UI for Trait Research Module
# Split into three tabs: Species List/Lookup Status, Found Traits, Trait Code Reference

#' Legend rows for one trait, built from the vocabulary labels
#'
#' The MS/FS/MB/EP/PR legends are generated from get_trait_vocab()$labels
#' (R/config/harmonization_config.R), so they can never drift from the codes
#' the harmonizers assign and the food-web model prices (pre-v2 the MB table
#' here said MB2 = "Limited movement", MB4 = "Burrower").
#'
#' @param trait "MS", "FS", "MB", "EP" or "PR".
#' @param detail Label field for the third column: "examples" or "description".
#' @return A tags$tbody.
trait_vocab_legend_rows <- function(trait, detail = "examples") {
  labels <- get_trait_vocab()$labels[[trait]]
  do.call(tags$tbody, lapply(names(labels), function(code) {
    info <- labels[[code]]
    tags$tr(tags$td(code), tags$td(info$label), tags$td(info[[detail]]))
  }))
}

trait_research_ui <- function() {
```

Replace (FS table)

```r
                    # Generated from get_harm_config()$foraging_labels
                    # (R/config/harmonization_config.R + session overrides).
                    # Single source of truth — same data-driven pattern as
                    # protection_labels (PR1b) and RS/TT/ST labels (PR8b
                    # Phase B). Pre-P4 the UI hard-coded a table with the
                    # wrong ordering (FS1=Herbivore vs config FS1=predator).
                    {
                      .fs_labels <- (get_harm_config() %||% HARMONIZATION_CONFIG)$foraging_labels
                      do.call(tags$tbody, lapply(names(.fs_labels), function(code) {
                        info <- .fs_labels[[code]]
                        tags$tr(tags$td(code),
                                tags$td(info$label),
                                tags$td(info$examples))
                      }))
                    }
```

with

```r
                    # Generated from the vocabulary labels (TRAIT_VOCAB). Pre-P4
                    # the UI hard-coded a table with the wrong ordering
                    # (FS1=Herbivore vs config FS1=predator).
                    trait_vocab_legend_rows("FS")
```

Replace (MB table)

```r
                    tags$tbody(
                      tags$tr(tags$td("MB1"), tags$td("Sessile"), tags$td("Barnacles, mussels, sponges")),
                      tags$tr(tags$td("MB2"), tags$td("Limited movement"), tags$td("Sea anemones, some worms")),
                      tags$tr(tags$td("MB3"), tags$td("Crawler"), tags$td("Crabs, sea stars, snails")),
                      tags$tr(tags$td("MB4"), tags$td("Burrower"), tags$td("Lugworms, clams")),
                      tags$tr(tags$td("MB5"), tags$td("Swimmer"), tags$td("Fish, squid, jellyfish"))
                    )
```

with

```r
                    trait_vocab_legend_rows("MB")
```

Replace (EP table)

```r
                    tags$tbody(
                      tags$tr(tags$td("EP1"), tags$td("Pelagic"), tags$td("Open water column")),
                      tags$tr(tags$td("EP2"), tags$td("Benthopelagic"), tags$td("Near-bottom dwelling")),
                      tags$tr(tags$td("EP3"), tags$td("Epibenthic"), tags$td("Lives on sediment surface")),
                      tags$tr(tags$td("EP4"), tags$td("Endobenthic"), tags$td("Lives within sediment"))
                    )
```

with

```r
                    trait_vocab_legend_rows("EP", detail = "description")
```

Replace (PR table)

```r
                    # Generated from get_harm_config()$protection_labels
                    # (R/config/harmonization_config.R + session overrides)
                    # so this table can no longer drift from
                    # harmonize_protection() AND respects per-session config
                    # overrides under PR9α.
                    {
                      .pr_labels <- (get_harm_config() %||% HARMONIZATION_CONFIG)$protection_labels
                      do.call(tags$tbody, lapply(names(.pr_labels), function(code) {
                        info <- .pr_labels[[code]]
                        tags$tr(tags$td(code),
                                tags$td(info$label),
                                tags$td(info$examples))
                      }))
                    }
```

with

```r
                    # Generated from the vocabulary labels (TRAIT_VOCAB), so this
                    # table can no longer drift from harmonize_protection().
                    trait_vocab_legend_rows("PR")
```

(The RS/TT/ST legends below stay on `get_harm_config()`: those vocabularies are not part of C-5.)

- [ ] **Step 6: Help tables from the vocabulary**

In `R/functions/trait_help_content.R`, replace

```r
#' Generate Trait Dimensions HTML content
#' @export
generate_trait_help_dimensions <- function() {
  HTML("
    <div class='well'>
```

with

```r
#' Help-table rows for one trait, from the vocabulary labels
#'
#' Code, label, description and examples come from get_trait_vocab()$labels
#' (R/config/harmonization_config.R), escaped, so the help text cannot drift
#' from the codes the harmonizers assign (pre-v2 it said MB2 = "Limited").
#'
#' @param trait "MS", "FS", "MB", "EP" or "PR".
#' @return One HTML string of <tr> rows.
trait_help_table_rows <- function(trait) {
  labels <- get_trait_vocab()$labels[[trait]]
  esc <- htmltools::htmlEscape
  paste(vapply(names(labels), function(code) {
    info <- labels[[code]]
    sprintf("        <tr><td><strong>%s</strong></td><td>%s</td><td>%s</td><td>%s</td></tr>",
            esc(code), esc(info$label), esc(info$description), esc(info$examples))
  }, character(1)), collapse = "\n")
}


#' Generate Trait Dimensions HTML content
#' @export
generate_trait_help_dimensions <- function() {
  HTML(paste0("
    <div class='well'>
```

Replace

```r
        <tr><th>Code</th><th>Strategy</th><th>Description</th><th>Trophic Level</th></tr>
      </thead>
      <tbody>
        <tr><td><strong>FS0</strong></td><td>Primary Producer</td><td>Photosynthesis/chemosynthesis</td><td>1.0</td></tr>
        <tr><td><strong>FS1</strong></td><td>Predator</td><td>Active pursuit of live prey</td><td>3.0+</td></tr>
        <tr><td><strong>FS2</strong></td><td>Scavenger</td><td>Dead/moribund organisms</td><td>2.5-3.0</td></tr>
        <tr><td><strong>FS3</strong></td><td>Omnivore</td><td>Mixed diet (plants + animals)</td><td>2.5-3.0</td></tr>
        <tr><td><strong>FS4</strong></td><td>Grazer</td><td>Algae/plant consumption</td><td>2.0</td></tr>
        <tr><td><strong>FS5</strong></td><td>Deposit Feeder</td><td>Sediment organic matter</td><td>2.0-2.5</td></tr>
        <tr><td><strong>FS6</strong></td><td>Filter Feeder</td><td>Suspended particles</td><td>2.0-2.5</td></tr>
      </tbody>
```

with

```r
        <tr><th>Code</th><th>Strategy</th><th>Description</th><th>Examples</th></tr>
      </thead>
      <tbody>
", trait_help_table_rows("FS"), "
      </tbody>
```

Replace

```r
      <tbody>
        <tr><td><strong>MB1</strong></td><td>Sessile</td><td>Permanently attached, no movement</td><td>Barnacles, mussels, corals</td></tr>
        <tr><td><strong>MB2</strong></td><td>Limited</td><td>Passive floating, very slow creeping</td><td>Jellyfish, sea anemones</td></tr>
        <tr><td><strong>MB3</strong></td><td>Crawling-Burrowing</td><td>Benthic locomotion</td><td>Crabs, gastropods, worms</td></tr>
        <tr><td><strong>MB4</strong></td><td>Facultative Swimmer</td><td>Swimming + benthic resting</td><td>Flatfish, rays, shrimp</td></tr>
        <tr><td><strong>MB5</strong></td><td>Obligate Swimmer</td><td>Continuous pelagic swimming</td><td>Most fish, squid, mammals</td></tr>
      </tbody>
```

with

```r
      <tbody>
", trait_help_table_rows("MB"), "
      </tbody>
```

Replace

```r
      <tbody>
        <tr><td><strong>EP1</strong></td><td>Pelagic</td><td>Water column, no substrate</td><td>Plankton, herring, jellyfish</td></tr>
        <tr><td><strong>EP2</strong></td><td>Benthopelagic</td><td>Near bottom, some swimming</td><td>Adult cod, flatfish</td></tr>
        <tr><td><strong>EP3</strong></td><td>Epibenthic</td><td>On sediment surface</td><td>Starfish, crabs, bottom fish</td></tr>
        <tr><td><strong>EP4</strong></td><td>Endobenthic</td><td>Buried in sediment</td><td>Burrowing bivalves, polychaetes</td></tr>
      </tbody>
```

with

```r
      <tbody>
", trait_help_table_rows("EP"), "
      </tbody>
```

Replace

```r
      <tbody>
        <tr><td><strong>PR0</strong></td><td>None</td><td>Soft-bodied, no defenses</td><td>Fish, jellyfish, worms</td></tr>
        <tr><td><strong>PR2</strong></td><td>Tube</td><td>Protective tube/case</td><td>Tube-dwelling polychaetes</td></tr>
        <tr><td><strong>PR3</strong></td><td>Burrow</td><td>Sediment refuge</td><td>Burrowing shrimp</td></tr>
        <tr><td><strong>PR5</strong></td><td>Soft Shell</td><td>Thin calcium carbonate</td><td>Small gastropods, young bivalves</td></tr>
        <tr><td><strong>PR6</strong></td><td>Hard Shell</td><td>Thick calcium carbonate</td><td>Adult bivalves, large snails</td></tr>
        <tr><td><strong>PR7</strong></td><td>Few Spines</td><td>Some defensive spines</td><td>Sea urchins, some fish</td></tr>
        <tr><td><strong>PR8</strong></td><td>Armoured</td><td>Heavy exoskeleton + spines</td><td>Crabs, lobsters</td></tr>
      </tbody>
    </table>
    <p><em>Principle:</em> Physical defenses reduce predation vulnerability (but effectiveness decreases with predator size).</p>
  ")
```

with

```r
      <tbody>
", trait_help_table_rows("PR"), "
      </tbody>
    </table>
    <p><em>Principle:</em> Physical defenses reduce predation vulnerability (but effectiveness decreases with predator size).</p>
  "))
```

- [ ] **Step 7: Orchestrator console labels**

In `R/functions/trait_lookup/orchestrator.R`, replace

```r
    mb_labels <- c("MB1"="Sessile", "MB2"="Limited Movement", "MB3"="Floater/Drifter",
                   "MB4"="Crawler/Walker", "MB5"="Swimmer")
    if (!is.na(result$MB) && result$MB %in% names(mb_labels)) {
      message("     (", mb_labels[result$MB], ")")
    }
```

with

```r
    if (!is.na(trait_code_label(result$MB))) message("     (", trait_code_label(result$MB), ")")
```

replace

```r
        mb_labels <- c("MB1"="Sessile", "MB2"="Burrower", "MB3"="Crawler",
                       "MB4"="Limited Swimmer", "MB5"="Swimmer")
        if (result$MB %in% names(mb_labels)) {
          message("     (", mb_labels[result$MB], ")")
        }
```

with

```r
        if (!is.na(trait_code_label(result$MB))) message("     (", trait_code_label(result$MB), ")")
```

replace

```r
    ep_labels <- c(EP1 = "Pelagic", EP2 = "Benthopelagic", EP3 = "Epibenthic", EP4 = "Endobenthic")
    if (!is.na(result$EP) && result$EP %in% names(ep_labels)) {
      message("     (", ep_labels[result$EP], ")")
    }
```

with

```r
    if (!is.na(trait_code_label(result$EP))) message("     (", trait_code_label(result$EP), ")")
```

replace

```r
        ep_labels <- c(EP1 = "Pelagic", EP2 = "Benthopelagic", EP3 = "Epibenthic", EP4 = "Endobenthic")
        if (result$EP %in% names(ep_labels)) {
          message("     (", ep_labels[result$EP], ")")
        }
```

with

```r
        if (!is.na(trait_code_label(result$EP))) message("     (", trait_code_label(result$EP), ")")
```

and replace

```r
    pr_labels <- c("PR0"="Unprotected", "PR2"="Tube", "PR3"="Burrow",
                   "PR4"="Exoskeleton", "PR5"="Soft Shell", "PR6"="Hard Shell",
                   "PR7"="Spines", "PR8"="Armoured")
    if (!is.na(result$PR) && result$PR %in% names(pr_labels)) {
      message("     (", pr_labels[result$PR], ")")
    }
```

with

```r
    if (!is.na(trait_code_label(result$PR))) message("     (", trait_code_label(result$PR), ")")
```

- [ ] **Step 8: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/functions/local_trait_databases.R', 'scripts/initialization/build_offline_trait_db.R', 'R/ui/trait_research_ui.R', 'R/functions/trait_help_content.R', 'R/functions/trait_lookup/orchestrator.R', 'tests/testthat/test-trait-vocabulary.R')) parse(file = f); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); for (f in c('test-layer2b-csv-databases.R', 'test-rebuild-lock.R', 'test-layer4-ui.R', 'test-trait-table-escaping.R', 'test-trait-lookup-unit.R')) testthat::test_file(file.path('tests/testthat', f))"
```

Expected: `OK`; `PASS 156` for the vocabulary file (130 + 26); the five files `FAIL 0` (`test-rebuild-lock.R` runs the build script as a child and must still pass).

- [ ] **Step 9: Commit**

```bash
git add R/functions/local_trait_databases.R scripts/initialization/build_offline_trait_db.R R/ui/trait_research_ui.R R/functions/trait_help_content.R R/functions/trait_lookup/orchestrator.R tests/testthat/test-trait-vocabulary.R
git commit -m "$(cat <<'EOF'
fix(traits): offline-DB writer, local DBs, legends and help read the vocabulary (C-5)

The build script's BIOTIC Living_habit -> EP, mobility and protection go
through classify_by_patterns(): burrowers are EP4 and attached, tube and
free-living taxa EP3 (F18; the live DB had 0 BIOTIC rows at EP4).
Phytoplankton are MB2 in the PTDB/BVOL writers and the local BVOL mapper;
the SpeciesEnriched mapper uses the vocabulary for MB/EP/PR. The Trait
Research legends, the help tables and the orchestrator's console labels
are generated from TRAIT_VOCAB. A static guard fails on any habitat,
mobility or protection word inside a grepl() literal in that code (C1.5).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Never serve codes from another vocabulary - offline-DB gate, cache envelopes, ML (C3.5, C3.7)

**Files:**
- Modify: `R/functions/offline_db_rebuild.R` (append `offline_db_schema_sql()`)
- Modify: `scripts/initialization/build_offline_trait_db.R` (schema from the shared function; `trait_vocab_version` metadata)
- Modify: `R/functions/trait_lookup/orchestrator.R` (gate state + `reset_offline_vocab_gate()`; the gate in `lookup_offline_traits()`; reader; two writers)
- Modify: `R/functions/validation_utils.R` (`read_cache_field(..., vocab_version = NULL)`)
- Modify: `R/modules/trait_research_server.R`, `R/functions/parallel_lookup.R` (pass `vocab_version`)
- Modify: `R/functions/ml_trait_prediction.R` (`predict_missing_traits()` MB gate)
- Modify: `tests/testthat/helper-fixtures.R` (append `make_offline_db_fixture()`), `tests/testthat/test-trait-cache-config-hash.R` (envelope stamp; B1 static strings)
- Test: `tests/testthat/test-trait-vocabulary.R` (append)

**Interfaces:**
- Consumes: `current_trait_vocab_version()` (Task 1); the v2 writer (Task 4); B3's build layout (lock, `offline_traits.db.tmp.<pid>`, `finalize_offline_db_build(con, tmp_path, db_path)`).
- Produces: `offline_db_schema_sql()`, `reset_offline_vocab_gate()`, `read_cache_field(..., vocab_version = NULL)`, `make_offline_db_fixture()`; envelopes carry `trait_vocab_version`; built DBs carry `metadata.trait_vocab_version = '2'`.

- [ ] **Step 1: Add the fixture helper**

Append exactly this block to the end of `tests/testthat/helper-fixtures.R`:

```r

# ---------------------------------------------------------------------------
# Offline trait DB fixture
# ---------------------------------------------------------------------------
# A temp SQLite file with the build script's schema (offline_db_schema_sql()
# in R/functions/offline_db_rebuild.R, the same statements the writer runs)
# plus metadata. `rows` is a data frame of species_traits columns; `species`
# is required. vocab_version = NULL writes no trait_vocab_version row (a
# pre-v2 build). The file is removed when the calling test ends.
make_offline_db_fixture <- function(rows = NULL, vocab_version = 2L, env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  source(file.path(get_app_root(), "R/functions/offline_db_rebuild.R"), local = TRUE)
  path <- tempfile("offline_traits_", fileext = ".db")
  withr::defer(unlink(path), envir = env)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (stmt in offline_db_schema_sql()) DBI::dbExecute(con, stmt)
  DBI::dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('build_timestamp', ?)",
                 params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
  if (!is.null(vocab_version)) {
    DBI::dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('trait_vocab_version', ?)",
                   params = list(as.character(vocab_version)))
  }
  if (!is.null(rows)) DBI::dbAppendTable(con, "species_traits", rows)
  path
}
```

- [ ] **Step 2: Append the failing tests**

Append exactly this block to the end of `tests/testthat/test-trait-vocabulary.R`:

```r

# ---------------------------------------------------------------------------
# Task 5 - stale codes are never served (offline DB, cache, ML)
# ---------------------------------------------------------------------------

offline_row <- function(species = "Testus maximus") {
  data.frame(species = species, MS = "MS3", FS = "FS1", MB = "MB2", EP = "EP1", PR = "PR0",
             primary_source = "ontology", stringsAsFactors = FALSE)
}

test_that("an offline DB from another vocabulary is skipped with one warning (C3.5)", {
  reset_offline_vocab_gate()
  withr::defer(reset_offline_vocab_gate())
  db <- make_offline_db_fixture(offline_row(), vocab_version = 1L)
  expect_warning(res <- lookup_offline_traits("Testus maximus", db_path = db),
                 "DB vocab v1 != config v2; rebuild required, offline DB skipped", fixed = TRUE)
  expect_null(res)
  # Once per process: the next lookup is skipped silently.
  expect_no_warning(expect_null(lookup_offline_traits("Testus maximus", db_path = db)))
})

test_that("an offline DB without a vocab stamp (pre-v2 build) is skipped", {
  reset_offline_vocab_gate()
  withr::defer(reset_offline_vocab_gate())
  db <- make_offline_db_fixture(offline_row(), vocab_version = NULL)
  expect_warning(res <- lookup_offline_traits("Testus maximus", db_path = db), "DB vocab vnone")
  expect_null(res)
})

test_that("an offline DB in the current vocabulary is served", {
  reset_offline_vocab_gate()
  db <- make_offline_db_fixture(offline_row(), vocab_version = current_trait_vocab_version())
  res <- expect_no_warning(lookup_offline_traits("Testus maximus", db_path = db))
  expect_identical(res$MB, "MB2")
})

test_that("read_cache_field treats another or a missing vocab version as stale (C3.7)", {
  f <- tempfile(fileext = ".rds")
  withr::defer(unlink(f))
  env <- list(traits = data.frame(MB = "MB2"), timestamp = Sys.time(), config_hash = "h")
  saveRDS(env, f)
  expect_null(read_cache_field(f, "traits", config_hash = "h", vocab_version = 2L))
  env$trait_vocab_version <- 1L
  saveRDS(env, f)
  expect_null(read_cache_field(f, "traits", config_hash = "h", vocab_version = 2L))
  env$trait_vocab_version <- 2L
  saveRDS(env, f)
  expect_equal(read_cache_field(f, "traits", config_hash = "h", vocab_version = 2L)$MB, "MB2")
  # No vocab asked (e.g. classify_species_api envelopes): unchanged behaviour.
  env$trait_vocab_version <- NULL
  saveRDS(env, f)
  expect_equal(read_cache_field(f, "traits")$MB, "MB2")
})

test_that("both orchestrator cache writers stamp trait_vocab_version", {
  orch <- readLines(file.path(get_app_root(), "R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  orch <- orch[!startsWith(trimws(orch), "#")]
  expect_equal(sum(grepl("trait_vocab_version = current_trait_vocab_version()", orch, fixed = TRUE)), 2L)
})

test_that("an ML model trained before vocab v2 does not predict MB", {
  root <- get_app_root()
  source(file.path(root, "R/functions/ml_trait_prediction.R"), local = FALSE)
  old_load <- load_ml_models
  old_pred <- predict_trait_ml
  withr::defer({
    assign("load_ml_models", old_load, envir = globalenv())
    assign("predict_trait_ml", old_pred, envir = globalenv())
    .ml_cache$vocab_warned <- FALSE
  })
  .ml_cache$vocab_warned <- FALSE
  assign("predict_trait_ml", function(trait, taxonomic_info, models_package) {
    list(value = paste0(trait, "1"), probability = 0.9)
  }, envir = globalenv())

  assign("load_ml_models", function() list(models = list(), trait_vocab_version = NULL), envir = globalenv())
  expect_warning(p <- predict_missing_traits(list(MS = NA, MB = NA, EP = NA), list()), "MB predictions disabled")
  expect_setequal(names(p), c("MS", "EP", "FS", "PR"))

  assign("load_ml_models", function() list(models = list(), trait_vocab_version = 2L), envir = globalenv())
  expect_no_warning(p2 <- predict_missing_traits(list(MB = NA), list()))
  expect_true("MB" %in% names(p2))
})

# The build script, run as a child Rscript in a scratch project root holding
# exactly what it sources (same technique as test-rebuild-lock.R).
local_vocab_build_root <- function(env = parent.frame()) {
  root <- tempfile("vocab_build_")
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  app_root <- get_app_root()
  for (rel in c("R/config/harmonization_config.R",
                "R/functions/validation_utils.R",
                "R/functions/trait_lookup/harmonization.R",
                "R/functions/offline_db_rebuild.R",
                "scripts/initialization/build_offline_trait_db.R")) {
    dir.create(file.path(root, dirname(rel)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(app_root, rel), file.path(root, rel))
  }
  dir.create(file.path(root, "cache"))
  dir.create(file.path(root, "data"))
  writeLines(c(
    "taxon_name,aphia_id,trait_category,trait_name,trait_modality,trait_score",
    "Aurelia aurita,135306,life_history,mobility,floater,3",
    "Aurelia aurita,135306,habitat,zone,pelagic,3"
  ), file.path(root, "data", "ontology_traits.csv"))
  writeLines(c(
    "Species,Max_Length_mm,Longevity_years,Feeding_mode,Living_habit,Mobility,Substratum,Skeleton",
    "Arenicola marina,200,6,deposit_feeder,burrower,burrower,soft_sediment,none",
    "Lanice conchilega,300,2,suspension_feeder,tube_dweller,sessile,soft_sediment,none",
    "Mytilus edulis,100,10,filter_feeder,attached,sessile,hard_substrata,calcium_shell"
  ), file.path(root, "data", "biotic_traits.csv"))
  root
}

test_that("the build writes trait_vocab_version and v2 codes into the DB it installs", {
  skip_if_not_installed("processx")
  skip_if_not_installed("RSQLite")
  root <- local_vocab_build_root()
  res <- processx::run(file.path(R.home("bin"), "Rscript"),
                       args = "scripts/initialization/build_offline_trait_db.R",
                       wd = root, error_on_status = FALSE, timeout = 120,
                       env = c("current", ECONETOOL_REBUILD_LOCK_TOKEN = ""))
  expect_equal(res$status, 0, info = res$stderr)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(root, "cache", "offline_traits.db"))
  withr::defer(DBI::dbDisconnect(con))
  meta <- DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key = 'trait_vocab_version'")$value
  expect_identical(meta, as.character(current_trait_vocab_version()))
  rows <- DBI::dbGetQuery(con, "SELECT species, MB, EP, PR FROM species_traits ORDER BY species")
  expect_identical(rows$EP[rows$species == "Arenicola marina"], "EP4")
  expect_identical(rows$EP[rows$species == "Lanice conchilega"], "EP3")
  expect_identical(rows$EP[rows$species == "Mytilus edulis"], "EP3")
  expect_identical(rows$MB[rows$species == "Arenicola marina"], "MB3")
  expect_identical(rows$MB[rows$species == "Aurelia aurita"], "MB2")
  expect_identical(rows$EP[rows$species == "Aurelia aurita"], "EP1")
})
```

- [ ] **Step 3: Run to verify the new tests fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"`

Expected: FAIL. All 7 new tests fail or error (`could not find function "reset_offline_vocab_gate"` / `"offline_db_schema_sql"`, `unused argument (vocab_version = 2)`, 0 writers stamp the version, no "MB predictions disabled" warning, no `trait_vocab_version` metadata row). The 38 earlier tests still pass.

- [ ] **Step 4: One schema for the writer and the fixture**

In `R/functions/offline_db_rebuild.R`, replace the end of the file

```r
  try(Sys.chmod(db_path, mode = "0664", use_umask = FALSE), silent = TRUE)
  invisible(db_path)
}
```

with

```r
  try(Sys.chmod(db_path, mode = "0664", use_umask = FALSE), silent = TRUE)
  invisible(db_path)
}

#' The offline trait DB schema (CREATE statements)
#'
#' Shared by scripts/initialization/build_offline_trait_db.R (the writer) and
#' the test fixture make_offline_db_fixture() (tests/testthat/helper-fixtures.R)
#' so the two cannot drift. It lives here, not in the build script, because
#' sourcing the script runs a whole build.
#'
#' @return Character vector of SQL statements, run in order with DBI::dbExecute().
offline_db_schema_sql <- function() {
  c(
    "
  CREATE TABLE IF NOT EXISTS species_traits (
    id              INTEGER PRIMARY KEY AUTOINCREMENT,
    species         TEXT UNIQUE NOT NULL,
    aphia_id        INTEGER,
    functional_group TEXT,
    MS              TEXT,
    FS              TEXT,
    MB              TEXT,
    EP              TEXT,
    PR              TEXT,
    -- PR8b: extended modalities. Reproductive Strategy / Temperature
    -- Tolerance / Salinity Tolerance. Default NULL so the source-block
    -- INSERTs that only list the core columns keep working.
    RS              TEXT,
    TT              TEXT,
    ST              TEXT,
    MS_confidence   REAL DEFAULT 0.0,
    FS_confidence   REAL DEFAULT 0.0,
    MB_confidence   REAL DEFAULT 0.0,
    EP_confidence   REAL DEFAULT 0.0,
    PR_confidence   REAL DEFAULT 0.0,
    RS_confidence   REAL DEFAULT 0.0,
    TT_confidence   REAL DEFAULT 0.0,
    ST_confidence   REAL DEFAULT 0.0,
    primary_source  TEXT,
    region          TEXT,
    notes           TEXT
  )
",
    "
  CREATE TABLE IF NOT EXISTS metadata (
    key   TEXT PRIMARY KEY,
    value TEXT
  )
",
    "CREATE INDEX IF NOT EXISTS idx_species ON species_traits(species)",
    "CREATE INDEX IF NOT EXISTS idx_aphia   ON species_traits(aphia_id)"
  )
}
```

(Check: `grep -c "^finalize_offline_db_build <- function" R/functions/offline_db_rebuild.R` is `1` before you edit; the old text above is the last four lines of that function.)

- [ ] **Step 5: The build writes the shared schema and the vocab version (into the tmp DB)**

In `scripts/initialization/build_offline_trait_db.R`, replace

```r
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS species_traits (
    id              INTEGER PRIMARY KEY AUTOINCREMENT,
    species         TEXT UNIQUE NOT NULL,
    aphia_id        INTEGER,
    functional_group TEXT,
    MS              TEXT,
    FS              TEXT,
    MB              TEXT,
    EP              TEXT,
    PR              TEXT,
    -- PR8b: extended modalities. Reproductive Strategy / Temperature
    -- Tolerance / Salinity Tolerance — orchestrator already populates
    -- them from BlackSea/ArcticTraits/Cefas/CoralTraits/WoRMS_Traits/
    -- PolyTraits during a live lookup, but until this PR they had no
    -- column to land in. Default NULL so existing source-block INSERTs
    -- (ontology/biotic/maredat/ptdb/bvol/species_enriched) work unchanged
    -- while a future stakeholder-driven follow-up wires writers per source.
    RS              TEXT,
    TT              TEXT,
    ST              TEXT,
    MS_confidence   REAL DEFAULT 0.0,
    FS_confidence   REAL DEFAULT 0.0,
    MB_confidence   REAL DEFAULT 0.0,
    EP_confidence   REAL DEFAULT 0.0,
    PR_confidence   REAL DEFAULT 0.0,
    RS_confidence   REAL DEFAULT 0.0,
    TT_confidence   REAL DEFAULT 0.0,
    ST_confidence   REAL DEFAULT 0.0,
    primary_source  TEXT,
    region          TEXT,
    notes           TEXT
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS metadata (
    key   TEXT PRIMARY KEY,
    value TEXT
  )
")

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_species ON species_traits(species)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_aphia   ON species_traits(aphia_id)")

# Write metadata
dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('build_timestamp', ?)",
          params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('version', ?)",
          params = list(HARMONIZATION_CONFIG$version %||% "1.0.0"))
```

with

```r
# The schema is shared with the test fixture (offline_db_schema_sql() in
# R/functions/offline_db_rebuild.R), so writer and fixture cannot drift.
for (stmt in offline_db_schema_sql()) dbExecute(con, stmt)

# Write metadata
dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('build_timestamp', ?)",
          params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('version', ?)",
          params = list(HARMONIZATION_CONFIG$version %||% "1.0.0"))
# The vocabulary the codes below were written in. lookup_offline_traits()
# skips a DB whose version differs from the running app's (vocab gate).
dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('trait_vocab_version', ?)",
          params = list(as.character(current_trait_vocab_version())))
```

`con` is the connection to `offline_traits.db.tmp.<pid>` (B3), so the stamp is part of the build that `finalize_offline_db_build(con, tmp_path, db_path)` installs at the end; a failed build installs nothing.

- [ ] **Step 6: The offline vocab gate**

In `R/functions/trait_lookup/orchestrator.R`, replace

```r
#' Quick lookup from offline pre-computed trait database
#'
#' @param species_name Scientific name
#' @param db_path Path to offline SQLite database
#' @return Data frame row with trait codes, or NULL if not found
lookup_offline_traits <- function(species_name, db_path = "cache/offline_traits.db") {
```

with

```r
# Vocab gate state: warn once per process, not once per species.
.offline_vocab_gate <- new.env(parent = emptyenv())
.offline_vocab_gate$warned <- FALSE

#' Reset the once-per-process offline vocab warning (tests)
reset_offline_vocab_gate <- function() {
  .offline_vocab_gate$warned <- FALSE
  invisible(TRUE)
}

#' Quick lookup from offline pre-computed trait database
#'
#' A DB whose metadata.trait_vocab_version is missing or differs from
#' current_trait_vocab_version() holds codes in another vocabulary (e.g. a
#' pre-v2 build: MB2 = burrower) and is skipped - with one warning per
#' process - until it is rebuilt; lookups then fall back to the live APIs.
#'
#' @param species_name Scientific name
#' @param db_path Path to offline SQLite database
#' @return Data frame row with trait codes, or NULL if not found
lookup_offline_traits <- function(species_name, db_path = "cache/offline_traits.db") {
```

and replace

```r
    # Check staleness
    meta <- DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key = 'build_timestamp'")
```

with

```r
    # Vocab gate: never serve codes written in another trait vocabulary.
    vocab_row <- DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key = 'trait_vocab_version'")
    db_vocab <- if (nrow(vocab_row) > 0) vocab_row$value[1] else "none"
    if (!identical(db_vocab, as.character(current_trait_vocab_version()))) {
      if (!isTRUE(.offline_vocab_gate$warned)) {
        .offline_vocab_gate$warned <- TRUE
        warning(sprintf("[offline] DB vocab v%s != config v%s; rebuild required, offline DB skipped",
                        db_vocab, current_trait_vocab_version()), call. = FALSE)
      }
      return(NULL)
    }

    # Check staleness
    meta <- DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key = 'build_timestamp'")
```

(The gate sits inside the existing `tryCatch({ ... }, error = ...)`, after `migrate_offline_schema()`. The warning is not an error, so it propagates normally; `return(NULL)` returns from `lookup_offline_traits()` as the existing `if (nrow(result) == 0) return(NULL)` does.)

- [ ] **Step 7: Envelopes carry the vocab version; the reader checks it**

In `R/functions/trait_lookup/orchestrator.R`, replace

```r
    cached_traits <- read_cache_field(cache_file, "traits", config_hash = harm_config_hash())
```

with

```r
    cached_traits <- read_cache_field(cache_file, "traits", config_hash = harm_config_hash(),
                                      vocab_version = current_trait_vocab_version())
```

replace

```r
        saveRDS(list(traits = result, timestamp = Sys.time(),
                     config_hash = harm_default_config_hash()), cache_file)
```

with

```r
        saveRDS(list(traits = result, timestamp = Sys.time(),
                     config_hash = harm_default_config_hash(),
                     trait_vocab_version = current_trait_vocab_version()), cache_file)
```

and replace

```r
      species = species_name,
      timestamp = Sys.time(),
      config_hash = harm_config_hash()
    )
```

with

```r
      species = species_name,
      timestamp = Sys.time(),
      config_hash = harm_config_hash(),
      trait_vocab_version = current_trait_vocab_version()
    )
```

In `R/functions/validation_utils.R`, replace

```r
#'   session's harmonization config).
#' @return The field's value, or NULL if absent/stale/missing/unreadable/foreign-config.
#' @export
read_cache_field <- function(cache_file, field, max_age_days = 30, config_hash = NULL) {
```

with

```r
#'   session's harmonization config).
#' @param vocab_version Optional current_trait_vocab_version() of the reader.
#'   When given, an envelope whose `trait_vocab_version` differs - or is
#'   missing, i.e. written before trait vocab v2 - is a miss, so old MB/EP/PR
#'   codes are refreshed on first read instead of served for 30 days. Compute
#'   it in the calling process, like `config_hash`.
#' @return The field's value, or NULL if absent/stale/missing/unreadable/foreign-config/foreign-vocab.
#' @export
read_cache_field <- function(cache_file, field, max_age_days = 30, config_hash = NULL,
                             vocab_version = NULL) {
```

and replace

```r
  if (!is.null(config_hash) && !identical(cached$config_hash, config_hash)) {
    return(NULL)
  }
  cached[[field]]
}
```

with

```r
  if (!is.null(config_hash) && !identical(cached$config_hash, config_hash)) {
    return(NULL)
  }
  if (!is.null(vocab_version) &&
        !isTRUE(suppressWarnings(as.integer(cached$trait_vocab_version)) == as.integer(vocab_version))) {
    return(NULL)
  }
  cached[[field]]
}
```

In `R/modules/trait_research_server.R`, replace

```r
      # This session's harmonization settings key the shared trait cache (F72).
      cfg_hash <- harm_config_hash()
```

with

```r
      # This session's harmonization settings key the shared trait cache (F72),
      # and envelopes from another trait vocabulary are misses (C-5).
      cfg_hash <- harm_config_hash()
      vocab_ver <- current_trait_vocab_version()
```

and replace

```r
        cached_traits <- read_cache_field(cache_file, "traits", config_hash = cfg_hash)
```

with

```r
        cached_traits <- read_cache_field(cache_file, "traits", config_hash = cfg_hash,
                                          vocab_version = vocab_ver)
```

In `R/functions/parallel_lookup.R`, replace

```r
  # harm_config_hash() inside it would always see the process default.
  cfg_hash <- harm_config_hash()
```

with

```r
  # harm_config_hash() inside it would always see the process default.
  cfg_hash <- harm_config_hash()
  vocab_ver <- current_trait_vocab_version()
```

and replace

```r
      cached <- read_cache_field(cache_file, "traits", config_hash = cfg_hash)
```

with

```r
      cached <- read_cache_field(cache_file, "traits", config_hash = cfg_hash, vocab_version = vocab_ver)
```

- [ ] **Step 8: No MB predictions from a pre-v2 ML model**

In `R/functions/ml_trait_prediction.R`, `predict_missing_traits()`, replace

```r
  models_package <- load_ml_models()
  if (is.null(models_package)) {
    return(list())
  }

  predictions <- list()
```

with

```r
  models_package <- load_ml_models()
  if (is.null(models_package)) {
    return(list())
  }

  # Trait vocab v2 (C-5) re-keyed MB (MB2 was "burrower", is now "passive
  # floater / drifter"). A model trained on pre-v2 labels predicts MB in the
  # old meaning, so its MB model is not used until it is retrained on v2
  # labels and stamped with trait_vocab_version. MS / FS / EP / PR kept their
  # meaning.
  model_vocab <- models_package$trait_vocab_version
  if (!identical(as.integer(model_vocab), as.integer(current_trait_vocab_version()))) {
    if ("MB" %in% traits_to_predict && !isTRUE(.ml_cache$vocab_warned)) {
      .ml_cache$vocab_warned <- TRUE
      warning(sprintf("[ml] models were trained on trait vocab v%s, the app uses v%s; MB predictions disabled",
                      if (is.null(model_vocab)) "1" else model_vocab, current_trait_vocab_version()),
              call. = FALSE)
    }
    traits_to_predict <- setdiff(traits_to_predict, "MB")
  }

  predictions <- list()
```

- [ ] **Step 9: Update B1's cache tests to the new envelope and call sites**

In `tests/testthat/test-trait-cache-config-hash.R`, replace

```r
write_envelope <- function(path, hash) {
  envelope <- list(traits = data.frame(species = "Gadus morhua", MS = "MS6", stringsAsFactors = FALSE),
                   timestamp = Sys.time())
  if (!is.null(hash)) envelope$config_hash <- hash
  saveRDS(envelope, path)
}
```

with

```r
write_envelope <- function(path, hash) {
  envelope <- list(traits = data.frame(species = "Gadus morhua", MS = "MS6", stringsAsFactors = FALSE),
                   timestamp = Sys.time(),
                   # Trait vocab v2 (C-5): the orchestrator's reader also requires the
                   # envelope's vocabulary to be the current one.
                   trait_vocab_version = current_trait_vocab_version())
  if (!is.null(hash)) envelope$config_hash <- hash
  saveRDS(envelope, path)
}
```

replace

```r
  expect_true(grepl('read_cache_field(cache_file, "traits", config_hash = harm_config_hash())', orch, fixed = TRUE))
```

with

```r
  expect_true(grepl('read_cache_field(cache_file, "traits", config_hash = harm_config_hash(),', orch, fixed = TRUE))
  expect_true(grepl("vocab_version = current_trait_vocab_version())", orch, fixed = TRUE))
```

and replace

```r
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash)', srv, fixed = TRUE)))

  par <- readLines(app_path("R/functions/parallel_lookup.R"), warn = FALSE)
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash)', par, fixed = TRUE)))
```

with

```r
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash,', srv, fixed = TRUE)))
  expect_true(any(grepl("vocab_version = vocab_ver)", srv, fixed = TRUE)))

  par <- readLines(app_path("R/functions/parallel_lookup.R"), warn = FALSE)
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash, vocab_version = vocab_ver)',
                        par, fixed = TRUE)))
```

- [ ] **Step 10: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/functions/offline_db_rebuild.R', 'scripts/initialization/build_offline_trait_db.R', 'R/functions/trait_lookup/orchestrator.R', 'R/functions/validation_utils.R', 'R/modules/trait_research_server.R', 'R/functions/parallel_lookup.R', 'R/functions/ml_trait_prediction.R', 'tests/testthat/helper-fixtures.R', 'tests/testthat/test-trait-cache-config-hash.R', 'tests/testthat/test-trait-vocabulary.R')) parse(file = f); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-trait-vocabulary.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); for (f in c('test-trait-cache-config-hash.R', 'test-deep-analysis-fixes.R', 'test-rebuild-lock.R', 'test-rebuild-observer.R', 'test-trait-routing.R', 'test-trait-lookup-unit.R', 'test-session-isolation.R')) testthat::test_file(file.path('tests/testthat', f))"
```

Expected: `OK`; `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 181 ]` for the vocabulary file (156 + 25; re-verified); the seven files `FAIL 0` (`test-trait-cache-config-hash.R` 30 pass, 2 more than before).

- [ ] **Step 11: Rebuild the LOCAL offline DB (dev box only)**

The dev box's `cache/offline_traits.db` (untracked) was built with vocab v1, so from now on `test-offline-traits.R` "lookup_offline_traits returns data for known species when DB exists" would skip with the vocab warning. Rebuild it locally (reads `data/`, ~1 min, many harmless "Unrecognized feeding mode" warnings); do not touch `cache/offline_traits-laguna-safeBackup-0001.db`:

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/initialization/build_offline_trait_db.R 2>&1 | tail -20
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "con <- DBI::dbConnect(RSQLite::SQLite(), 'cache/offline_traits.db'); print(DBI::dbReadTable(con, 'metadata')); x <- DBI::dbReadTable(con, 'species_traits'); print(table(x[x[['primary_source']] == 'biotic', 'EP'], useNA = 'ifany')); DBI::dbDisconnect(con)"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-offline-traits.R')"
```

Expected: "Total species in database: 3615"; a `trait_vocab_version 2` metadata row; BIOTIC `EP3 483`, `EP4 196` (verified on a scratch copy of the data; the v1 DB had `EP2 453`, `EP3 196`, 30 NA); `test-offline-traits.R` `FAIL 0` with the same skip count as the baseline. Overall EP NA share on the same data: 827/3615 (22.9%) before, 759/3615 (21.0%) after - the zonation -> NA rule does not raise it (acceptance criterion 4).

- [ ] **Step 12: Commit**

```bash
git add R/functions/offline_db_rebuild.R scripts/initialization/build_offline_trait_db.R R/functions/trait_lookup/orchestrator.R R/functions/validation_utils.R R/modules/trait_research_server.R R/functions/parallel_lookup.R R/functions/ml_trait_prediction.R tests/testthat/helper-fixtures.R tests/testthat/test-trait-cache-config-hash.R tests/testthat/test-trait-vocabulary.R
git commit -m "$(cat <<'EOF'
fix(traits): never serve codes from another trait vocabulary (C-5, C3.5, C3.7)

The offline-DB build stamps metadata.trait_vocab_version into its tmp DB
before the atomic install; lookup_offline_traits() skips a DB with a
missing or different version, with one warning per process, until it is
rebuilt. Trait cache envelopes carry trait_vocab_version and the three
trait readers pass vocab_version to read_cache_field(), so pre-v2 files
are refreshed on first read. A pre-v2 ML model no longer predicts MB. The
DB schema is shared by the writer and the new make_offline_db_fixture().

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: Convention note, full suite, push and PR

**Files:**
- Modify: `CONTRIBUTING.md` (one bullet)

**Interfaces:**
- Consumes: Tasks 1-5; the BASELINE line from Task 0.
- Produces: the fix PR (squash-merged without a version bump, Execution ruling 1).

- [ ] **Step 1: Add the convention**

In `CONTRIBUTING.md`, "Trait-pipeline & concurrency patterns", insert a new bullet directly after the bullet that ends with

```
  into `renderUI()`/`insertUI()` without replacing the guard: rendered later,
  the first report can follow the push and be a real click.
```

namely:

```
- **The trait vocabulary lives only in `TRAIT_VOCAB`.** MS/FS/MB/EP/PR
  codes, their labels, the default MB/EP/PR text patterns, their precedence
  and the taxonomic rules are in `TRAIT_VOCAB`
  (`R/config/harmonization_config.R`); read them through `get_trait_vocab()`,
  `trait_codes()`, `trait_code_label()`, `classify_by_patterns(text, trait)`
  and `apply_taxon_rules(taxonomy, trait)`, never through a private regex or
  a literal code list. Sessions and saved JSON may tune `*_patterns` only.
  `tests/testthat/test-trait-vocabulary.R` fails on a habitat, mobility or
  protection word inside a `grepl("...")` in the MB/EP/PR code. When a code
  changes meaning, bump `trait_vocab_version`: offline DBs
  (`metadata.trait_vocab_version`), cache envelopes and the ML model are
  then ignored until rebuilt or retrained, and the production offline DB
  must be rebuilt after the deploy.
```

- [ ] **Step 2: Lint the new code (no new lints)**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/config/harmonization_config.R', 'tests/testthat/test-trait-vocabulary.R', 'tests/testthat/helper-fixtures.R')) { l <- as.data.frame(lintr::lint(f, linters = list(lintr::line_length_linter(120), lintr::trailing_whitespace_linter(), lintr::assignment_linter()))); cat(f, nrow(l), '\n') }"
```

Expected: `0` for all three (verified). The other edited files carry pre-existing lints; do not reformat them.

- [ ] **Step 3: Run the full suite**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('AFTER pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: `fail 0 error 0`; `pass` = BASELINE + 183 (181 in `test-trait-vocabulary.R`, 2 in `test-trait-cache-config-hash.R`); `skip` = BASELINE (only if Task 5 Step 11 rebuilt the local DB; otherwise +1 skip with the "rebuild required" warning). Verified on scratch copies of master and of the finished branch: no test that passed before fails after.

- [ ] **Step 4: Commit**

```bash
git add CONTRIBUTING.md
git commit -m "$(cat <<'EOF'
docs(contributing): the trait vocabulary lives only in TRAIT_VOCAB

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

- [ ] **Step 5: Push and open the PR (STOP - ask the user before running)**

```bash
git push -u origin fix/c5-trait-vocabulary
gh pr create --base master --title "fix(traits): C-5 one trait vocabulary - model codes everywhere, vocab gates, strict link threshold" --body "$(cat <<'EOF'
Implements spec C (`docs/superpowers/specs/2026-09-26-fix-c-trait-pipeline-correctness-design.md`) PR C-5: C1 entirely, the `trait_vocab_version = 2L` DB metadata, the offline-DB vocab gate (C3.5), the cache-envelope vocab check (C3.7) and its contribution to the F72 config hash.

- **One vocabulary (F17):** `TRAIT_VOCAB` holds the model's codes and labels (MB2 = passive floater), default MB/EP/PR patterns, precedence and taxon rules. The live and fuzzy harmonizers, the offline-DB writer, the local BVOL/SpeciesEnriched mappers, the UI legends and the help all use it; a static guard forbids private vocabulary regexes.
- **Reclassifications:** benthopelagic -> EP2 (F35); subtidal/intertidal -> no EP from text (F77); BIOTIC burrowers EP4, attached/tube/free-living EP3 (F18); copepods pelagic before the depth rule (F36); Cnidaria by class, echinoderms PR7 (sea cucumbers PR1), small arthropods PR4 (F38).
- **Model (F70, F71):** PR1 row, MS1/MS6 columns, no hard-coded fallbacks, strict `p > threshold`, validation from the vocabulary. User-approved: `MB_MB["MB1", MB2..MB5]` 0.05 -> 0.10 (calibration placeholder) so sessile consumers keep mobile prey at the default threshold.
- **Stale codes are never served:** the build stamps `metadata.trait_vocab_version`; a DB with another version is skipped (one warning) until rebuilt; cache envelopes carry the version; a pre-v2 ML model predicts no MB.

**After deploy the production offline DB must be rebuilt** (until then it is skipped and lookups use the live APIs).

Deviations, the user's decisions (MB1 recalibration, 1.6.0, MAREDAT kept, ML MB off until C-8) and the verification counts are in `docs/superpowers/plans/2026-09-28-c5-trait-vocabulary.md`.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Expected: PR URL; CI green (the offline testthat job runs `test-trait-vocabulary.R`; it has no offline DB, so the DB-content tests skip as before).

- [ ] **Step 6: Merge (STOP - ask the user)**

Squash-merge per Execution ruling 1 (no version bump in this PR). Then continue with Task 7 from the updated master.

---

### Task 7: Release 1.6.0 (separate release PR)

Run only after the Task 6 PR is merged. C-5 ships alone as 1.6.0 (Execution ruling 2, user decision); later C PRs are 1.6.x.

**Files:**
- Modify: `VERSION`, `R/config.R` (`load_version_info()` fallback), `app.R` (header), `README.md`, `CHANGELOG.md`
- Create: `docs/releases/1.6.0-notes.md`

**Interfaces:**
- Consumes: merged master; tags `v1.5.0`..`v1.5.3`.
- Produces: `VERSION=1.6.0`, `## [1.6.0] - <date>` at the CHANGELOG head with the "trait codes changed" section; tag `v1.6.0`.

- [ ] **Step 1: Branch and check tags**

```bash
git checkout master && git pull --ff-only
git tag -l "v1.5.*"
grep -n -m2 "^## \[" CHANGELOG.md
git checkout -b release/1.6.0
```

Expected: `v1.5.0`..`v1.5.3` listed; the CHANGELOG head is `## [1.5.3] - 2026-09-28`. If `v1.5.3` is missing, **STOP - ask the user** (the generator would merge 1.5.3 into 1.6.0).

- [ ] **Step 2: Bump the version strings**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.6.0 --name "Trait Vocabulary"
sed -i 's/\r$//' VERSION app.R README.md
sed -i 's/^GIT_BRANCH=.*/GIT_BRANCH=master/' VERSION
```

`version_bump.R` writes `VERSION` with CRLF and records the current branch; the two `sed` lines fix both. It does not touch the `R/config.R` fallback: in `load_version_info()`'s `version_info <- list(...)` set `VERSION = "1.6.0"`, `VERSION_NAME = "Trait Vocabulary"`, `RELEASE_DATE = "<release date>"`, `MINOR = 6`, `PATCH = 0` (keep `STATUS = "stable"`, `MAJOR = 1`).

```bash
grep -n "^VERSION=\|^VERSION_NAME=\|^RELEASE_DATE=\|^MINOR=\|^PATCH=\|^GIT_BRANCH=" VERSION
grep -n 'VERSION = "\|VERSION_NAME = "\|RELEASE_DATE = "\|MINOR = \|PATCH = ' R/config.R | head -5
grep -n "CURRENT VERSION" app.R
grep -n "1\.[56]\.[0-9]" README.md
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config.R'); parse(file='app.R'); cat('OK\n')"
```

Expected: `VERSION=1.6.0`, `MINOR=6`, `PATCH=0`, `GIT_BRANCH=master`, today's `RELEASE_DATE`; the same in `R/config.R`; `# CURRENT VERSION: v1.6.0 (...)`; README shows 1.6.0 and no 1.5.3; `OK`.

- [ ] **Step 3: Write the hand-written release notes**

Create `docs/releases/1.6.0-notes.md` with exactly:

```markdown
### Changed — trait codes changed

Trait codes now follow one vocabulary: the food-web model's. Trait tables, offline-DB rows and trait-based
networks exported from 1.5.x or earlier are **not comparable** with 1.6.0 output.

- **Mobility (MB) re-keyed.** MB1 Sessile; MB2 Passive floater / drifter (plankton, medusae); MB3
  Crawler-burrower (includes infaunal burrowers); MB4 Facultative / limited swimmer; MB5 Obligate swimmer.
  Before, the live harmonizer used MB2 = burrower and MB3 = crawler, the ontology path MB3 = floater, the
  local SpeciesEnriched table MB2 = crawler, and the legend MB4 = burrower.
- **Environmental position (EP).** "benthopelagic" is EP2 (was EP1); zonation words (subtidal, sublittoral,
  intertidal, littoral) no longer set an EP - taxonomy or depth decides (subtidal/intertidal were EP4); BIOTIC
  burrowers are EP4 and attached, tube-dwelling and free-living taxa EP3 (were EP3/EP2); "benthic surface" and
  bare "benthic" are EP3; copepods, cladocerans, salps and medusae are pelagic (EP1) before the depth rule;
  the default is EP3 (was EP2).
- **Protection (PR).** Sea urchins, sea stars, brittle stars and crinoids are PR7, sea cucumbers PR1 (were
  PR5); bare "exoskeleton" is PR4 and "heavy exoskeleton" / calcified exoskeletons PR8; "soft shell" is PR5;
  small arthropods PR4 (were PR5 with the crustacean rule off); Scyphozoa/Cubozoa MB2 and Anthozoa MB1 (were all
  MB1). The "Cnidarians -> MB1" rule checkbox is retired.
- **PR1 (mucus / cuticle) is in the model** (a copy of the PR0 row) and validates, as does PR4.
- **New MS1 and MS6 columns** in the EP x MS and PR x MS matrices (copies of MS2 and MS5). These and the PR1
  row are placeholders until calibrated; before, MS1/MS6 prey fell to a hard-coded 0.05 / 0.50.
- **Sessile consumers recalibrated.** In the mobility matrix a sessile (MB1) consumer's probability for mobile
  prey (MB2-MB5) is now 0.10 instead of 0.05, so suspension feeders keep drifting plankton at the default
  threshold. This is a calibration placeholder, not a fitted value.
- **Strict link threshold.** A link now needs a probability strictly above the threshold. At the default 0.05
  every pair sitting at the matrices' 0.05 "implausible" floor loses its link. Threshold 0 no longer links
  every pair (and self-loops).
- An unknown trait code is now an error in the Food Web Construction tab instead of a silent floor value.

### Notes

- **Action required after upgrading: rebuild the offline trait database** (Trait Research > Rebuild Database as
  an unlocked admin, or `Rscript scripts/initialization/build_offline_trait_db.R` from the app directory). Until
  then the old database is skipped with a warning and lookups use the online sources.
- Cached lookups (`cache/taxonomy/`) from 1.5.x are refreshed automatically on first use.
- Machine-learning gap filling no longer predicts MB until the models are retrained on the new codes.
- The confidence label bands (0.34 / 0.67) are unchanged in this release.
```

- [ ] **Step 4: Regenerate the CHANGELOG and re-insert every hand-written note**

Save this throwaway helper outside the repo with the Write tool, e.g. `<scratchpad>/reinsert_release_notes.R` (it checks the first NON-blank line, because `1.5.2-notes.md` and `1.5.3-notes.md` start with a blank line):

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

Then:

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.6.0
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"   # second run must print nothing
sed -i 's/\r$//' CHANGELOG.md
git diff CHANGELOG.md | grep '^-' | grep -v '^---'
grep -n -m2 "^## \[" CHANGELOG.md
grep -n "^### Changed\|^### Results changed\|^### Notes" CHANGELOG.md
tail -c 2 CHANGELOG.md | od -c
```

Expected: the first run prints four `re-inserted ...` lines (1.5.0, 1.5.2, 1.5.3, 1.6.0); the second prints nothing; the `-` list is empty or only footer compare-links; the head is `## [1.6.0] - <date>` then `## [1.5.3]`; a `### Changed` line (the "trait codes changed" heading) directly under `[1.6.0]`, "### Results changed" under `[1.5.0]`, "### Notes" under 1.5.2, 1.5.3 and 1.6.0; the file ends in exactly one `\n`. If any other `-` line appears, `git checkout -- CHANGELOG.md` and paste `scripts/generate_changelog.R --preview --version 1.6.0` above the old head by hand, then re-run the helper.

- [ ] **Step 5: Commit**

```bash
git add VERSION R/config.R app.R README.md CHANGELOG.md docs/releases/1.6.0-notes.md
git commit -m "$(cat <<'EOF'
chore(release): 1.6.0 - trait vocabulary (C-5)

Version 1.6.0 in VERSION, the R/config.R fallback, the app.R header and
README. CHANGELOG regenerated with the hand-written 1.5.0, 1.5.2 and 1.5.3
notes re-inserted and the new 1.6.0 "trait codes changed" section, also
kept in docs/releases/1.6.0-notes.md.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

- [ ] **Step 6: Push, PR, merge, tag (STOP - ask the user before each)**

```bash
git push -u origin release/1.6.0
gh pr create --base master --title "chore(release): 1.6.0 - trait vocabulary" --body "$(cat <<'EOF'
Release PR for C-5 (trait vocabulary). Version strings, regenerated CHANGELOG with every hand-written note re-inserted, and the new "trait codes changed" section (`docs/releases/1.6.0-notes.md`).

After merge: tag `v1.6.0` on the merge commit, deploy, then rebuild the production offline DB (mandatory).

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

After the user merges it:

```bash
git checkout master && git pull --ff-only
git tag -a v1.6.0 -m "1.6.0 - Trait Vocabulary" && git push origin v1.6.0
```

---

### Task 8: Deploy 1.6.0 and rebuild the production offline DB

Every step touches production or the shared server. **Each is STOP - show the user the exact command and wait for confirmation.** Deploy from the merged, tagged master.

**Files:** none in the repo. Remote: `/home/razinka/EcoNeTool_staging/`, `/srv/shiny-server/EcoNeTool/`.

**Interfaces:**
- Consumes: `v1.6.0` on master.
- Produces: production on 1.6.0 with a v2 offline DB.

- [ ] **Step 1: Pre-deploy check (local, read-only)**

```bash
git checkout master && git pull --ff-only && git log -1 --oneline
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; cd ..
```

Expected: the 1.6.0 release commit is HEAD; no errors (the script must run from inside `deployment/`).

- [ ] **Step 2: Record the live DB's state before the deploy (STOP - read-only)**

Write this throwaway script with the Write tool as `<scratchpad>/verify_vocab_db.R`:

```r
con <- DBI::dbConnect(RSQLite::SQLite(), "/srv/shiny-server/EcoNeTool/cache/offline_traits.db")
print(DBI::dbGetQuery(con, "SELECT key, value FROM metadata"))
print(DBI::dbGetQuery(con, "SELECT EP, COUNT(*) AS n FROM species_traits WHERE primary_source = 'biotic' GROUP BY EP"))
print(DBI::dbGetQuery(con, "SELECT COUNT(*) AS total, SUM(EP IS NULL) AS ep_na FROM species_traits"))
DBI::dbDisconnect(con)
```

```bash
scp "<scratchpad>/verify_vocab_db.R" razinka@laguna.ku.lt:/home/razinka/verify_vocab_db.R
ssh razinka@laguna.ku.lt "cd /srv/shiny-server/EcoNeTool && Rscript /home/razinka/verify_vocab_db.R; ls -la config/ | grep -i harmonization"
```

Expected: no `trait_vocab_version` row; BIOTIC rows at EP2/EP3 and none at EP4; note `total` and `ep_na`. The `config/` line shows whether a server-default `harmonization_custom.json` exists (informational: v1 pattern values in it are ignored by `trait_patterns()`, and a `cnidarians_sessile: false` in it only warns).

- [ ] **Step 3: Upload to staging (STOP)**

```bash
powershell ./deploy-windows.ps1 -NoSudo
```

Expected: the script empties staging, uploads code without `data/` (default) and never uploads `config/api_keys.*`, `config/harmonization_custom.json`, `.Renviron`, `*.bak` or `*safeBackup*`. Ignore any printed `sudo` / `rm -rf /srv/shiny-server/EcoNeTool/*` suggestion.

- [ ] **Step 4: Copy over the live tree and reload (STOP)**

```bash
ssh razinka@laguna.ku.lt "cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt"
```

Expected: silent success; `data/`, `cache/`, `.Renviron` survive (`cp -rT` deletes nothing).

- [ ] **Step 5: Verify the code (STOP - read-only)**

```bash
ssh razinka@laguna.ku.lt "cd /srv/shiny-server/EcoNeTool && grep -c 'TRAIT_VOCAB <- local' R/config/harmonization_config.R; grep -c 'reset_offline_vocab_gate' R/functions/trait_lookup/orchestrator.R; grep -c 'offline_db_schema_sql' R/functions/offline_db_rebuild.R scripts/initialization/build_offline_trait_db.R; grep '^VERSION=' VERSION; stat -c %y restart.txt; ls data | head -3; ls data/biotic_traits.csv data/ontology_traits.csv"
curl -sL -o /dev/null -w '%{http_code}\n' http://laguna.ku.lt/EcoNeTool/
```

Expected: `1`, a non-zero count, non-zero counts for both files; `VERSION=1.6.0`; a fresh `restart.txt`; a non-empty `data/` listing and both CSVs present (the rebuild reads them); `200`.

- [ ] **Step 6: Rebuild the production offline DB (STOP - mandatory, spec section 6 item 5)**

Until this runs, every R process logs one "[offline] DB vocab vnone != config v2; rebuild required, offline DB skipped" warning and lookups use the online sources. Either:

- (a) the admin: https://laguna.ku.lt/EcoNeTool/ -> Trait Research -> Configure API Keys -> unlock with the admin password -> close -> **Rebuild Database**, and wait for "Offline database rebuilt! Total species in database: N"; or
- (b) a console run by razinka (same lock, same tmp-then-rename install):

```bash
ssh razinka@laguna.ku.lt "cd /srv/shiny-server/EcoNeTool && Rscript scripts/initialization/build_offline_trait_db.R 2>&1 | tail -25"
```

Expected (b): "Total species in database: ..." and "Database saved to: /srv/shiny-server/EcoNeTool/cache/offline_traits.db". If it reports "Rebuild already running", wait and retry; never delete the lock by hand while a build may be running.

- [ ] **Step 7: Verify the rebuilt DB (STOP - read-only)**

```bash
ssh razinka@laguna.ku.lt "cd /srv/shiny-server/EcoNeTool && Rscript /home/razinka/verify_vocab_db.R && rm /home/razinka/verify_vocab_db.R; ls -la cache/ | grep offline_traits"
```

Expected (spec section 6 item 5 and acceptance criterion 4): a `trait_vocab_version | 2` metadata row; BIOTIC `EP4 > 0` and `EP2 < 50` (dev data: EP3 483, EP4 196, no EP2); `ep_na / total` at most 5 percentage points above the Step 2 share (dev data: it fell from 22.9% to 21.0%); `offline_traits.db` mode `-rw-rw-r--` and no `.lock` / `.tmp.` entries. The running app picks the new DB up on the next lookup; no restart is needed (the gate reads the metadata on every lookup).

- [ ] **Step 8: Smoke test (user or browser automation, with the user's go-ahead)**

On https://laguna.ku.lt/EcoNeTool/:
1. Trait Research -> Trait Code Reference: the MB table reads MB1 Sessile / MB2 Passive floater / drifter / MB3 Crawler-burrower / MB4 Facultative / limited swimmer / MB5 Obligate swimmer; PR lists PR0-PR8 with PR7 "Spines / ossicle plates".
2. Trait Research: look up `Aurelia aurita`, `Echinus esculentus`, `Hediste diversicolor`: Aurelia MB2 (and EP1 when a habitat or taxon rule applies), Echinus PR7, Hediste served from the offline DB (source "offline: biotic").
3. Food Web Construction: load the "simple" example and build at the default threshold: it builds without error with 6 links, including Phytoplankton -> Benthic_filter_feeder (re-verified on the scratch copy).

---

## Self-Review

1. **Spec coverage.** C1.1 single source of truth, `size_labels`/`mobility_labels`/`environmental_labels`, `TRAIT_VOCAB` + `get_trait_vocab()`, not user-overridable, `modifyList` pattern merge, `TRAIT_DEFINITIONS` derived via labels, consumers `trait_foodweb.R` (validation, template) / `trait_research_ui.R` / `trait_help_content.R` (FS7), FS0 label, FS3 warning -> Tasks 1, 3, 4 (deviations 6, 14). C1.2 `classify_by_patterns(text, trait)`, leading boundary, precedence -> Task 1 (deviations 2-4). C1.3 re-keyed patterns, zonation -> NA, bare benthic EP3, PR0/PR5/PR4/PR8, taxon rules (Cnidaria, Echinodermata, Arthropoda), `echinoderms_calcium_plates` -> PR7, `cnidarians_sessile` retired, text-then-taxon with `override_text`, EP order, default EP3 -> Tasks 1-2 (deviations 1, 5, 8). C1.4 fuzzy functions, live cascades, `get_config_pattern` wrapper, build script (BIOTIC EP, ontology via fuzzy, PTDB/BVOL MB2, metadata write), `local_trait_databases.R`, `trait_foodweb.R`, UI tables -> Tasks 1, 2, 4, 5 (deviations 12, 13). C1.5 static guard -> Task 4. C1.6 PR1 row, MS1/MS6 columns, fallback deleted, strict `>`, self-loop test -> Task 3 (finding 17); user-approved MB1 recalibration with its pin test -> Task 3 (deviation 18). C1.7 NA text, invalid-pattern warning, zero-length ranks -> Tasks 1-2. C3.5 vocab gate -> Task 5. C3.7 last bullet (envelope field at both writers, `read_cache_field`, F72 hash) -> Tasks 1 and 5 (deviations 10, 11). Section 5 `test-trait-vocabulary.R` rows: all present (the BIOTIC habit strings, MS3/FS6/MS1, strict threshold, `diag`, guard); the "fixture DB with vocab 1" and "old envelope" rows of the C3 table -> Task 5; `make_offline_db_fixture()` via a shared `offline_db_schema_sql()` -> Task 5 (deviation 9). The existing `test-offline-traits.R:76-87` PR1/PR4 test still passes (Task 1 Step 8). Section 6 items 1 and 5-8 -> Tasks 6-8. Section 8 items 1-4, 7 -> Tasks 1-8 (criterion 4 measured on dev data in Task 5 Step 11, re-checked in Task 8 Step 7).
2. **Placeholder scan.** Every code step carries complete code, identical to the files that were run on a scratch copy of master (end state, re-verified after the MB1 recalibration: `test-trait-vocabulary.R` 181 pass / 0 fail; full suite on the scratch copies: no new failures, +183 pass). `<scratchpad>` and `<release date>` are values the executor fills at run time.
3. **Type consistency.** `trait_codes()`, `trait_definitions()`, `trait_code_label()`, `get_trait_vocab()`, `current_trait_vocab_version()` (Task 1, config) are used unchanged in Tasks 3-5; `classify_by_patterns(text, trait)` / `trait_patterns(trait)` (Task 1) and `apply_taxon_rules(taxonomy, trait, text, override_only)` (Task 2) are used with those signatures in Tasks 2 and 4 and named in the Task 4 guard; `offline_db_schema_sql()`, `reset_offline_vocab_gate()`, `read_cache_field(..., vocab_version =)`, `make_offline_db_fixture(rows, vocab_version)` are defined and used in Task 5. Per-task pass counts 77 / 97 / 130 / 156 / 181 come from the end-state run's per-section sums (77 + 20 + 33 + 26 + 25).
4. **Review Focus.** Each of the five lines names the test that pins it (Tasks 1, 2, 5); the one unpinned input (negations such as "no shell") is stated as a known limitation.
