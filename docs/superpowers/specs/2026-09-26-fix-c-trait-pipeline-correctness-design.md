# Fix C: Trait Pipeline Correctness (Design Spec)

**Status:** Draft, ready for plan-writing
**Date:** 2026-09-26
**Baseline:** master @ 7a96789, VERSION 1.4.4
**Source:** `docs/econetool-deep-analysis-2026-09-26.md`, "Detail" entries and remediation batches 5, 6 and 8
**Depends on:** Sub-project A (network science) and Sub-project B (platform safety, including F19 admin-gated DB rebuild and F72 config-hash trait cache key). Both land first.
**Target release:** 1.6.0. A and B own 1.5.x.

---

## 1. Goal & context

The trait pipeline currently disagrees with itself in three ways:

- **Codes.** A single species can get different MB/EP/PR codes depending on the path it takes: live harmonizer, fuzzy ontology harmonizer, offline DB writer, BVOL/local DBs, or the UI legend. The food-web model then prices those codes with a *fourth* vocabulary (TRAIT_DEFINITIONS plus the MB_MB/EP_MS/PR_MS matrices).
- **Lookups.** Several lookups give wrong raw values: WoRMS units, FishBase grams, AlgaeBase indexing, singularised binomials. One of them crashes the whole batch (zero-length `class`).
- **Confidence and provenance.** Confidence is scored before imputation. Imputed values are cached as ground truth and then fed back into phylo imputation and ML training. Degraded results are cached for 30 days.

Sub-project C makes the model vocabulary the single source of truth, fixes the lookups, and defines one provenance and confidence contract.

Every finding below was re-verified against HEAD 7a96789. Line numbers are current.

## 2. Scope

Status key: C = CONFIRMED, P = PARTIAL (in scope; the corrected claim is noted).

| ID | Sev | Current location | St | Defect (as verified) |
|---|---|---|---|---|
| F17 | high | trait_foodweb.R:48-61,125-131; harmonization_config.R:64-70; harmonization.R:302-320,626-696; build_offline_trait_db.R:441,517-520,592; local_trait_databases.R:511,594-603; trait_research_ui.R:502-506 | C | The same MB code means different things in 6 places. The config has no `mobility_labels`. |
| F18 | high | build_offline_trait_db.R:378-383 | C | Inline BIOTIC regex sends burrow to EP3 and tube/attached/free to EP2. The live DB has 453/679 BIOTIC rows at EP2 and 0 at EP4. |
| F36 | med | harmonization.R:715-741, 844-879 | C | Hard-coded EP/PR regexes; `get_config_pattern()` (:465) is used for MB only. "benthic surface" gives EP1, "soft shell" gives PR0, a copepod at 10-20 m gives EP3, bare "exoskeleton" gives PR8 (config says PR4). |
| F35 | high | harmonization.R:384-388 | C | The fuzzy EP1 `pelagic` test precedes EP2 `benthopel`, so "benthopelagic" gives EP1. |
| F77 | high | harmonization.R:392-397 | C | An unanchored `tidal\|littoral` veto sends "subtidal"/"sublittoral" to EP4. |
| F38 | med | harmonization.R:681-687, 893-913; config :201,:208 | C | Cnidaria (jellyfish too) get MB1. Echinodermata get PR5 "soft shell", but the config/help put urchins at PR7 and the text "calcareous plates" gives PR6. |
| F71 | high | trait_foodweb.R:389, :82-100, :135-144, :458 | C (wider) | `valid_PR` lacks PR1 and PR4. PR1 is also missing from PR_MS, TRAIT_DEFINITIONS, help and `create_trait_template`. |
| F70 | med | trait_foodweb.R:225-233, 253, 265/315, 328/338 | P | The MS1/MS6 prey fallback is hard-coded to 0.05 under `min()`, and the `>=` threshold decides these links. The "self-loops at threshold 0" claim is not reproduced: :290-291 already skips i==j. |
| F33 | high | database_lookups.R:951-959, 1031-1032; harmonization.R:653,660,675,764,771,778,791,880,901,933; trait_research_server.R:361-487 | C (wider) | A `character(0)` class makes `!is.null(x) && grepl()` evaluate to NA and error. This happens in MB, EP and PR, and inside the WoRMS size block. One taxon aborts the whole batch. The report's claim that it "returned MB4" is wrong: it errors. |
| F32 | high | database_lookups.R:1004-1039; orchestrator.R:377-379 | C | The WoRMS `children` Unit is never read, so non-vertebrates are assumed mm and divided by 10. Mytilus gives 2 cm (fixture confirms). |
| F78 | med | database_lookups.R:85-88 | C | FishBase `Weight` (already in g) is multiplied by 1000. The fixture shows cod at 9.6e7 g. |
| F79 | med | database_lookups.R:245-253, 278-282 | C | `length()` on a tibble counts columns and `[[1]]$phylum` errors. The handler has no `warning()`. |
| F9 | high | taxonomic_api_utils.R:1005, 1064-1087; consumers ecopath_import_server.R:599, taxonomic_api_utils.R:1230 | C | WoRMS classification confidence is always "low", so it is discarded. (The report cited the second consumer's file wrongly.) |
| F10 | med | taxonomic_api_utils.R:352, 415, 421, 980, 1033 | C | The first substring match from `common_to_sci` is labelled "high". The cache key omits region. |
| F11 | med | taxonomic_api_utils.R:337-341 | C | Singularisation applies to binomials: "Pollachius virens" becomes "Pollachius viren", "Ammodytes" becomes "Ammodyte". |
| F12 | med | shark_api_utils.R:68-565; shark_server.R; app.R:125,165,185,324-328 | P | 9 `SHARK4R::sharkdata_*` calls. Their non-existence is unverified locally (SHARK4R is not installed). The tab is unconditional, and 6 handlers use `message()`. |
| F20 | med | orchestrator.R:80-92, 406-437 | C | The complete-row early return drops the SELECTed `*_confidence` values and hard-codes "high" (981 rows). The low stored values are PTDB/MAREDAT PR, not BVOL. |
| F25 | med | orchestrator.R:1450-1539, 1581, 1617-1624 | C | UQ runs before phylo. Phylo traits are unscored. `imputation_method` stays "observed". The inline 0.7/0.5 bands disagree with `confidence_to_label()`. |
| F26 | med | harmonization.R:943-944; orchestrator.R:1299-1306 | C | `harmonize_protection(character(), NULL)` returns "PR0", labelled "Taxonomy". PR is never NA, so ML and phylo never fill it. |
| F27 | med | orchestrator.R:1160-1163, 1210-1213, 1257-1260 | C | The fuzzy FS/MB/EP branches assign before any `offline_prefilled` check. |
| F28 | med | orchestrator.R:1105-1110, 1144 | C | `MS_source` is inferred from field presence. BIOTIC/MAREDAT/PTDB sizes never feed `size_cm`. The PTDB rung is dead (`cell_volume` is never set). `FS_source` is the last used source. |
| F29 | med | orchestrator.R:1678-1687 | C | `harmonized` stores imputed codes without `*_source`. Self-match needs a stale (>30 d) own file, because fresh files return early at :243-249. |
| F34 | med | phylogenetic_imputation.R:142-197, 291; scripts/train_trait_models.R:78-95 | C | Relatives are read from every cache file, including the target's own, with no provenance filter. Training uses imputed labels. |
| F37 | med | uncertainty_quantification.R:157-160; orchestrator.R:1529 | C | A size on a boundary gives factor 0, so the geometric mean is 0 and the label "low". A NaN is possible via an adjusted profile (latent). |
| F30 | med | orchestrator.R:1671-1751 | C | The cache write is unconditional. `worms_ok` (:470) is used only for routing. Lookups return `success=FALSE` for both not-found and error. |
| F22 | low | orchestrator.R:85-88, 404-437; build_offline_trait_db.R:772-777 | C (latent) | RS/TT/ST are SELECTed but never assigned. `INSERT OR IGNORE` cannot enrich existing rows. The counter ignores rows affected. |
| F24 | low | orchestrator.R:47, 400; trait_research_server.R:1053, 1216 | C | The offline DB path is relative to the working directory. |

## 3. Non-goals

- Sub-project A (edge direction, TL, MTI) and B (harmonization-settings safety, F72 cache keying, F19 admin gate, deploy/CI). C only *feeds* B's cache key (see C3.7).
- F73 (Databases checkboxes) and F80/F16/F39 message→warning sweep outside the files C touches. Batch 12 owns these.
- Re-tuning the numeric values of MS_MS/FS_MS/MB_MB/EP_MS/PR_MS, apart from the new PR1 row and the MS1/MS6 columns (C1.6).
- Populating `data/external_traits/*.csv`. They are header-only stubs by design. F22 only makes the plumbing correct.
- Rewriting the SHARK tab UX beyond the wire-or-remove rule in C2.8.

## 4. Design

### C1 Vocabulary consistency

**C1.1 Single source of truth.** `R/config/harmonization_config.R` gains `size_labels` (MS1-MS7), `mobility_labels` and `environmental_labels`, next to the existing `foraging_labels` and `protection_labels`. Each is a named list `code → list(name, description, examples)`. `TRAIT_DEFINITIONS` in `trait_foodweb.R:105-145` is then *derived* from these labels through a helper `trait_codes(trait)`.

**Vocabulary is not user-overridable.** The labels, `pattern_precedence`, `taxon_rules` and `trait_vocab_version` live in a `TRAIT_VOCAB` constant built when `harmonization_config.R` is sourced. They are read via `get_trait_vocab()`, never from a session config or a saved JSON. For `*_patterns` (which users may tune), use `modifyList(TRAIT_VOCAB$patterns[[k]], get_harm_config()[[k]] %||% list())`, so a `harmonization_custom.json` saved before C-5 cannot break startup or drop codes.

MS7 keeps its existing handling: it is never prey (trait_foodweb.R:189) and needs no matrix column.

The following consume `trait_codes()` / `*_labels` and never literals:

- `trait_foodweb.R`: validation (:389 and siblings), `create_trait_template` (:458)
- `trait_research_ui.R:502-506`: MB table, and the EP rows below it
- `trait_help_content.R`: MB :124-128, EP :140-143, PR :155-161; the FS list gains FS7

Canonical vocabulary (the model's; the matrices are already built on it):

| Code | Meaning |
|---|---|
| MB1 | Sessile |
| MB2 | Passive floater / drifter (plankton, medusae) |
| MB3 | Crawler-burrower (includes infaunal burrowers) |
| MB4 | Facultative / limited swimmer |
| MB5 | Obligate swimmer |
| EP1 | Pelagic |
| EP2 | Benthopelagic |
| EP3 | Epibenthic |
| EP4 | Endobenthic / infaunal (burrowing) |
| PR0 | None / soft body |
| PR1 | Mucus / cuticle |
| PR2 | Tube |
| PR3 | Burrow refuge |
| PR4 | Thin exoskeleton |
| PR5 | Soft shell (thin CaCO3, juvenile bivalves, moulting crabs) |
| PR6 | Hard shell (mussels, snails, barnacles) |
| PR7 | Spines / ossicle plates (echinoderms) |
| PR8 | Armoured (heavy carapace: crabs, lobsters) |

Smaller label fixes:

- FS0 label is "Primary producer / none" in both sources.
- The warning at trait_foodweb.R:433 says "FS3 (Omnivore)" instead of "Parasite".

**C1.2 Pattern engine.** Add a new function to `harmonization.R`:

```r
classify_by_patterns(text, trait)  # trait in "mobility","environmental","protection","foraging"
```

It reads the merged patterns (C1.1) and `get_trait_vocab()$pattern_precedence[[trait]]`. It lowercases the text, then tests the codes **in precedence order**. Each pattern alternative gets a **leading boundary only**, `(?<![a-z])(?:alt)` (perl), unless it already carries `^`. This keeps stems working ("burrow" still matches "burrowing", "plankton" matches "planktonic", "float" matches "floater"). It returns the first matching code, or `NA_character_`. We rely on **both** the leading boundary and precedence:

- The leading boundary stops `tidal` matching inside "subtidal", `surface` inside "subsurface", and `pelagic` inside "benthopelagic".
- Precedence puts specific codes before generic ones where one phrase legitimately contains two terms: "benthic surface" gives EP3 before EP1, and "soft shell" gives PR5 before PR6 `shell`.

Config additions:

```r
pattern_precedence = list(
  environmental = c("EP4", "EP2", "EP3", "EP1"),
  protection    = c("PR8", "PR7", "PR5", "PR6", "PR4", "PR2", "PR3", "PR1", "PR0"),
  mobility      = c("MB1", "MB5", "MB4", "MB3", "MB2"),
  foraging      = <existing order>
)
```

**C1.3 Re-keyed patterns (intended semantics).**

- `mobility_patterns`:

  | Code | Pattern |
  |---|---|
  | MB1 | `sessile\|attached\|cemented\|colonial hydroid` |
  | MB2 | `drift\|plankton\|float\|passive\|medusa` |
  | MB3 | `crawl\|burrow\|infauna\|endobenthic\|creep\|tube.dwell` |
  | MB4 | `limited swim\|facultative swim\|swim.*occasional` |
  | MB5 | `swimmer\|nekton\|active swim` |

- `environmental_patterns`:
  - EP3 adds `benthic surface`, `attached`, `sessile`, `tube`, `free.living`, `crevice`.
  - EP4 keeps `burrow(ing)?|infauna|endobenthic|interstitial|within sediment`.
  - **Zonation terms (subtidal, sublittoral, intertidal, littoral, eulittoral) determine no EP.** They match nothing, so the result is NA and the taxonomic or depth rules decide. They describe depth zone, not position relative to the substrate. The current mappings (subtidal→EP4 and intertidal→EP4) are both wrong.
  - Bare "benthic" maps to EP3.
- `protection_patterns`:
  - PR0 is `^soft$|soft.?bod(y|ied)|naked|none`.
  - PR5 is `soft.?shell|thin shell|...` and outranks PR0 by precedence.
  - Bare `exoskeleton|cuticle.*chitin` maps to PR4. `armou?r|carapace|heavy exoskeleton|lobster` maps to PR8.
- **Taxonomic rules** in `harmonize_mobility` (:669-696) and `harmonize_protection` (:880-933) move to a config table `taxon_rules[[trait]]`: an ordered list of `list(rank, pattern, code)`. They are evaluated by `apply_taxon_rules(taxonomy, trait)`, with `isTRUE()`-guarded scalar access.
  - Cnidaria (F38):
    - Scyphozoa, Cubozoa, Ctenophora → MB2
    - Anthozoa, Staurozoa → MB1
    - Hydrozoa → MB1, *unless* the text matches medusa/pelagic, then MB2
    - Other or unknown Cnidaria → NA
  - Echinodermata (F38):
    - Echinoidea, Asteroidea, Ophiuroidea, Crinoidea → PR7
    - Holothuroidea → PR1 (leathery body wall, microscopic ossicles)
  - Config rule `echinoderms_calcium_plates` keeps its name but now targets PR7. `cnidarians_sessile` is replaced by the class-branched rule; the key is kept as a no-op for JSON compatibility, with a `warning()` if it is set FALSE.
  - Arthropoda: Malacostraca → PR8; Copepoda, Cladocera, Ostracoda → PR4. The "Soft exoskeleton PR5" fallback at :896 becomes PR4.
- **Text vs taxon, MB and PR:** text runs first (`classify_by_patterns`), then `apply_taxon_rules` fills NA. The exception is a rule flagged `override_text = TRUE`, which wins over text. Only the Echinodermata PR rules carry the flag, so Echinoidea with the text "calcareous plates" gives PR7, not PR6.
- **Order in `harmonize_environmental_position`** (F36):
  1. explicit text via `classify_by_patterns`
  2. taxonomic pelagic rules (Copepoda, Cladocera, Appendicularia, Chaetognatha, Thaliacea, medusae → EP1)
  3. depth rule (currently :731-741)
  4. the default

  Text "Default: epibenthic" at :804 now returns EP3, as the comment says.

**C1.4 Per-file changes.**

- `harmonization.R`
  - The fuzzy functions (:302-320, :384-397) call `classify_by_patterns` on the ontology modality. They keep their categorical confidence output.
  - The live cascades (:626-696, :715-804, :844-944) call `classify_by_patterns` and then `apply_taxon_rules`.
  - `get_config_pattern()` is kept as a thin wrapper.
- `scripts/initialization/build_offline_trait_db.R`
  - Replace the inline regexes with `classify_by_patterns()`. This covers BIOTIC Living_habit→EP (:378-383), cnidarian/ctenophore MB (:441), diatom/BVOL MB (:517-520, :592) and copepod/cladoceran MB.
  - "Non-motile" phytoplankton maps to MB2. Motile phytoplankton maps to MB2 as well: flagellar motility is below the size-scale of MB_MB's facultative swimmer.
  - Write `metadata(key='trait_vocab_version', value=cfg$trait_vocab_version)`.
- `local_trait_databases.R:511, 594-603`: BVOL gives MB2. The local mapping (Crawler MB2, Burrower MB3) is replaced by `classify_by_patterns(..., "mobility")`.
- `trait_foodweb.R`
  - Add `trait_codes()`.
  - Add PR1 (C1.6).
  - Validation uses `trait_codes()`.
- `trait_research_ui.R:502-506`: build the MB/EP tables from `*_labels`, the same way the PR table already is.

**C1.5 Static guard.** A new test targets the vocabulary leak, not code literals: `return("MB2")` stays legal. It scans the bodies of the MB/EP/PR cascade functions in `harmonization.R`, `local_trait_databases.R` and the build script's classification blocks. It fails on any `grepl("` whose pattern literal contains a habitat, mobility or protection vocabulary word (`burrow|pelagic|benth|tidal|littoral|surface|shell|soft|exoskeleton|spine|sessile|drift|swim|crawl|tube|attached`). Name-override regexes in `taxonomic_api_utils.R` are out of its scope. The technique is the same as `test-deep-analysis-fixes.R:507-520`.

**C1.6 Model matrices (F71, F70).**

- `PR_MS` gains a PR1 row equal to the PR0 row. Rationale: mucus or cuticle gives negligible size-structured protection against gape-limited predators, and this avoids inventing new calibration.
- `EP_MS` and `PR_MS` gain an MS1 column (copy of MS2) and an MS6 column (copy of MS5). This is nearest-neighbour extrapolation.
- The hard-coded `0.05` fallback at :225-233 is deleted. An unknown code now errors in validation before construction, instead of silently getting a floor value.
- The link test becomes strictly `p > threshold` (:315, :338). The UI default threshold stays 0.05. **Intended semantics:** 0.05 is the matrices' "implausible" floor (for example MS_MS row MS3 → MS4-MS6, FS4 grazer → MS3+). At the default, a pair with any floor-valued cell is no longer linked. Existing networks will lose those links, and the CHANGELOG says so.
- The existing self-loop skip (:290-291) is kept and gets a test.

**C1.7 Error handling.** `classify_by_patterns` with NA/empty text returns NA with no warning. If a config pattern is invalid regex, it emits `warning("[harmonization] invalid <trait> pattern for <code>: ...")` and skips that code. `apply_taxon_rules` treats NULL or zero-length ranks as absent.

### C2 Lookup correctness

**C2.1 F33.**

- `lookup_worms_traits`: every taxonomy field is normalised with `.scalar_chr(x)`, which returns `NA_character_` if the length is 0, else `x[1]`. This applies at database_lookups.R:951-959, and at :1031-1032 before the size block.
- All harmonizer guards become `isTRUE(!is.na(class) && grepl(...))` through `apply_taxon_rules`.
- The batch loop in `trait_research_server.R:365-405` wraps *each* `lookup_species_traits` call in `tryCatch`. The handler calls `warning(sprintf("[trait_research] lookup failed for '%s': %s", sp, msg))` and appends a row with `species`, all codes NA and `error = msg`. The outer tryCatch (:361, :482-487) remains only for setup failures.

**C2.2 F32.** In the WoRMS size block (:1004-1039):

1. For each body-size row, read `children[[i]]` and pick `measurementValue` where `measurementType == "Unit"`.
2. Convert with `to_cm(value, unit)`: mm ×0.1, cm ×1, m ×100, µm ×1e-4.
3. If the unit child is absent, fall back to the qualitative-size row, then to the class heuristic, with `warning("[worms] no unit for '%s' body size; assuming %s from class")`.
4. Take the max over converted rows.
5. Record `traits$size_unit_source` as one of "child", "qualitative" or "class_heuristic".

**C2.3 F78.** `traits$max_weight_g <- weight` (drop `* 1000`), at :88.

**C2.4 F79.** Guard with `is.null(worms_data) || NROW(worms_data) == 0`. Access `worms_data$phylum[1]` and `worms_data$class[1]`. The error handler adds `warning()` while keeping `result$error <<-`.

**C2.5 F9.** Set `result$confidence <- confidence_to_label(0.66)` ("medium") unconditionally when the WoRMS classification succeeds, before the checks at :1061-1082. Those checks are *name-based overrides*: they force Fish, Benthos or Birds from the common name against WoRMS. They correctly keep "low", because the group is then pattern-derived, not WoRMS-derived. "high" stays reserved for explicit curated sources. Delete the dead `is.null` branch at :1087.

**C2.6 F10.** After `common_to_sci`, keep rows where `tolower(ComName) == tolower(query)` (exact). The candidate set is the exact rows if there are any, else the substring rows.

- One unique species gives "high".
- Several species, of which exactly one matches the region, give "medium".
- Otherwise the first species by `Species` sort order (deterministic) gets "low", plus `warning("[fishbase] '%s' ambiguous: %d candidates")`.

The cache key becomes `<clean_name>__<region|any>.classify.rds`.

**C2.7 F11.** The lookup order is:

1. the original string, as a scientific name and then as a common name
2. only if both return nothing, the singularised form

There is no name-shape gate: the reordering alone fixes F11. English plurals ("Sandeels", "Gobies") still reach step 2 as they do today.

**C2.8 F12 (dead-UI policy).**

- Wrappers `sharkdata_dyntaxa_search`, `sharkdata_worms_search` and `sharkdata_get_biological` are rewired to `SHARK4R::get_dyntaxa_records`, `SHARK4R::match_worms_taxa` and `SHARK4R::get_shark_data`. `lookup_shark_traits` (database_lookups.R:293-310) already uses `get_shark_data`.
- The other six wrappers (algaebase_search, get_parameters, get_physical_chemical, validate, quality_check, list_datasets) are rewired only if a same-purpose function is in `getNamespaceExports("SHARK4R")` at PR time. The implementer installs SHARK4R and records the export list in the PR description. Otherwise each wrapper and its UI control/output in `shark_ui.R`/`shark_server.R` is **removed**.
- If none of the three core functions exists either, the whole tab (app.R:125,165,185,324-328,384,788) is removed.
- All handlers use `warning()`. This is its own PR (C-6b).

### C3 Confidence & provenance contract

**C3.1 Per-trait fields.** For T in MS, FS, MB, EP, PR, RS, TT, ST:

- `T`: the code, or NA.
- `T_source`: the database that produced the code, set **at assignment time**.
- `T_method` (new): one of `observed` (value from a trait DB or measured size), `rule` (taxonomic or depth rule, fuzzy ontology), `ml`, `phylo`.
- `T_confidence`: numeric in [0,1]. It is NA if and only if `T` is NA.

`imputation_method` becomes the aggregate. It is "observed" if every non-NA `T_method` is observed or rule. Otherwise it is the sorted unique non-observed methods joined with "+", e.g. "ml+phylo".

**C3.2 Pipeline order** in `lookup_species_traits`: harmonize (:1099-1308) → ML (:1338-1428) → **phylo** (moved from :1572-1608) → **UQ** (moved from :1450-1539), which scores every non-NA trait → overall label → cache. The overall confidence is the geometric mean of the non-NA `T_confidence` (MS..PR). The label is `confidence_to_label(overall)` (bands 0.34/0.67). The inline 0.7/0.5 bands (:1529-1535, :1617-1624) are deleted. Guard with `isTRUE(is.finite(overall))`, otherwise the label is "none".

**C3.3 Scoring imputed values.**

- Phylo: `T_confidence = T_phylo_confidence × get_database_weight("Phylogenetic")`.
- ML: `T_ml_probability × get_database_weight("ML")`.
- `DATABASE_WEIGHTS` (uncertainty_quantification.R:22-40) gains explicit keys, so nothing falls to the 0.5 default silently: "ML" (alias of "ML_prediction"), "Phylogenetic" 0.55, "OfflineDB" 0.80, "Ontology" 0.60, "Taxonomy" 0.50, "Rule-based" 0.40, "Depth-based" 0.40, "Harmonized" 0.50.
- `get_database_weight()` emits `warning()` for an unknown key.

**C3.4 F37.** `calculate_threshold_distance` computes the distance on the **profile-adjusted** size (the same value `harmonize_size_class` used). The factor becomes `min(1, max(0.3, d / 0.1))`: a measured size on a boundary is still measured data. The function returns NA (not NaN) for non-finite input.

**C3.5 Offline DB path (F20, F22, F24).**

- F24: `lookup_offline_traits(species_name, db_path = app_path("cache/offline_traits.db"))`. trait_research_server.R:1053 and :1216 use `app_path()`.
- F20: the early return (:406-430) and the prefill (:433-437) copy `offline$T_confidence` and set `T_source = offline$primary_source` and `T_method = "observed"`. The overall value is the geometric mean, labelled with `confidence_to_label()`. Stored 0.0 is treated as "unknown" and replaced by `get_database_weight(primary_source)`.
- F22: the reader assigns RS/TT/ST through `assign_trait_if_resolved()`. The writer (:772-777) becomes `INSERT INTO species_traits (...) VALUES (...) ON CONFLICT(species) DO UPDATE SET RS=COALESCE(species_traits.RS, excluded.RS), ...` (SQLite ≥3.24, bundled with RSQLite). The counter adds the rows-affected value returned by `safe_insert`. There is no schema change.
- Vocab gate: if the DB `metadata.trait_vocab_version` is missing or differs from `cfg$trait_vocab_version`, `lookup_offline_traits` emits `warning("[offline] DB vocab v%s != config v%s; rebuild required, offline DB skipped")` once per process and returns NULL. This stops stale MB/EP codes being served between deploy and rebuild.

**C3.6 Orchestrator guards (F26, F27, F28).**

- F26: `harmonize_protection` returns `NA_character_` when there is no protection text and no taxon rule fires. The unconditional `return("PR0")` at :943-944 is deleted. The orchestrator (:1299-1306) sets `PR_source` to "Taxonomy" only when `apply_taxon_rules` fired and to the text source when the text matched. Otherwise PR stays NA and is eligible for ML and phylo.
- F27: each fuzzy branch (:1160, :1210, :1257) is wrapped in `if (!"FS" %in% offline_prefilled)` (and likewise for MB and EP) *before* the assignment.
- F28:
  - A local `size_source` is set wherever `size_cm` is first assigned.
  - Precedence: FishBase > SeaLifeBase > WoRMS > BIOTIC > MAREDAT > PTDB > BVOL > SpeciesEnriched/freshwater/pelagic. A later source fills only when `is.null(size_cm)`.
  - The BIOTIC (:591-611), MAREDAT (:748-755) and PTDB (:767-771) blocks feed `max_length_cm`.
  - The ladder at :1105-1110 is replaced by `result$MS_source <- size_source`.
  - `FS_source` is recorded where FS is assigned, not taken from `sources_used`.

**C3.7 Cache contract (F29, F30, F34).**

- The `harmonized` block (:1678-1687) stores `T`, `T_source` and `T_method` for every trait, plus `trait_vocab_version` and `degraded`.
- Readers:
  - `find_closest_relatives` (phylogenetic_imputation.R:142-197) excludes the file whose species equals the target (normalised name) and uses only values with `T_method %in% c("observed", "rule")`. Legacy files without `T_method` are skipped with a single `warning()`.
  - `scripts/train_trait_models.R:78-95` applies the same filter.
  - `min_matches` is enforced.
- Degraded:
  - Each routed lookup already returns a list. Its error handlers set `result$error <<- conditionMessage(e)`. "Not found" leaves `error` NULL.
  - `degraded <- !worms_ok || any(vapply(raw_traits, function(r) !is.null(r$error), logical(1)))`.
  - A degraded envelope carries `ttl_days = 1`. `read_cache_field` honours `envelope$ttl_days %||% max_age_days`. `result$degraded` is surfaced in the Trait Research table as a warning badge.
- Vocab invalidation: `read_cache_field` treats `envelope$trait_vocab_version != cfg$trait_vocab_version` as stale. `cfg$trait_vocab_version <- 2L` is included in B's F72 config hash, so old `cache/taxonomy/*.rds` files are refreshed on first read instead of serving old MB codes for 30 days.

**C3.8 Error handling.** Every new `tryCatch` uses `warning()` and `<<-` for outer mutation, per CLAUDE.md. Scoring failures leave `T_confidence` NA and the label "none". They never abort the lookup.

## 5. Testing strategy

TDD: each fix lands with its failing test first. All offline tests use `local_mocked_bindings` on synthetic tibbles or recorded fixtures. Anything touching the network is gated with `skip_if_no_live_tests()` and wrapped in `with_timeout()`.

New helper in `tests/testthat/helper-fixtures.R`: `make_offline_db_fixture(rows, vocab_version = 2L)`. It creates a temp SQLite file with the build script's `CREATE TABLE` (sourced from a new `offline_db_schema_sql()` in the build script, so writer and fixture cannot drift) plus metadata, and returns its path.

**`tests/testthat/test-trait-vocabulary.R` (C1)**

| Case | Expected |
|---|---|
| `names(cfg$<T>_labels) == names(TRAIT_DEFINITIONS$<T>)` for MS/FS/MB/EP/PR | TRUE |
| `classify_by_patterns("benthopelagic","environmental")` | EP2 |
| "subtidal", "sublittoral", "intertidal" (environmental) | NA |
| "benthic surface" / "subsurface deposit" / "burrowing" | EP3 / NA / EP4 |
| "soft shell" / "soft" / "exoskeleton" / "heavy exoskeleton" (protection) | PR5 / PR0 / PR4 / PR8 |
| `harmonize_fuzzy_habitat` modality "benthopelagic" | EP2 |
| taxon Scyphozoa / Anthozoa / Hydrozoa (mobility) | MB2 / MB1 / MB1 |
| Echinoidea / Holothuroidea (protection) | PR7 / PR1 |
| Echinoidea plus the text "calcareous plates" | PR7 (override rule) |
| session config with a pre-C-5 JSON lacking `mobility_labels` / `taxon_rules` | `trait_codes("MB")` still returns MB1-MB5 |
| text "burrowing" / "planktonic" / "floater" (stems, mobility) | MB3 / MB2 / MB2 |
| Copepoda at depth 10-20 m, no text (EP) | EP1 |
| BIOTIC habit "Burrow dwelling" / "Attached" / "Tube dwelling" / "Free living" | EP4 / EP3 / EP3 / EP3 |
| `validate_trait_data` with PR1 and PR4 rows | valid = TRUE |
| MS3 FS6 consumer eats MS1 prey | p == min of the five matrix cells; no bare `0.05` literal left in trait_foodweb.R outside the matrices |
| threshold == p exactly | no edge (strict `>`) |
| any threshold | `diag(adj) == 0` |
| static guard (C1.5) | no literal code regex outside config |

The existing test-offline-traits.R:76-85 (PR1/PR4 emitted) must still pass.

**`tests/testthat/test-trait-lookup-correctness.R` (C2)**

| Case | Expected |
|---|---|
| `harmonize_protection(NULL, list(phylum="Nematoda", class=character(0)))` | no error, NA |
| same taxonomy through `harmonize_mobility` and `harmonize_environmental_position` | no error |
| mocked `wm_attr_data` with a body-size row of 20 whose child Unit is "cm" (Mytilus) | `max_length_cm == 20`, `size_unit_source == "child"` |
| same row, no child, class Bivalvia | warning matched `"no unit"`, heuristic applied |
| mocked FishBase `Weight = 96000` | `max_weight_g == 96000` |
| mocked `wm_records_name` tibble (3 cols, 1 row) in the AlgaeBase fallback | phylum read, no error |
| mocked `query_worms` success | classification confidence "medium", kept by the `assign_functional_group_enhanced` filter |
| mocked `common_to_sci` returning "Atlantic cod" (exact) and "Arctic cod" | Gadus morhua, "high" |
| mocked ambiguous, no exact match | "low" plus warning; cache key contains the region |
| lookup candidates for "Pollachius virens" / "Ammodytes" | original tried first; no "viren"/"Ammodyte" call when the original resolves |
| batch loop with one species whose lookup throws | other rows returned, error row present, warning emitted (via `testServer`) |
| SHARK guard: every `SHARK4R::` symbol in R/ is in `getNamespaceExports("SHARK4R")` | `skip_if_not_installed("SHARK4R")` |

Fixtures `worms_mytilus_edulis.rds` (max_length_cm = 2) and `fishbase_gadus_morhua.rds` (9.6e7 g) encode the bugs. They are re-captured with `capture_fixtures.R` under `RUN_LIVE_TESTS=true`, and the diff is reviewed in the PR. `test-trait-lookup-live.R` gains asserts Mytilus ≥ 10 cm and cod weight < 1e6 g.

**`tests/testthat/test-trait-provenance.R` (C3)**

| Case | Expected |
|---|---|
| fixture DB complete row with confidences 0.5/0.8/0.8/0.8/0.3 | `MS_confidence == 0.5`, `PR_confidence == 0.3`, label `confidence_to_label(geomean)` = "medium", not "high" |
| fixture DB with `trait_vocab_version = 1` | NULL plus warning `"rebuild required"` |
| fixture DB via default `db_path` with wd = tests/testthat | found (`app_path`) |
| fixture DB row with RS="RS2" and other codes NA | `result$RS == "RS2"` |
| build upsert on an existing species with a new RS | row updated, counter = rows affected |
| `harmonize_protection(character(), NULL)` | NA; orchestrator leaves PR NA with source NA |
| offline-prefilled FS5 plus a mocked ontology fuzzy FS1 | FS5 kept |
| mocked WoRMS size only, SLB row present without size | `MS_source == "WoRMS"` |
| mocked MAREDAT ESD only | `size_cm` set, `MS_source == "MAREDAT"` |
| mocked phylo imputation of EP | `EP_method == "phylo"`, `EP_confidence > 0`, `imputation_method` contains "phylo" |
| `calculate_threshold_distance` at 5, 20, 50, 150 cm | ≥ 0.3; overall label is not "low" for otherwise-high data |
| non-finite size | NA, no error |
| `find_closest_relatives` over a temp cache holding the target's own file and an `EP_method = "ml"` relative | both excluded |
| mocked WoRMS error | envelope `degraded = TRUE`, `ttl_days = 1`; `read_cache_field` treats it as stale after 1 day |
| envelope with an old `trait_vocab_version` | stale |
| `get_database_weight("ML")` | equals `DATABASE_WEIGHTS[["ML_prediction"]]`; an unknown key warns |

Run with `testthat::test_dir("tests/testthat")` (the legacy `tests/run_all_tests.R` needs `dggridR` and cannot run locally). Nightly live tests cover WoRMS/FishBase re-verification.

## 6. Rollout

Each PR is TDD, with a parse check and the full offline suite green before merge.

1. **PR C-5 (vocabulary)**
   - Contents: C1 entirely, plus everything that stops old codes being served against the new MB_MB:
     - the `trait_vocab_version = 2L` config key and the DB metadata write
     - the offline-DB vocab gate (C3.5, last bullet)
     - the cache-envelope vocab check in `read_cache_field` and the envelope field written at orchestrator.R:422-426 and :1735-1750 (C3.7, last bullet)
     - its contribution to B's F72 hash
   - These ship with the change that makes old codes wrong, not with C-8.
   - Classifications change: MB for burrowers, cnidarians and phytoplankton; EP for BIOTIC burrowers and tube/attached taxa and zonation text; PR for echinoderms, bare "exoskeleton" and "soft shell".
2. **PR C-6a (lookups)**: C2.1-C2.7 and fixture re-capture.
3. **PR C-6b (SHARK)**: C2.8. Record the export list in the PR.
4. **PR C-8 (provenance)**: the rest of C3. Requires C-5's vocab gate and B's F72 cache key.

Deploy and release:

5. **Offline DB rebuild (mandatory, right after C-5 deploys; again after C-8 if C-8 changes writer output).** Run by an admin through the B-gated rebuild (F19 lock and atomic rename) or `scripts/initialization/build_offline_trait_db.R` on laguna. Until then the vocab gate (C3.5) skips the stale DB with a warning, and lookups fall back to live APIs. Verify after the rebuild:
   - `SELECT EP, COUNT(*) FROM species_traits WHERE primary_source='biotic' GROUP BY EP` shows EP4 > 0 and EP2 < 50.
   - `metadata.trait_vocab_version = 2`.
6. **Cache.** `cache/taxonomy/*.rds` is invalidated lazily by the vocab version (C3.7). No manual purge.
7. **Version 1.6.0** (minor): VERSION and `R/config.R` fallback. The CHANGELOG gets a "### Changed — trait codes changed" section that lists:
   - the MB re-key table
   - the EP/PR reclassifications
   - PR1 in the model
   - the new MS1/MS6 matrix columns
   - the strict threshold
   - the confidence label bands (0.34/0.67 replace 0.7/0.5)
   - the note that exported trait tables from ≤1.5.x are not comparable
8. **Deploy.** Use the documented workflow (pre-deploy check from `deployment/`, `-SkipData -NoSudo`, `cp -rT`, `touch restart.txt`), then the rebuild in step 5.

## 7. Risks & open questions

- **Tube-dwellers → EP3.** Many soft-sediment tube polychaetes (Lanice, Owenia) are partly infaunal. We choose EP3 because the tube projects above the sediment and exposes the animal to epibenthic predators. This is revisitable per taxon via `taxon_rules` without code changes.
- **Zonation → NA** reduces text-based EP coverage for ontology rows with only "subtidal". The taxonomic and depth rules fill most of them. Acceptance criterion 4 checks the coverage drop.
- **PR1 = PR0 row** and the **MS1/MS6 nearest-neighbour columns** are placeholders for calibration. The CHANGELOG states this. Sub-project A's network tests may shift. A runs first, so its known-answer tests become the regression net.
- **DATABASE_WEIGHTS new values** (C3.3) are judgement calls. They are chosen to rank below curated DBs (0.80+) and above the implicit 0.5 only for OfflineDB and Ontology.
- **Stricter confidence** reduces "high" labels noticeably. The Ecopath import filter (high/medium) is affected only via F9, which raises WoRMS to medium.
- **SHARK4R API drift.** The package is not installed locally. C-6b may remove most of the tab.
- **Legacy cache files without `T_method`** stop contributing phylo evidence until refreshed. Phylo coverage dips temporarily.
- **SQLite UPSERT** needs ≥3.24. Assert `RSQLite::rsqliteVersion()` in the build script, with a fallback UPDATE-then-INSERT.

## 8. Acceptance criteria

1. The three new test files pass. The full offline suite shows 0 failures and no new skips without a reason.
2. Every reproduction in section 2 is fixed:
   - benthopelagic → EP2, subtidal → NA
   - "benthic surface" → EP3, "soft shell" → PR5
   - copepod at 10-20 m → EP1
   - PR1/PR4 validate
   - a zero-length class does not crash
   - Mytilus ≥ 10 cm, cod weight in grams
   - Pollachius virens resolves
3. The C1.5 guard passes: no habitat, mobility or protection vocabulary regex remains in the cascades or the build script.
4. After the rebuild: BIOTIC EP4 > 0 and the tube/attached taxa are EP3. The share of EP = NA across all DB rows rises by < 5 percentage points.
5. For a mixed batch (Gadus morhua, Mytilus edulis, Acartia, Aurelia aurita, Echinus esculentus, a nematode):
   - no batch abort
   - Aurelia is MB2/EP1, Echinus is PR7, Mytilus is MS5
   - each row has non-NA `T_method` and `T_confidence` for non-NA codes
   - the label agrees with `confidence_to_label(overall_confidence)`
6. The cache envelope contains `T_method`, `trait_vocab_version` and `degraded`. A simulated WoRMS failure produces a 1-day TTL.
7. VERSION is 1.6.0, and the CHANGELOG has the "trait codes changed" section.

## Appendix: excluded or corrected findings

No finding was classified NOT-REPRODUCED or ALREADY-FIXED. The corrections to the source report, as applied above, are:

- **F70:** "threshold 0 makes self-loops" is not reproduced (trait_foodweb.R:290-291 skips i == j). Only that sub-claim is excluded.
- **F33:** "mobility returned MB4" is wrong; it errors (config `fish_obligate_swimmers = TRUE` reaches :653). The scope is widened to 11 guard sites plus database_lookups.R:1031.
- **F9:** the second consumer is taxonomic_api_utils.R:1230, not ecopath_import_server.R:1230.
- **F20:** the "BVOL defaults 0.2-0.4" wording is wrong. No complete row is BVOL; the low stored confidences are PTDB MS/PR and MAREDAT PR.
- **F28:** failure scenario (c) is dead code (the field is `cell_volume_um3`). The rung is removed rather than fixed.
- **F29/F34:** self-match requires a stale (>30 d) own file or a foreign envelope. Fresh files return early at orchestrator.R:243-249.
- **F37:** the NaN path is reachable only through a hand-edited `active_profile` (the UI never writes it). It is kept in scope as a guard.
- **F12:** the non-existence of the `sharkdata_*` functions is unverified (SHARK4R is not installed). It is handled by the export-list rule in C2.8.
