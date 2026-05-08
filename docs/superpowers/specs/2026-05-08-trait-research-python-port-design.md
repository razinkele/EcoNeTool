# Trait Research — Shiny for Python Port (Design)

**Date:** 2026-05-08
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Draft v6 (post different-angle review: UX, security, performance, plan-feasibility)
**Source feature:** EcoNeTool (R Shiny) — `trait_research_*` modules
**Target project:** `EcoNeTool-py/` (sibling repo on OneDrive)

---

## 0. Document conventions

- **R file references** are absolute paths under `…\HORIZON_EUROPE\MARBEFES\Traits\Networks\EcoNeTool\` and abbreviated `R/...` in body text.
- **C-numbered constraints** in § 2 are hard requirements set by stakeholder decisions.
- **R-numbered risks** in § 11 are tracked items with explicit mitigations.
- "**EcoNeTool**" means the R Shiny app; "**EcoNeTool-py**" means this new Python project.

## 1. Goals & non-goals

### Goals

- Port the EcoNeTool **trait research** feature (species → harmonized trait codes MS / FS / MB / EP / PR / RS / TT / ST — eight modalities) to a Shiny for Python application with **full feature parity**, defined precisely by § 13 acceptance criteria.
- Open the door to Python-only ML/imputation: `scikit-learn` (`KNNImputer`, `IterativeImputer`, plus a custom **taxonomy-aware imputer** because sklearn's distance metrics are feature-space, not species-space — see § 6).
- Run on `laguna.ku.lt`, alongside the existing R EcoNeTool but **on a different process manager** (R stays under Shiny Server; Python runs under systemd-managed uvicorn — see § 10).
- Share the SQLite cache and CSV trait databases on disk with EcoNeTool — no runtime coupling, contract-based coupling only. Two SQLite files, both shared (§ 9).

### Non-goals (explicit)

- No FastAPI service layer. Standalone Shiny app; pipeline is a Python library.
- No browser end-to-end (Playwright) tests in the MVP.
- No port of `BHPMF` or `rphylopars`; they are R-only and optional. Their role is taken by `scikit-learn` methods + a custom phylogenetic imputer.
- No restructuring of EcoNeTool itself, except for the **two prerequisite R-side patches** in § 2.5 (WAL + busy_timeout). Both are minimal-surface PRs.
- No Live REST/IPC integration with R EcoNeTool. File-based hand-off only (§ 5.4).

## 2. Driving constraints

| # | Constraint | Source |
|---|-----------|--------|
| C1 | Shared SQLite cache + CSV trait data with EcoNeTool, on the same filesystem | Stakeholder decision |
| C2 | Cache rows written by either app must be consumable by the other | Follows from C1 |
| C3 | Harmonization rules are declarative; R config is the source-of-truth, the YAML is generated from it (not hand-mirrored) — see § 5.1 | Reviewer finding (parity-test circularity) |
| C4 | Deploy target: `laguna.ku.lt`, long-running Python process under systemd | Stakeholder decision + reviewer finding (R uses Shiny Server) |
| C5 | Shiny Core (modules), not Express | Stakeholder decision |
| C6 | Imputation: faithful port of R weighted-vote + agreement-threshold logic + new sklearn methods (with caveats — § 6) | Stakeholder decision + reviewer finding |
| C7 | Sibling project at `OneDrive/.../EcoNeTool-py/`; data paths via env vars | Stakeholder decision |
| C8 | All 8 trait modalities (MS / FS / MB / EP / PR / RS / TT / ST) supported | Reviewer finding (R has 8, spec v1 had 5) |
| C9 | All 17 active R lookup sources represented (§ 4 lookup table) | Reviewer finding (spec v1 had 11) |

### 2.5 Prerequisites — must land before Python ships

These are the *only* changes required to EcoNeTool itself:

| # | Change | File | Owner | Effort |
|---|--------|------|-------|--------|
| P1 | Set `PRAGMA journal_mode=WAL; PRAGMA busy_timeout=5000` on every cache connection in R | `R/functions/cache_sqlite.R:76` (extend the existing `PRAGMA foreign_keys = ON` block) | R team / user | ½ day |
| P2 | Create `/srv/econet-shared/` on `laguna.ku.lt` with group ownership shared between the `shiny` user (R) and a new `econet-py` service user; chmod 0775 | laguna sysadmin | user (sudo) | ½ day |
| P3 | Migrate R EcoNeTool to read CSV trait data from `/srv/econet-shared/data/` and the cache from `/srv/econet-shared/cache/` (currently `cache/` relative to app dir) | `R/config.R` env-var injection | R team / user | 1 day |
| P4 | Reconcile FS-code-name and PR1 disagreements between R UI text and harmonization config; pick canonical labels (config-side authoritative); update both files consistently and bump harmonization-version tag | `R/ui/trait_research_ui.R`, `R/config/harmonization_config.R` | R team / user | ½ day |

P1–P4 add ~2.5 working days of R-side work and one laguna sudo session before Python ships. They are tracked separately in the implementation plan (§ 14).

## 3. Repo & package layout

```
EcoNeTool-py/
├── pyproject.toml                  hatchling build backend; pinned versions § 8.1
├── README.md
├── docs/superpowers/specs/         design docs
├── conf/
│   ├── harmonization.yaml          GENERATED from R/config/harmonization_config.R
│   │                                via tools/regenerate_harmonization.R; never hand-edited
│   ├── routing.yaml                taxonomy → sources, first-match-wins, ~12 rules
│   └── sources.yaml                per-source: base URL, timeout, rate limit, retry
├── traitresearch/                  pure-Python library, NO shiny imports
│   ├── settings.py                 env-var loader (§ 8.4)
│   ├── models/                     pydantic v2
│   │   ├── taxonomy.py             Species, Taxonomy
│   │   ├── traits.py               RawTraits, HarmonizedTraits, SourceResult,
│   │   │                            LookupResult, FetchStatus enum (§ 4 contracts)
│   │   ├── taxonomy_cache.py       TaxonomyCacheEntry (taxonomy.db.species — § 9.2)
│   │   └── offline_cache.py        OfflineCacheEntry (offline_traits.db.species_traits — § 9.3)
│   ├── taxonomy/                   worms.py (4-strategy resolver), gbif.py
│   ├── lookup/                     base.py + ratelimit.py + 17 source modules
│   │   (see § 4 lookup table)
│   ├── orchestrator.py             routing + asyncio orchestration (§ 4)
│   ├── harmonize/
│   │   ├── config.py               loads conf/harmonization.yaml
│   │   ├── size.py        → MS1–MS7
│   │   ├── foraging.py    → FS0–FS7    (xylophagous = FS7; § 5.2)
│   │   ├── mobility.py    → MB1–MB5
│   │   ├── position.py    → EP1–EP4
│   │   ├── protection.py  → PR0–PR8
│   │   ├── reproduction.py → RS1–RS4
│   │   ├── temperature.py  → TT1–TT4
│   │   ├── salinity.py     → ST1–ST5
│   │   └── fuzzy.py                ontology fuzzy-score path
│   ├── impute/
│   │   ├── base.py                 Imputer protocol (§ 6.1)
│   │   ├── phylogenetic.py         port of R weighted-vote w/ agreement gate
│   │   ├── taxonomy_knn.py         custom: precomputed taxonomic-distance kNN
│   │   └── sklearn_methods.py      IterativeImputer (trait-feature-space only)
│   ├── cache/
│   │   ├── schema_check.py         introspect PRAGMA table_info; READ-only validation
│   │   ├── taxonomy_db.py          aiosqlite wrappers for taxonomy.db (§ 9.2)
│   │   ├── offline_db.py           aiosqlite wrappers for offline_traits.db (§ 9.3)
│   │   └── connection.py           PRAGMA setup, busy_timeout, WAL
│   ├── secrets.py                  API-key resolver (§ 8.4)
│   ├── exporters.py                CSV, Excel, RDS-fallback Parquet (§ 5.4)
│   └── lifecycle.py                startup probes, graceful shutdown (§ 7.5)
├── app/                            Shiny Core
│   ├── app.py                      App(app_ui, server) — ASGI entrypoint
│   ├── modules/
│   │   ├── species_input.py        manual / file / 5 test datasets / validate-button
│   │   ├── database_select.py      9-of-17 source checkboxes (some always-on)
│   │   ├── api_keys_modal.py       freshwaterecology / algaebase user-pass / etc.
│   │   ├── lookup_progress.py      live per-species status icons
│   │   ├── results_table.py        harmonized + 4 stat value-boxes + source badges
│   │   ├── results_radar.py        plotly radar chart, 8 axes
│   │   ├── raw_details.py          per-species per-DB raw-data collapsible
│   │   ├── trait_reference.py      static help tab content
│   │   ├── harmonization_settings.py  8 ecosystem profiles + thresholds
│   │   ├── impute_panel.py         UI for impute methods (logic in traitresearch/impute)
│   │   ├── offline_db.py           3 stat value-boxes + rebuild + DT preview
│   │   ├── help_modal.py           ports trait_help_content.R fully
│   │   ├── reference_pdf.py        "Download Reference PDF" + "Open Full Reference Guide"
│   │   ├── downloads.py            CSV / Excel / RDS / Parquet
│   │   └── schema_banner.py        warning banner if cache schema mismatched (§ 9.5)
│   └── www/                        css, logos, fonts
├── data/
│   └── test_datasets/              5 CSVs (baltic_full, baltic_fish, baltic_benthos,
│                                    baltic_plankton, baltic_top_predators);
│                                    last one mixes hardcoded list — see § 5.3
├── tests/
│   ├── unit/                       pytest, hits library only
│   ├── integration/                @pytest.mark.network, pytest-httpx fixtures
│   ├── parity/                     R-vs-Python harmonization parity (§ 8.3)
│   └── fixtures/                   30-species frozen reference + cache_snapshot.sqlite
├── tools/
│   ├── regenerate_harmonization.R  R script: emits conf/harmonization.yaml from R config
│   └── regenerate_parity_fixture.R R script: emits tests/fixtures/r_outputs.json
└── deployment/
    ├── deploy.sh                   rsync to laguna.ku.lt
    └── systemd/econet-py.service
```

### Two architectural rules (CI-enforced)

1. **`traitresearch/` has zero `shiny` imports.** CI check: `! grep -rE "^(import|from) shiny" traitresearch/ tests/parity tests/unit`.
2. **`app/` modules may not contain numpy / sklearn / pandas pipeline code.** Only render controls + delegate. CI check: a `ruff` allow-list per directory.

## 4. Lookup pipeline data flow

### 4.1 Per-species request lifecycle

For one species (the batch case is `asyncio.gather` over species, with each species wrapped in `asyncio.wait_for(..., timeout=settings.species_deadline_s)` — default 30 s):

1. **Cache check** (`taxonomy_db.get(name)`, aiosqlite). Hit-fresh-complete → return early. Stale or partial → continue, will UPSERT.
2. **Offline DB check** (`offline_db.get(name)`). Hit → use as authoritative; merge with cache outcome.
3. **Taxonomy resolution** — `taxonomy.worms.resolve()`, **4-strategy** matching the R fallback chain (`R/functions/trait_lookup/database_lookups.R:830-911`): exact → fuzzy → genus-only → first-two-words for trinomials, with "abort after 2 timeouts" budget. On total failure → `taxonomy.gbif.resolve()`.
4. **Routing** — `orchestrator.route(taxonomy, conf/routing.yaml)`. ~12 rules, first-match-wins.
5. **Sequenced fetch with early-exit** — by default sources are run in dependency order. CSV sources (no I/O wait) execute concurrently inside `asyncio.gather`. REST sources execute one-at-a-time per species but multiple species batch in parallel.

   The early-exit pattern uses `asyncio.wait(..., return_when=FIRST_COMPLETED)` in a loop, **not** raising `CancelledError` inside a sibling task (which can't selectively cancel siblings of `asyncio.gather`):

   ```python
   sources_by_task: dict[asyncio.Task, LookupSource] = {
       asyncio.create_task(s.fetch(taxonomy)): s for s in rest_sources
   }
   pending = set(sources_by_task)
   try:
       while pending and not raw.is_complete():
           done, pending = await asyncio.wait(pending, return_when=asyncio.FIRST_COMPLETED)
           for task in done:
               if task.exception() is not None:                   # fail-soft per § 7
                   log_source_failure(sources_by_task[task], task.exception())
                   continue
               raw.merge(task.result())                            # SourceResult
   finally:
       # Always run cleanup, even if the outer asyncio.wait_for(species_deadline_s)
       # cancels us mid-loop or an unexpected exception escapes the body.
       for task in list(pending):
           src = sources_by_task[task]
           if src.cancellable:                                     # from sources.yaml
               task.cancel()
       await asyncio.gather(*pending, return_exceptions=True)      # drain remaining
   ```

   `LookupSource.cancellable` is read from `conf/sources.yaml` at config-load time (§ 4.3 single-source-of-truth rule). Always-on sources (`cancellable: false`) are never cancelled — they finish, even if `raw.is_complete()` already returned true. CSV sources execute concurrently in a separate `asyncio.gather` step preceding this loop (they don't participate in early-exit because they're already cheap).

   This matches R's early-exit behaviour (`R/functions/trait_lookup/orchestrator.R:506-512, 761`).

   **Always-on sources.** A source can declare `cancellable: false` in `conf/sources.yaml` (e.g., the user explicitly checked the box and wants its data even when the species is already "complete"). Such sources are awaited fully even after early-exit triggers; they are *not* in the cancellable set. The "always-on" status is also surfaced in the UI source-checkbox column so the user understands behaviour.
6. **Raw trait merge** — combines source results into a `RawTraits` model (feeding_mode, mobility_text, habitat, protection_text, size_cm, trophic_level, depth_min/max, body_shape, ontology_scores, plus per-source provenance).
7. **Harmonize** — `harmonize.harmonize_all(raw, profile, config)` → `HarmonizedTraits` with per-trait code, confidence (0–1), and source. Confidence formulas ported as-is from R.
8. **Imputation (if incomplete)** — `impute.get(method).impute(harmonized, taxonomy, cache)`. Three production methods (§ 6).
9. **Cache UPSERT** — `taxonomy_db.upsert(LookupResult)`. Same SQLite schema R EcoNeTool writes; Python only writes columns it knows (column-by-column `INSERT … ON CONFLICT DO UPDATE`); see § 9.4.
10. **Return** to UI; results-table + radar modules re-render reactively. `exporters.to_csv / to_excel / to_rds_fallback / to_parquet` for downloads.

### 4.2 Library contracts (explicit)

```python
class FetchStatus(StrEnum):
    OK       = "ok"        # data returned
    EMPTY    = "empty"     # source has no record for this species
    SKIPPED  = "skipped"   # routing skipped this source
    FAILED   = "failed"    # exception (timeout, HTTP error, parse error)

class TraitObservation(BaseModel):
    value: float | str | None
    unit: str | None = None
    raw_text: str | None = None
    source_record_id: str | None = None       # e.g. WoRMS AphiaID, FishBase SpecCode
    source_url: str | None = None             # deep-link to the upstream record
    harmonization_rule_id: str | None = None  # e.g. "MS.size_threshold.MS5"
    confidence: float | None = None           # 0–1
    is_imputed: bool = False                  # true iff produced by impute/, not lookup/

class SourceResult(BaseModel):
    source: str                                     # "fishbase"
    status: FetchStatus
    reason: str | None = None                       # populated if status != OK
    latency_ms: int | None = None
    data: dict[TraitName, TraitObservation] = {}    # TraitName: Literal[…]

class StalenessState(StrEnum):
    FRESH                  = "fresh"                   # < TTL, served as-is
    STALE_REFRESH_OK       = "stale_refresh_ok"        # TTL exceeded; live re-fetch succeeded
    STALE_REFRESH_FAILED   = "stale_refresh_failed"    # TTL exceeded; re-fetch failed; serving cache

class LookupResult(BaseModel):
    species_name: str
    aphia_id: int | None
    taxonomy: Taxonomy
    raw: RawTraits
    harmonized: HarmonizedTraits          # 8 modalities, each w/ code + conf + src
    sources_used: list[SourceResult]
    imputation_method: str | None         # "observed" | "phylogenetic" | "knn" | "iterative"
    cache_status: Literal["hit", "miss", "stale"]
    staleness_state: StalenessState
    overall_confidence: float
    cached_at: datetime
    refresh_attempted_at: datetime | None  # populated when staleness_state != FRESH
```

UI semantics for `staleness_state`:
- `FRESH` — no badge.
- `STALE_REFRESH_OK` — green check-icon tooltip: "refreshed just now".
- `STALE_REFRESH_FAILED` — **amber clock icon** with tooltip: "served from cache dated {cached_at}; refresh failed: {reason}". This row is also flagged in CSV/Excel exports via a `staleness` column.

### 4.3 LookupSource protocol

```python
class LookupSource(Protocol):
    name: ClassVar[str]              # "fishbase" or "regional:cefas" (subname-style)

    async def fetch(self, taxonomy: Taxonomy) -> SourceResult: ...
```

All other source attributes live in `conf/sources.yaml` — single source of truth, no duplication between protocol and config. The orchestrator looks up these attributes by `source.name`.

### 4.3.1 `sources.yaml` schema

Each source entry is keyed by its full name (including subname for `regional:*`):

```yaml
fishbase:
  host: fishbase.ropensci.org      # informational; logged + rate-limited per host
  parquet_root: https://...        # FishBase/SeaLifeBase only — duckdb path
  timeout_s: 15                    # overrides ECONET_HTTP_TIMEOUT_S for this source
  rate_limit:
    rps: 2                         # max requests per second
    burst: 5
  retry:
    max_attempts: 3
    backoff_seconds: [1, 4, 16]
    jitter_pct: 25
  parallel_eligible: false         # CSV/XLSX: true; rate-limited REST: false
  cancellable: true                # if false, finish even after early-exit triggers
  requires_secret: null            # or e.g. "ECONET_FRESHWATER_KEY"
  applies_when:                    # optional predicate; default: always applies
    not_in_class: [phaeophyceae, florideophyceae, ulvophyceae]   # macroalgae carve-out
```

**`applies_when` predicate grammar.** A small declarative DSL evaluated against `Taxonomy` fields. Supported keys:

| Key | Type | Semantics |
|-----|------|-----------|
| `in_class`, `not_in_class` | list[str] (lower-case) | `taxonomy.class` ∈/∉ list |
| `in_phylum`, `not_in_phylum` | list[str] | same for phylum |
| `in_kingdom`, `not_in_kingdom` | list[str] | same for kingdom |
| `freshwater` | bool | matches the `freshwater: true` computed flag (§ 4.6) |
| `phyto` | bool | matches the `phyto: true` computed flag (§ 4.6) |

A source whose `applies_when` evaluates `False` for a given species is treated as `SKIPPED` for that species. Routing-rule selection happens first (which sources to *consider*); the per-source `applies_when` is the second gate (which to *actually run*). This is what implements the macroalgae carve-out in § 4.6 without requiring an exception in the routing engine.

The schema is validated at startup (§ 7.5 step 2): unknown keys, missing required fields, and references to undefined env vars all fail loudly.

**Subname-style names.** A module that exposes multiple logical sources (e.g., `lookup/regional.py` for the five regional CSVs) registers each with a colon-prefixed name: `regional:cefas`, `regional:blacksea`, `regional:arctic`, `regional:coral`, `regional:pelagic`. The factory in `lookup/__init__.py` resolves `regional:<sub>` by importing `regional.py` and instantiating `RegionalSource(sub="cefas")`. `routing.yaml` references the full subname.

### 4.4 Imputer protocol

```python
class Imputer(Protocol):
    name: ClassVar[str]

    def impute(
        self,
        harmonized: HarmonizedTraits,
        taxonomy: Taxonomy,
        cache: TaxonomyCache,
    ) -> tuple[HarmonizedTraits, ImputationDiagnostics]: ...
```

`ImputationDiagnostics` carries: method, n_relatives_found, agreement_score, per-trait imputed-from-source.

### 4.5 The 17 data sources

(Row 1 — `taxonomy/worms.py` — is a **taxonomy resolver**, not a `LookupSource`; it produces a `Taxonomy` not a `SourceResult`. Listed here for completeness because the orchestrator depends on it. Rows 2–17 implement `LookupSource`.)

| # | Module | Protocol | Source-of-truth | Notes |
|---|--------|----------|----------------|-------|
| 1 | `taxonomy/worms.py` | REST | WoRMS API | Taxonomy + body-size + functional-group + AMBI extraction |
| 2 | `lookup/worms_attrs.py` | REST | WoRMS AphiaAttributesByAphiaID | distinct from #1 |
| 3 | `lookup/fishbase.py` | DuckDB → parquet | FishBase parquet store | see "FishBase / SeaLifeBase note" below |
| 4 | `lookup/sealifebase.py` | DuckDB → parquet | SeaLifeBase parquet store | shares the parquet client |
| 5 | `lookup/algaebase.py` | REST + auth | AlgaeBase | requires `ALGAEBASE_USER` + `ALGAEBASE_PASS` |
| 6 | `lookup/freshwaterecology.py` | REST + key | freshwaterecology.info | requires `ECONET_FRESHWATER_KEY` |
| 7 | `lookup/biotic.py` | CSV | `data/biotic_traits.csv` | parallel-eligible |
| 8 | `lookup/maredat.py` | CSV | `data/maredat_zooplankton.csv` | parallel-eligible |
| 9 | `lookup/ptdb.py` | CSV | `data/ptdb_phytoplankton.csv` | parallel-eligible |
| 10 | `lookup/bvol.py` | XLSX | `data/bvol_nomp_version_2024.xlsx` | parallel-eligible; uses polars + fastexcel |
| 11 | `lookup/species_enriched.py` | XLSX | `data/species_enriched.xlsx` | parallel-eligible |
| 12 | `lookup/ontology.py` | CSV (fuzzy) | `data/ontology_traits.csv` | AphiaID-preferred lookup; primary feeding extraction |
| 13 | `lookup/polytraits.py` | REST | PolyTraits | |
| 14 | `lookup/emodnet.py` | REST | EMODnet biology Btrait | |
| 15 | `lookup/obis.py` | REST | OBIS occurrence + MoF | |
| 16 | `lookup/eol_traitbank.py` | REST | EOL TraitBank | requires `ECONET_TRAITBANK_KEY` (template) |
| 17 | `lookup/regional.py` | CSV (multi) | `data/external_traits/{blacksea,arctic,cefas,coral,pelagic}.csv` | five sub-sources `regional:{cefas,…}`; see § 4.3 subname-style |

**FishBase / SeaLifeBase note (v4 correction):** the legacy `https://fishbase.ropensci.org/...` REST endpoints are retired. `rfishbase` ≥ 4.0 reads parquet files directly via `duckdbfs` against a hosted parquet store (FishBase Cloud). The Python port matches this access model:

- `lookup/fishbase.py` uses `duckdb` (Python package) to query the same hosted parquet files. Connection string follows the upstream URL convention used by `rfishbase` at the version we pin against (record the URL in `conf/sources.yaml::fishbase.parquet_root` and refresh on `rfishbase` major bumps).
- The first lookup of a session caches a local DuckDB attached file under `$ECONET_CACHE_DIR/fishbase/` (the parquet snapshot is reused across species; subsequent queries hit local cache).
- **Cache-size budget.** Hard cap `ECONET_FISHBASE_CACHE_MAX_GB=4` (default; configurable, see § 10.6). A weekly cron under `deployment/cron/econet-py-maint.cron` evicts old files. Same treatment for `sealifebase/`. Total budget on `/srv/econet-shared/`: ≤ 8 GB combined.

  **Eviction policy assumes `relatime` or `strictatime`.** Modern Linux often mounts SSDs with `noatime` for wear reduction; under `noatime`, `find -atime` no-ops and eviction never fires. Two safeguards:
  - The bootstrap script asserts `/srv/econet-shared/` is mounted with `relatime` or `strictatime` (parses `/proc/mounts`); if `noatime` is detected, it errors out and prints the remediation.
  - As belt-and-braces, `lookup/fishbase.py` (and `sealifebase.py`) writes a sidecar `_last_query.json` next to each parquet file on every read, with a `last_used_utc` timestamp. The eviction cron reads these sidecars (not `atime`) when present. So eviction works correctly regardless of mount option after the first bootstrap-time check.
- `sealifebase.py` reuses the DuckDB client wrapper, only swapping the parquet root.
- No `httpx` use for FishBase/SeaLifeBase — they're parquet/DuckDB sources.

This is a real architectural deviation from the "REST + httpx" pattern; tracked in § 4.5 lookup-source table by listing the protocol as "DuckDB → parquet" rather than REST. Pinned: `duckdb >= 0.10` in § 8.1.

**Marked deprecated (not ported):** SHARK (no longer routed in R `orchestrator.R:328`).

`offline_db` is *not* a `LookupSource` — it's a cache-tier check in step 4.1.2.

### 4.6 Routing config (`conf/routing.yaml`)

Representative excerpt — full file targets ~12 rules covering all classes the R orchestrator recognises:

```yaml
- match: { phylum: Chordata, class: Actinopterygii }
  sources: [worms, worms_attrs, fishbase, ontology]
- match: { phylum: Chordata, class: Aves }
  sources: [worms]                      # birds: minimal trait coverage; warn user
- match: { phylum: Mollusca }
  sources: [worms, sealifebase, biotic, polytraits, species_enriched, ontology, regional:cefas, regional:blacksea]
- match: { phylum: Cnidaria, class: Anthozoa }
  sources: [worms, sealifebase, regional:coral, ontology]
- match: { phylum: Annelida, class: Polychaeta }
  sources: [worms, polytraits, biotic, regional:cefas, ontology]
- match: { phylum: Arthropoda, subphylum: Crustacea }
  sources: [worms, sealifebase, biotic, maredat, regional:cefas, regional:arctic, ontology]
- match: { kingdom: Plantae }   # macroalgae
  sources: [worms, algaebase, ontology]
- match: { phyto: true }        # phytoplankton: see expansion below
  sources: [worms, bvol, ptdb, algaebase, ontology]
- match: { freshwater: true }   # WoRMS isMarine == false
  sources: [worms, freshwaterecology, fishbase, ontology]
- match: {}                     # fallback
  sources: [worms, fishbase, sealifebase, biotic, ontology]
```

**Computed match flags.** Some `match` keys are computed properties on `Taxonomy`, not raw fields:

- `phyto: true` — true iff `phylum ∈ {chlorophyta, ochrophyta, haptophyta, dinoflagellata, bacillariophyta, rhodophyta, cyanobacteria, cryptophyta, euglenozoa, charophyta, myzozoa, miozoa}` **OR** `class` matches `/phyceae$/` (covers algal classes). The **macroalgae carve-out** — `class ∈ {phaeophyceae, florideophyceae, ulvophyceae}` — does **not** invert the flag; instead the orchestrator's source-selection logic *de-selects* BVOL and PTDB for these classes while keeping AlgaeBase + freshwaterecology + ontology. Implemented as a per-source `applies_when:` predicate on each source's `sources.yaml` entry, evaluated against the matched rule's source list. Mirrors `R/functions/trait_lookup/orchestrator.R:401-413` (`query_phyto` allow-list of 12 phyla; macroalgae block at lines 409-413 sets `query_bvol = FALSE; query_ptdb = FALSE`).
- `freshwater: true` — true iff WoRMS `isMarine == FALSE` (and not `isBrackish`).
- The full list of computed flags lives in `traitresearch/orchestrator.py::Taxonomy.compute_flags()` and is unit-tested.

Adding a new source = `lookup/<name>.py` + `sources.yaml` entry + `routing.yaml` rule(s). No `orchestrator.py` change.

### 4.7 Cache TTL

30 days, matching R EcoNeTool. Configurable via env var `ECONET_CACHE_TTL_DAYS`.

### 4.8 Per-source edge cases (ported faithfully from R)

These are easy to miss in implementation — flagged here so the source ports include them:

- **Species-name cleaning** before any lookup: strip life-stage prefixes (`adult|juvenile|young|immature|larvae|larval`) and parenthesised authority strings (`Linnaeus 1758` etc.). `R/functions/trait_lookup/database_lookups.R:822-824`. Lives in `traitresearch/taxonomy/normalize.py`.
- **Trinomial fallback to first two words.** `Mytilus galloprovincialis lamarcki` → retry as `Mytilus galloprovincialis`. R strategy 4. (§ 4.1 step 3 already references this.)
- **Genus-only fallback.** If species-level fails, try `<genus>` alone; results are tagged `confidence -= 0.2`. R strategy 3.
- **Substring partial-match for CSV sources** (PTDB, MAREDAT, BIOTIC, ontology): match if database species name *contains* the query OR vice versa. `R/functions/trait_lookup/database_lookups.R:680`. Implemented as a `fuzzy=true` flag on each CSV source's lookup function.
- **Body-size unit normalization** (per-source, in `lookup/`, *not* `harmonize/`). Some sources return `qual_size` strings. Real R rule (`database_lookups.R:994-1003`): if the qualitative-size text contains `"mm"`, divide reported value by 10 (mm → cm); else if `class ∈ {actinopterygii, actinopteri, elasmobranchii}`, treat as already-cm; else divide by 10. Each affected source applies this in its parser before producing `TraitObservation(value=..., unit="cm")`. `harmonize/size.py` consumes already-canonical cm.
- **Cache hit + legacy-row repair.** If a row exists but lacks a column Python expects (legacy rows pre-dating a R-side schema bump), Python treats it as a partial hit, performs the lookup for the missing modalities, and UPSERTs.
- **AphiaID-preferred lookup paths.** BVOL, species_enriched, ontology prefer AphiaID over species_name (R `local_trait_databases.R:182-188`, `database_lookups.R:1095-1100`). The Python sources accept `aphia_id` first and fall back to species_name.
- **Ontology fuzzy primary-feeding extraction.** The ontology source returns fuzzy scores per feeding category; the orchestrator picks the category with the highest score as the primary feeding and emits it as a regular trait observation. `R/functions/trait_lookup/orchestrator.R:236-240`.

## 5. Harmonization

YAML-loaded thresholds + regex patterns + taxonomic rules + 8 ecosystem profiles. Generated from `R/config/harmonization_config.R` by `tools/regenerate_harmonization.R` to break the parity-test circularity (C3, R1).

### 5.1 Generated YAML — workflow

1. Edit `R/config/harmonization_config.R` (R team or user).
2. Tag a release `harmonization-v<n>` in EcoNeTool repo.
3. Run `Rscript tools/regenerate_harmonization.R` → writes `conf/harmonization.yaml` + `tests/fixtures/r_harmonization_outputs.json`.
4. Commit both with the EcoNeTool tag in the commit message.
5. CI parity test compares Python output against the **frozen** fixture. Drift between R config and YAML is impossible because the YAML is regenerated from R; drift between Python harmonization and the fixture fails the build.

This means the YAML is a **build artifact**, not a hand-edited source — file header carries `# AUTO-GENERATED FROM EcoNeTool R/config/harmonization_config.R @ <tag>; DO NOT EDIT`.

**Fallback for non-data fields.** `R/config/harmonization_config.R` is R *code*, not pure data. Today (verified) the file is one `HARMONIZATION_CONFIG` list of static data plus four top-level helper functions outside the list (`get_size_threshold`, `get_foraging_pattern`, `check_taxonomic_rule`, `save_/load_harmonization_config`). The YAML-gen script serialises only `HARMONIZATION_CONFIG` itself, so today's content is fully serialisable. The fallback exists for future-proofing — if R bumps a field inside the list to a function:

- A field whose evaluation produces an atomic vector, named list, or matrix → emitted to YAML.
- A field whose evaluation produces a `function`, `closure`, or unhandled S4 class → emitted as the literal sentinel string `__NOT_SERIALISED__:function:<field_name>` and the R script logs a `WARN`. Python harmonization, on `harmonize/config.py` load, **fails loudly** with `ConfigLoadError` if it encounters this sentinel for any field it expects to consume — never silently treats the sentinel as a regex or threshold.
- The script *errors out* and refuses to write if a previously-serialised field becomes unserialisable mid-version. "Previously-serialised" is defined relative to a snapshot file `tools/harmonization_yaml_gen.serialised_fields.txt` committed to the repo — the script reads this file before regeneration; any change requires the snapshot file to be updated explicitly in the same commit.

### 5.2 Trait modalities (all 8)

Per `R/config/harmonization_config.R:18-84`:

| Modality | Codes | Notes |
|----------|-------|-------|
| MS (max size) | MS1–MS7 | thresholds in cm |
| FS (foraging) | FS0–FS7 | FS7 = xylophagous (R has it; spec v1 missed) |
| MB (mobility) | MB1–MB5 | |
| EP (env. position) | EP1–EP4 | |
| PR (protection) | PR0–PR8 | |
| RS (reproduction) | RS1–RS4 | |
| TT (temperature) | TT1–TT4 | cold-steno/eury → warm-eury/steno |
| ST (salinity) | ST1–ST5 | freshwater → euhaline |

**Open issues — UI-vs-config disagreements (must resolve before YAML-gen lands):**

- **FS code names disagree.** R config `R/config/harmonization_config.R:22` has `FS1_predator` (FS1 = Predator). R UI text `R/ui/trait_research_ui.R:469` shows `FS1 = Herbivore`. The two diverge.
- **PR1 has no regex pattern.** R config has `PR0, PR2, PR3, PR4, PR5, PR6, PR7, PR8` — no `PR1` rule (`R/config/harmonization_config.R:50-58`). R UI shows `PR1 = Mucus/slime` (`R/ui/trait_research_ui.R:540`). PR1 is therefore unreachable from harmonization but rendered in the reference UI.

Both must be reconciled in EcoNeTool first. The decision (config-side authoritative) is captured as **prerequisite P4** in § 2.5 and § 14: the EcoNeTool maintainer (a) chooses the canonical labels for FS1 and decides whether PR1 needs a pattern or should be retired from UI, (b) edits `R/config/harmonization_config.R` and `R/ui/trait_research_ui.R` consistently, (c) bumps the harmonization version. Until P4 lands, the YAML-gen R script refuses to run.

### 5.3 Test datasets

5 datasets returned by `get_test_dataset()` (`R/modules/trait_research_server.R:38-66`), all built from `data/baltic_sea_test_species.csv`:

- `baltic_full`
- `baltic_fish`
- `baltic_benthos`
- `baltic_plankton`
- `baltic_top_predators` — combines birds + mammals from CSV **plus** a hardcoded predatory-fish list: `Gadus morhua, Sander lucioperca, Esox lucius, Salmo salar, Salmo trutta, Perca fluviatilis, Anguilla anguilla`. The Python equivalent in `data/test_datasets/baltic_top_predators.csv` materialises this combination at build time so it's a flat CSV.

Ancillary files on disk (`data/european_marine_test_species.csv`, `data/baltic_benthic_species_reference.csv`) are **not** surfaced as built-in test datasets in the MVP; users can upload them via "From File."

### 5.3.1 Input species-list ingestion (Excel-paste tolerant)

Real users paste species names from Excel, Word, or PDF — the input must tolerate:

- **Encoding:** auto-detect via `charset-normalizer`; accept UTF-8 (with or without BOM), UTF-16 LE (Windows Excel "Save as CSV UTF-8" emits BOM on some locales), Latin-1.
- **Smart-quote / typographic substitution:** strip `"` `"` `'` `'` `«` `»` and Unicode mathematical italic letters (`𝐺𝑎𝑑𝑢𝑠` → `Gadus`). NFKC-normalize, then strip combining marks for ASCII safety in lookup paths (taxonomy DBs are ASCII).
- **Authority-string preservation:** German umlauts in author names (`Müller, 1776`) survive name cleaning (§ 4.8 strips parenthesised authorities, so umlauts only matter for display).
- **Separator detection:** sniff `;`, `,`, `\t`, newline. Excel locales emit `;` on European installs.
- **Preview step:** the species_input module shows a "you pasted N species; here are the first 10 parsed names" preview before allowing Run, so the user catches encoding mishaps without wasting an API budget.

### 5.4 Food Web hand-off (file-based)

R EcoNeTool's Food Web module reads CSVs via `read.csv` (`R/modules/foodweb_construction_server.R:165-167`) and expects columns `species, MS, FS` (uses MB/EP/PR if present). Therefore:

- The Python app exports a **CSV** (not Parquet) at `/srv/econet-shared/handoff/<timestamp>_traits.csv`. `<timestamp>` is `f"{datetime.utcnow():%Y%m%dT%H%M%SZ}"` — never user-supplied. The final path is asserted via `Path(...).resolve().is_relative_to(Path(ECONET_HANDOFF_DIR).resolve())` before write to defeat path-traversal.
- Column schema (mandatory left, provenance/integrity right):
  - `species` (canonical resolved name)
  - 5 trait codes: `MS, FS, MB, EP, PR`
  - 3 optional trait codes: `RS, TT, ST`
  - 8 paired imputed flags: `MS_imputed, FS_imputed, …, ST_imputed` (boolean) — so the Food Web module can never silently consume predictions as observations
  - 8 paired source columns: `MS_source, …, ST_source`
  - `staleness` ∈ {`fresh`, `stale_refresh_ok`, `stale_refresh_failed`}
  - `overall_confidence` (0–1)
- **Header rows (kept above the column-name row, all start with `#` so R `read.csv(..., comment.char = "#")` skips them):**
  - `# EcoNeTool-py trait export`
  - `# Harmonization version: <ECONET_HARMONIZATION_VERSION>`
  - `# App git SHA: <commit>`
  - `# Generated at: <ISO8601 UTC>`
  - `# License: see https://laguna.ku.lt/trait-py/licenses`
  - `# Per-source attribution: WoRMS, FishBase (CC-BY-NC), …`
- **Formula-injection escape:** before serialisation, any cell whose first character is in `{=, +, -, @, \t, \r}` is prefixed with a single quote `'` (Excel/LibreOffice neutralises). Verified by a fixture row containing `Foo =cmd|' /C calc'!A1`.
- Encoding UTF-8, no BOM, trailing newline.
- An integration parity test (`tests/parity/test_foodweb_handoff.R`) runs `read.csv(..., comment.char = "#")` on a Python-written file and asserts column types match R expectations and that the comment headers don't break parsing.

Parquet export is offered in the UI but flagged "for Python tools only — not readable by EcoNeTool." The same provenance and license columns are included.

### 5.5 Settings panel — write paths

`harmonization_settings.py` (UI) mutates an in-session copy of the config and offers two save options:

- **Save for me** (default) — writes `~/.econet-py/harmonization_overrides.json` (user-scope override).
- **Save shared** — writes `/srv/econet-shared/conf/harmonization_overrides.json` (visible to both R and Python). Also writes a `__SOURCE__` audit row. Gated by `ECONET_ADMIN_TOKEN` env var.

**Note on the admin gate:** `ECONET_ADMIN_TOKEN` is read at process start, so the gate is single-tenant — every browser session that hits the running app is either admin or non-admin. There's no per-user identity check (per § 12, "auth not in MVP scope"). This is a **Python-only hardening, not parity** — R EcoNeTool's `harmonization_settings_server.R` writes to disk freely with no analogous gate. The Python design adds the gate because the shared file affects R too. If a per-user model is required later, it slots in as a follow-up under proper auth.

**Configuration load order** (deep-merge precedence, lowest → highest) — note this 4-level layering is a **Python-only enhancement**, not parity. R EcoNeTool uses a single override file (`config/harmonization_custom.json`, loaded via `load_harmonization_config()` in `R/config/harmonization_config.R:196-201`) that *replaces* (not deep-merges) the default. The Python layering is richer because it serves both per-user and shared-admin scopes; the contract with R is preserved by mapping the shared override file to R's `config/harmonization_custom.json` location.

1. `conf/harmonization.yaml` (generated from R config; baseline).
2. `/srv/econet-shared/conf/harmonization_overrides.json` (shared admin override; **also read by R as `config/harmonization_custom.json` via a symlink installed by `bootstrap-laguna.sh`**).
3. `~/.econet-py/harmonization_overrides.json` (user override; Python-only).
4. In-session edits (lost on restart unless the user clicks Save).

**Important runtime caveat.** R EcoNeTool reads `harmonization_custom.json` only at app startup (R Shiny's reactive system doesn't watch the file). A Python "Save shared" therefore requires the R Shiny worker to respawn before the change takes effect for R users. Shiny Server respawns workers per-session for fresh state, so in practice each new R session picks up the latest file — but a long-running R session won't. Documented in `docs/operations.md` § 14.2 (post-shared-save procedure: tell R users to refresh).

The generated `conf/harmonization.yaml` is **never** modified at runtime. § 5.1 invariant.

8 ecosystem profiles (per `R/ui/harmonization_settings_ui.R:106-115`): `arctic, temperate, tropical, mediterranean, atlantic_ne, deep_sea, baltic, black_sea`. All exposed in the dropdown.

## 6. Imputation

Three production methods. All implement the `Imputer` protocol (§ 4.4).

### 6.1 Phylogenetic (`impute/phylogenetic.py`)

Faithful port of R `phylogenetic_imputation.R:283-410`. Weighted majority vote with **agreement-threshold gate** (R uses 0.6 default; ported as-is): only impute when ≥ 60 % of relatives' votes agree on the same code; otherwise leave NA. Vote weights by taxonomic rank match: genus = 5, family = 3 (close) / 4 (sister), order = 2.

### 6.2 Custom taxonomy-aware kNN (`impute/taxonomy_knn.py`)

**Why custom, not sklearn:** sklearn `KNNImputer` accepts a callable metric, but the callable receives row vectors of feature values and is expected to return feature-space distance — it cannot index into a separate species-distance matrix. Therefore we don't use `KNNImputer` for species similarity.

Implementation:

1. Build a precomputed taxonomic-distance matrix over species in the cache (cheap; cached in `~/.econet-py/cache/taxdist.parquet`).
2. For each missing trait, find K nearest species (default K=5) by taxonomic distance.
3. Impute by mode (categorical) or mean (continuous) of the K, weighted by 1 / distance.
4. ~80 lines of code; no sklearn dependency in this method.

### 6.3 Iterative (`impute/sklearn_methods.py`)

Trait-feature-space `IterativeImputer`. Required imports:

```python
from sklearn.experimental import enable_iterative_imputer  # noqa: F401
from sklearn.impute import IterativeImputer
```

Default `max_iter=10, n_nearest_features=10`. Runs sync inside `asyncio.to_thread`. Documented as experimental — sklearn API may change without deprecation.

### 6.4 UI

`app/modules/impute_panel.py` exposes a method dropdown and a "re-impute existing matrix" button. All algorithm code lives in `traitresearch/impute/`; the panel only renders controls and `await`s the chosen `Imputer`.

### 6.5 Offline-DB rebuild — background process

The R app runs offline-DB rebuilds as a `processx::process$new` background R process and polls the running script every 2 s via `invalidateLater(2000)` (`R/modules/trait_research_server.R:938`). Python equivalent:

- Rebuild is spawned by an **`async @reactive.effect`** in `app/modules/offline_db.py` that calls `asyncio.create_subprocess_exec` (a safe argv-list spawn API; no shell, no string interpolation) with the CLI module from the same micromamba env: `["python", "-m", "traitresearch.cli.rebuild_offline_db", "--out", f"{ECONET_CACHE_DIR}/offline_traits.db.tmp"]`. The `--out` path is derived from `ECONET_CACHE_DIR`, not hardcoded.
- A reactive value holds the subprocess handle + start time + last-status-line.
- The same module uses Shiny's `reactive.invalidate_later(2.0)` (also async-aware) to re-render the 3 status value-boxes (running PID / species processed / status text) every 2 s.
- On completion the script atomically renames `.db.tmp` → `.db` (cross-process safe).
- A "Cancel rebuild" button calls `process.terminate()` and resets the reactive state.
- Rebuild path is Linux-only on the deployment server. On Windows dev machines it works under Python 3.8+ default `ProactorEventLoop`; uvloop on Linux uses a different selector but `create_subprocess_exec` is supported there too.

## 7. Error handling & resilience

- **Per-species deadline:** `asyncio.wait_for(lookup_species(s), timeout=ECONET_SPECIES_DEADLINE_S)` (default 30 s). Exceeding the deadline returns a `LookupResult` with `partial=True` and a reason; UI flags the row.
- **Per-source `asyncio.gather(..., return_exceptions=True)`.** One source's failure never aborts the species or the batch.
- `httpx.HTTPError`, `asyncio.TimeoutError`, `json.JSONDecodeError`, `ValueError` caught uniformly; wrapped into `SourceResult(status="failed", reason=...)`.
- **Rate limiting:** async token bucket per host, tunable in `conf/sources.yaml`. HTTP 429 honours `Retry-After`; 5xx retries 3× with exponential backoff (1, 4, 16 s ± 25 % jitter).
- **Cache failures degrade silently:** logged, not raised. UI shows banner if schema-check fails (§ 9.5).
- **Logging:** `loguru` to file under `$ECONET_LOG_DIR` with `rotation="00:00"`, `retention="14 days"`, `compression="zst"`. **Stdout goes to systemd journald** (no loguru rotation on stdout — journald handles it). Schema fields: `species, source, phase, latency_ms, outcome, reason`. Same field names as R `error_logging.R` so logs from both apps are jq-able together.
- **Secret-redaction filter** (mandatory). A loguru filter strips:
  - URL query parameters whose name matches `(?i)^(key|token|api_key|password|secret)$` (replace value with `***`).
  - Any value matching the contents of any env var named `ECONET_*_KEY`, `ECONET_*_PASS`, `ECONET_*_TOKEN`, `ECONET_*_USER` (computed at startup; substring match).
  - HTTP request/response headers from logged tracebacks (httpx `__str__` is filtered through the same redactor).
  Source modules are required to pass credentials in `Authorization` headers — never query strings — when the upstream API supports it. The filter is unit-tested with a fixture-key value to confirm it never leaks. UI error messages drawn from `SourceResult.reason` use a fixed enum (`timeout`, `auth_failed`, `not_found`, `parse_error`, `rate_limit_exceeded`, `unknown`), never raw exception strings.
- **UI feedback:** per-species fetch report — `WoRMS ✓ · FishBase ✓ · OBIS ✗ (timeout) · BIOTIC – (not in DB)`. Status icons: `✓ ok | – empty | ⏱ deadline | ✗ failed | ⊘ skipped`.

### 7.5 Lifecycle

Implemented in `traitresearch/lifecycle.py`. Startup probes (run before serving traffic):

1. `conf/harmonization.yaml` parses + matches expected schema version.
2. `conf/routing.yaml` parses; every referenced source resolves to an importable module.
3. `taxonomy.db` opens RW; `PRAGMA journal_mode` reports `wal`; `PRAGMA busy_timeout >= 5000`.
4. `offline_traits.db` opens (RO if not present — UI shows "no offline DB").
5. Schema check on both DBs (§ 9.5); on mismatch → set `cache_writable=False` and propagate to UI banner.
6. **Subname-style allow-list check.** For every `regional:<sub>` referenced in `routing.yaml`, verify `<sub>` is in `RegionalSource.SUPPORTED_SUBS`. Typos like `regional:cfeas` fail startup loudly rather than silently routing to nothing.
7. Open the shared `httpx.AsyncClient` with pinned limits and HTTP/2:
   ```python
   lifecycle.http_client = httpx.AsyncClient(
       http2=True,
       timeout=httpx.Timeout(ECONET_HTTP_TIMEOUT_S, connect=5.0),
       limits=httpx.Limits(max_connections=64, max_keepalive_connections=32, keepalive_expiry=30.0),
       headers={"User-Agent": f"EcoNeTool-py/{__version__} (+https://laguna.ku.lt/trait-py)"},
   )
   ```
   Closed in shutdown handler. Tests inject a mock by setting `lifecycle.http_client` on a fresh `Lifecycle()` instance.
8. **FishBase/SeaLifeBase parquet probe + pre-warm.** For each `*.parquet_root` URL in `conf/sources.yaml`, open a DuckDB connection, immediately apply the **extension lockdown** (§ 8.1), then run a trivial `DESCRIBE` query. If the URL is unreachable or returns no schema, log `WARN` and mark the source as degraded for the UI. If `ECONET_PARQUET_PREWARM=true` (default in systemd), additionally fire a non-blocking background `SELECT * FROM <species_table> WHERE Species = 'Gadus morhua' LIMIT 1` so the parquet bulk metadata is cached before the first user request. This catches the case where `rfishbase` upstream moves the parquet root mid-version *and* removes a 30–120 s first-fetch latency from the user-perceived path.
9. **Eager-load CSV/XLSX sources.** All small-to-medium local datasets (`biotic_traits.csv`, `maredat_zooplankton.csv`, `ptdb_phytoplankton.csv`, `ontology_traits.csv`, `bvol_nomp_version_2024.xlsx`, `species_enriched.xlsx`, all five regional CSVs under `data/external_traits/`) are loaded into `polars.DataFrame` instances on the `Lifecycle` instance once at startup, reused across sessions, and reloaded only on SIGHUP. ~25 MB resident; saves polars-init cost on every lookup.
10. **Path & env-var validation.** Verify `ECONET_DATA_DIR`, `ECONET_CACHE_DIR`, `ECONET_LOG_DIR`, `ECONET_HANDOFF_DIR` are absolute paths, exist, are owned by `econet-py:econet-shared`, and contain no `..` segments. Refuse startup otherwise.
11. **Symlink integrity check.** Resolve `…/EcoNeTool/config/harmonization_custom.json`. Assert (a) target equals `/srv/econet-shared/conf/harmonization_overrides.json` exactly, (b) target file owner is `root:econet-shared` mode `0664`, (c) parent dir is not group-writable by anything else. Refuse startup otherwise.

Graceful shutdown sequence on SIGTERM. State is held on a `Lifecycle` class (instance per app) in `traitresearch/lifecycle.py`, not module-level globals — so pytest fixtures can construct fresh instances without test cross-pollution:

```python
class Lifecycle:
    def __init__(self):
        self.active_fetches: set[asyncio.Task] = set()      # source.fetch() tasks
        self.background_upserts: set[asyncio.Task] = set()  # cache UPSERT tasks (after step 4.1.9)
        self.http_client: httpx.AsyncClient | None = None
        self.shutting_down: bool = False

    def reset_for_tests(self):                              # called from pytest fixtures
        self.active_fetches.clear()
        self.background_upserts.clear()
        self.shutting_down = False
```

A single `Lifecycle` instance is created in `app.py` at app startup and passed (or accessed via DI) by the orchestrator. Tests instantiate their own.

Each species coroutine adds its UPSERT task to `lifecycle.background_upserts` (with `add_done_callback(lifecycle.background_upserts.discard)`) instead of `await`-ing it inline, so the UPSERT survives the species coroutine's exit.

Sequence:

1. Stop accepting new species-lookup requests at the Shiny reactive layer (set a `_shutting_down` flag).
2. **Cancel all in-flight fetch tasks** (`for t in _active_fetches: t.cancel()`); await the gather with `return_exceptions=True` so cancellations don't propagate.
3. **Drain `_background_upserts`** with a 10 s budget: `await asyncio.wait(_background_upserts, timeout=10)`. UPSERTs already running complete; any still queued may miss the budget — they're best-effort.
4. Close `aiosqlite` connections (one per opened DB, two total).
5. Close the shared `httpx.AsyncClient` (held in `lifecycle.http_client`).
6. Flush loguru file handler.

Systemd unit gives 30 s before SIGKILL (`TimeoutStopSec=30`); steps 2–6 fit within budget.

(`asyncio.shield` is *not* used here — `shield` only protects the awaiter, not the inner coroutine. The detached-task-set pattern above achieves the actual goal of "fetch can be cancelled, UPSERT cannot.")

## 8. Testing strategy

### 8.1 Library pins (`pyproject.toml`)

| Package | Pin | Why |
|---------|-----|-----|
| `shiny` | `>=1.0,<2` | stable Core/Express API |
| `httpx[http2]` | `>=0.27,<0.29` | AsyncClient stable |
| `aiosqlite` | `>=0.20` | modern context manager |
| `pydantic` | `>=2.7,<3` | v2 stable; v1 syntax forbidden (see § 8.5) |
| `scikit-learn` | `>=1.4,<2` | callable metrics + experimental imputer |
| `polars` | `>=1.0` | with `fastexcel` engine |
| `loguru` | `>=0.7` | |
| `pytest` | `>=8` | |
| `pytest-httpx` | `>=0.30` | replaces vcrpy (which doesn't patch httpx) |
| `duckdb` | `>=0.10` | parquet client for FishBase/SeaLifeBase (matches rfishbase backend); see lockdown below |
| `charset-normalizer` | `>=3.0` | encoding sniff for input CSV/Excel paste (§ 5.3.1) |
| `ruff` | `>=0.5` | lint + format |
| `mypy` | `>=1.10` | with pydantic v2 plugin |
| `hatchling` | `>=1.25` | build backend only — no `hatch` CLI dep |
| `uvicorn[standard]` | `>=0.30` | ASGI server; brings httptools + uvloop on Linux |

`uvloop` itself is not pinned in `pyproject.toml` — it's installed separately on the laguna deployment (`micromamba install -n shiny uvloop`) and is a no-op on Windows dev machines.

**DuckDB extension lockdown (mandatory).** Every DuckDB connection runs the following PRAGMAs immediately after open, before any query:

```python
con = duckdb.connect(":memory:")
con.execute("SET autoinstall_known_extensions = false")
con.execute("SET autoload_known_extensions   = false")
con.execute("SET allow_community_extensions  = false")
con.execute("SET allow_unsigned_extensions   = false")
# httpfs is pre-installed at deploy time (bootstrap-laguna.sh runs `LOAD httpfs` once
# against a deployment-pinned DuckDB); never fetched at runtime.
con.execute("LOAD httpfs")
```

Parquet root URLs in `conf/sources.yaml` are pinned to HTTPS only — `http://` is rejected at config-load. Tracks supply-chain blast radius from a hostile parquet root (no extension auto-fetch, no community code).

### 8.2 Test layers

| Layer | Path | Frequency |
|-------|------|----------|
| Harmonization unit tests | `tests/unit/test_harmonize_*.py` | every PR |
| Cache schema round-trip | `tests/unit/test_cache_schema.py` | every PR |
| Cross-language harmonization parity | `tests/parity/test_harmonization_parity.py` | every PR |
| Cross-language imputation parity | `tests/parity/test_imputation_parity.py` | every PR (soft, ≥ 90 %) |
| Food-web hand-off round-trip (R reads Python CSV) | `tests/parity/test_foodweb_handoff.R` | every PR |
| Live API integration (pytest-httpx fixtures) | `tests/integration/test_*_live.py` | nightly + manual |

### 8.3 Parity test details

Fixture: 30 species spanning fish, invertebrates, zooplankton, phytoplankton, plus **one freshwater canary** (`Perca fluviatilis`) so the freshwaterecology source is exercised.

Two parity scopes:

- **Harmonization parity (hard, blocking):** Python harmonization vs `tests/fixtures/r_harmonization_outputs.json`. Diffs fail the build, naming the trait + the rule.
- **Imputation parity (soft, ≥ 90 %, non-blocking):** Phylogenetic imputation vs a snapshot cache. Cache-state-dependent vote weights make exact 100 % brittle; the test fails if match drops below 90 %.

`test_foodweb_handoff.R` exists in R land (run via `Rscript`); CI runs it on Linux runners with R 4.4 installed.

### 8.4 Live API tests (no vcrpy)

Use `pytest-httpx` for unit-level mocking; use **recorded JSON fixtures** for canary tests:

```python
# tests/integration/conftest.py
@pytest.fixture
def fishbase_canary_response():
    return json.loads(Path("tests/fixtures/fishbase/gadus_morhua.json").read_text())
```

Canary tests pull from on-disk JSON. Refreshing means hitting live APIs once and saving the response — there's a dedicated `make refresh-canaries` target. Canaries cover: WoRMS, FishBase, freshwaterecology, AlgaeBase, EOL, PolyTraits, OBIS, EMODnet. Stable test species: `Gadus morhua, Mytilus edulis, Calanus finmarchicus, Perca fluviatilis`.

### 8.5 Forbidden patterns (enforced by lint + review)

**Pydantic v2 — code review must reject:**
- `class Config:` (use `model_config = ConfigDict(...)`)
- `@validator` (use `@field_validator` / `@model_validator`)
- `.dict()` / `.json()` (use `.model_dump()` / `.model_dump_json()`)
- `parse_obj` (use `model_validate`)
- `Field(..., regex=)` (use `pattern=`)
- `allow_population_by_field_name` (use `populate_by_name`)

Enforced via `ruff` rule + `pydantic.deprecated` warnings-as-errors (`-W error::pydantic.PydanticDeprecatedSince20`).

**Asyncio — code review must reject:**
- `asyncio.run(...)` anywhere except the optional CLI entrypoint. Inside Shiny reactives, use `await` (you're already in a running loop).
- `loop.run_until_complete(...)` — same reason.
- `asyncio.gather(...)` without `return_exceptions=True` for fan-out work that should fail-soft (one source's failure must not abort the gather).

**Pydantic v2 introspection — must use v2 names:**
- `Model.__fields__` (v1) → `Model.model_fields` (v2). The column-by-column UPSERT in § 9.5 introspects pydantic models for column names; only `model_fields` is the supported v2 path.

### 8.6 CI shape

| Job | Scope | Runtime budget | Gating |
|-----|------|----------------|--------|
| `lint` | `ruff check` + `ruff format --check` + `mypy --strict` | 30 s | blocking, every PR |
| `arch-check` | shell script enforcing § 3 architectural rules + secret-redaction filter unit test | 10 s | blocking, every PR |
| `test-fast` | Python unit + harmonization unit + cache schema unit + harmonization parity (R fixture pre-committed; no R install needed) | ≤ 2 min | blocking, every PR |
| `test-cross-language` | cache round-trip parity (R-write → Python-read → R-read) + foodweb-handoff R-read test (requires R 4.4 + RSQLite + duckdb-laden rfishbase on the runner) | ~8 min | parallel job; blocking on merge to `main`, **not** on every push |
| `test-network` | live API canaries via real HTTP | ~10 min | nightly + manual; non-blocking |

Parity tests under construction (before fixtures land) carry `@pytest.mark.xfail(strict=False, reason="awaiting fixture under P4")` so CI stays green during early scaffolding.

## 9. Cache concurrency

### 9.1 The two SQLite databases

Per `R/functions/cache_sqlite.R`:

- **`/srv/econet-shared/cache/taxonomy.db`** — runtime cache. Tables: `species` (40+ columns, see § 9.2), `cache_stats`, `lookup_log`. The hot path read/write target.
- **`/srv/econet-shared/cache/offline_traits.db`** — pre-computed bulk database, rebuilt periodically (hours). Tables: `species_traits` (extends `species` with RS/TT/ST/imputation_method/extras), `metadata`. Read-mostly.

Both use WAL mode + `busy_timeout=5000` after P1 lands.

**WAL persistence note.** SQLite's WAL mode is a *per-database setting* that survives across processes — once any connection enables it, the `-wal` and `-shm` files appear next to the DB and the journal mode is sticky for all future connections (until explicitly changed). So if P1 lands and EcoNeTool runs once after the patch, both DBs are permanently WAL. Python's startup probe (§ 7.5 step 3) verifies this; it does *not* re-enable WAL — that would be a no-op against a sticky setting. Implication: **WAL requires a local filesystem**; never put `/srv/econet-shared/` on NFS or SMB (laguna `/srv/` is local disk per § 10.1).

**WAL maintenance (P1 amendment).** The R-side patch P1 also sets `PRAGMA journal_size_limit = 67108864` (64 MB) on every connection so the `-wal` file is bounded. A nightly cron job on laguna runs:

```bash
sqlite3 /srv/econet-shared/cache/taxonomy.db       'PRAGMA wal_checkpoint(TRUNCATE);'
sqlite3 /srv/econet-shared/cache/offline_traits.db 'PRAGMA wal_checkpoint(TRUNCATE);'
```

Tracked in `deployment/cron/econet-py-maint.cron` and documented in § 14.1.

**Backup story.** A second nightly cron runs SQLite's online backup API (safe with WAL active):

```bash
sqlite3 /srv/econet-shared/cache/taxonomy.db ".backup /srv/econet-shared/backups/taxonomy-$(date +%F).db"
```

Retention: 14 days (find -mtime +14 -delete). `offline_traits.db` is **not** backed up — it's regenerable from sources via the rebuild script. Restore procedure documented in § 14.7.

### 9.2 `taxonomy.db.species` schema (canonical)

Columns (verified against `R/functions/cache_sqlite.R:79-136`):

```
id INTEGER PRIMARY KEY,
species_name TEXT UNIQUE,
aphia_id INTEGER,
kingdom TEXT, phylum TEXT, class TEXT, order_name TEXT, family TEXT, genus TEXT,
size_cm REAL, max_length_cm REAL, common_length_cm REAL, weight_g REAL,
feeding_type TEXT, feeding_mode TEXT, mobility_type TEXT, habitat TEXT,
depth_range_m TEXT,
MS TEXT, FS TEXT, MB TEXT, EP TEXT, PR TEXT,
MS_confidence REAL, FS_confidence REAL, MB_confidence REAL,
EP_confidence REAL, PR_confidence REAL, overall_confidence REAL,
MS_source TEXT, FS_source TEXT, MB_source TEXT,
EP_source TEXT, PR_source TEXT, primary_source TEXT,
cached_at TIMESTAMP, updated_at TIMESTAMP,
lookup_count INTEGER, last_accessed TIMESTAMP,
raw_data BLOB                       -- R serialize() output; opaque to Python (§ 9.4)
```

Indexes: 12 (genus, family, order, class, phylum, composite, MS, FS, overall_confidence, primary_source, cached_at, last_accessed). Plus auxiliary tables `cache_stats(id, total_species, last_migration, db_version, created_at)` and `lookup_log(id, species_name, lookup_time_ms, sources_used, traits_found, timestamp)`.

### 9.3 `offline_traits.db.species_traits` schema

Extends `species` with: `RS, TT, ST, trophic_level, depth_min, depth_max, is_hab, longevity_years, growth_rate, body_shape, phyto_motility, phyto_growth_form, RS_confidence, TT_confidence, ST_confidence, imputation_method`.

Plus `metadata(key, value)`.

### 9.4 The `raw_data` BLOB compatibility issue

R writes `raw_data` via `serialize(x, NULL)` — a binary R-internal format. Python **cannot read it without `rdata` + special handling** and even then partially. Decision:

- Python **never reads** `raw_data` from cache rows; it always reconstructs raw traits from individual columns (`feeding_mode`, `mobility_type`, `habitat`, etc.).
- Python **never writes** `raw_data` either; writes `NULL` to that column on UPSERT.
- R EcoNeTool keeps writing `raw_data` for its own use; this is fine because both apps read/write the columns they each understand.
- A row written by Python lacks `raw_data`; R's "raw trait details" panel will show "(no raw data)" for those rows. Acceptable trade-off.

This is documented in code on the relevant model + DAO. `tests/parity/test_cache_roundtrip.py` asserts both directions work for non-BLOB columns.

### 9.5 Schema mismatch behaviour (visible)

Schema is introspected **once at startup** via `PRAGMA table_info` (§ 7.5 step 5) — not per connection acquire (which would add latency to every cache read). The introspection result is cached in module state and re-checked only on (a) explicit user action via an admin button, or (b) a deliberate `SIGHUP` signal handler that triggers re-introspection without restart.

Steps:

1. Compute the set of columns Python knows about per table.
2. Verify each is present in the DB; warn for unknowns (don't fail).
3. If a *known* column is missing → `cache_writable=False`; UI displays a persistent **red banner**: "Shared cache schema doesn't match expectations. Lookups still work, results are NOT being cached. Tell the EcoNeTool maintainer."
4. UPSERTs use only columns Python knows about (`INSERT … (col1, col2, …) ON CONFLICT(species_name) DO UPDATE SET col1=excluded.col1, …`). New columns added on the R side are preserved by SQLite.

**SQL-injection invariants** (enforced by code review and a unit test):

- Column identifiers in the UPSERT come *only* from `set(model.model_fields.keys()) & set(columns_from_PRAGMA_table_info)`. Both sets are static identifiers (compile-time + introspected from a trusted DB); never user input.
- Values are bound with positional `?` parameters via `aiosqlite.execute(sql, params)`. **No f-string interpolation of values, ever.** A unit test in `tests/unit/test_upsert_safety.py` asserts the generated SQL has exactly one `?` per non-key column and that the parameter tuple length matches.
- All harmonized TEXT values are length-capped at 256 chars, asserted `value.isprintable()`, and have control characters stripped before INSERT (defence against R rendering crafted strings as HTML).

**Runtime TOCTOU defence.** The startup-cached column set can become stale if R drops or renames a column while Python is running. UPSERTs additionally catch `aiosqlite.OperationalError` whose message matches `(no such column|has no column named)`; on hit, set `cache_writable=False` at runtime, raise the same red banner as the startup probe, and log `ERROR`. SIGHUP forces full re-introspection.

### 9.6 Migration rule (asymmetric)

- **Schema migrations are R-app responsibility.** Python never runs `ALTER TABLE`.
- Python's `cache/schema_check.py` validates known columns are present + logs unknown columns (informational).
- If a new column is needed, R adds it first, deploys, then Python is updated to use it. Documented + flagged in § 11 R8.

## 10. Deployment

### 10.1 Topology

```
laguna.ku.lt
├── /srv/shiny-server/EcoNeTool/         R Shiny, runs under shiny-server (port 3838)
│   └── (uses /srv/econet-shared/* via env vars)
├── /srv/econet-py/                      Python app, runs under systemd (port 8001)
│   ├── .venv/                           micromamba env or pip venv
│   └── ...
└── /srv/econet-shared/                  shared filesystem (P2)
    ├── data/                            CSV trait databases, BVOL.xlsx, …
    ├── cache/
    │   ├── taxonomy.db
    │   └── offline_traits.db
    ├── conf/
    │   └── harmonization_overrides.json (admin-saved overrides, both apps read)
    ├── handoff/
    │   └── <timestamp>_traits.csv       Food Web hand-off files
    └── logs/
        └── econet-py.log                loguru file output
```

Group ownership of `/srv/econet-shared/` is shared between `shiny` (R user) and `econet-py` (new service user). `chmod g+s` on directories so new files inherit the group. Default umask 002 in the systemd unit.

### 10.2 systemd unit (`deployment/systemd/econet-py.service`)

```ini
[Unit]
Description=EcoNeTool-py trait research Shiny app
After=network.target

[Service]
Type=simple
User=econet-py
Group=econet-shared
WorkingDirectory=/srv/econet-py
Environment="ECONET_DATA_DIR=/srv/econet-shared/data"
Environment="ECONET_CACHE_DIR=/srv/econet-shared/cache"
Environment="ECONET_LOG_DIR=/srv/econet-shared/logs"
Environment="ECONET_HANDOFF_DIR=/srv/econet-shared/handoff"
Environment="ECONET_CACHE_TTL_DAYS=30"
Environment="ECONET_SPECIES_DEADLINE_S=30"
Environment="ECONET_HTTP_TIMEOUT_S=15"
Environment="ECONET_PARQUET_PREWARM=true"
EnvironmentFile=/etc/econet-py/secrets.env   # API keys, mode 0600 (see § 10.6)
ExecStart=/srv/econet-py/.venv/bin/uvicorn app.app:app --host 127.0.0.1 --port 8001 --workers 1
Restart=on-failure
RestartSec=5s
TimeoutStartSec=60
TimeoutStopSec=30
MemoryHigh=2G
MemoryMax=3G
UMask=0002

[Install]
WantedBy=multi-user.target
```

A companion timer `econet-py-restart.timer` runs `OnCalendar=Sun 04:00` triggering `econet-py-restart.service` (a one-shot that calls `systemctl restart econet-py.service`). Cleans accumulated reactive state and any memory leaks; coordinated with the nightly maintenance crons (§ 9.1) to avoid simultaneous restarts.

**`MemoryMax` operational caveat.** `MemoryMax=3G` is a hard SIGKILL — no graceful shutdown, no UPSERT drain, in-flight species lose results. On a host with 16 GB RAM shared with R EcoNeTool the limit is necessary, but it can fire mid-batch on a hostile request (e.g., a 500-species batch with cold parquet cache + radar render). Mitigations: (a) **`MemoryHigh=2G`** is the soft throttle (kernel pressures the cgroup; processes throttled, not killed); the kill threshold sits at 3 GB so legitimate large batches still complete; (b) the species-batch UI caps input at **500 species per Run** (a soft validation in `species_input.py`) — larger batches must be split; (c) if SIGKILL fires, systemd auto-restarts within 5 s (`Restart=on-failure`, `RestartSec=5s`), and the cache TTL ensures no data loss for already-UPSERTed species. Tracked as R23 below.

Workers = 1 because Shiny in-memory reactive state is not shared across workers; scale by async, not processes.

### 10.3 Reverse proxy

Nginx config snippet (deploy adds, doesn't replace anything):

```nginx
location /trait-py/ {
    proxy_pass         http://127.0.0.1:8001/;
    proxy_http_version 1.1;
    proxy_set_header   Host $host;
    proxy_set_header   Upgrade $http_upgrade;       # WebSocket for Shiny
    proxy_set_header   Connection "upgrade";
    proxy_read_timeout 600s;
}
```

EcoNeTool's existing Shiny Server config at port 3838 / `/EcoNeTool/` is untouched.

**Auth (MVP-deferred but flagged).** Per § 12, full auth is out of MVP scope. As a stop-gap, the nginx `location /trait-py/` block adds either:

- `auth_basic "EcoNeTool-py"; auth_basic_user_file /etc/nginx/.htpasswd-econet;` with the laguna user list, OR
- `allow 10.0.0.0/8; deny all;` if laguna is on a closed VLAN, OR
- both (defence-in-depth).

The choice is documented in `docs/operations.md` § 14.8 alongside how to add/revoke a user. The rebuild-DB button additionally requires `ECONET_ADMIN_TOKEN` (§ 5.5) — basic auth gates *who can use the app*; admin token gates *who can mutate shared state*.

### 10.4 Deploy script (`deployment/deploy.sh`)

```bash
rsync -avz --delete \
  --exclude='.git/' --exclude='__pycache__/' --exclude='.venv/' \
  EcoNeTool-py/ razinka@laguna.ku.lt:/srv/econet-py/
ssh razinka@laguna.ku.lt 'sudo systemctl restart econet-py.service'
```

### 10.5 First-time bootstrap

A separate one-shot script `deployment/bootstrap-laguna.sh` documents and partially automates: create `econet-py` user, create `/srv/econet-shared/`, install nginx config snippet, install systemd unit, install symlink `…/EcoNeTool/config/harmonization_custom.json → /srv/econet-shared/conf/harmonization_overrides.json`. Re-runnable: every action is guarded (`id -u econet-py >/dev/null 2>&1 || useradd …`, `mkdir -p`, nginx snippet wrapped in `# BEGIN econet-py … # END econet-py` markers and replaced as a unit, `chmod g+s` only applied if not already set, secrets file *never* overwritten if present). Verified by running the script twice on a clean Ubuntu VM in CI.

**Caveats with the "idempotent" claim:** existing files in `/srv/econet-shared/` retain their original group ownership unless re-chgrp'd; the script does NOT mass-chgrp on re-run. Also, deleting the user after a run requires manual cleanup — script never deletes.

### 10.6 Environment variables (canonical list)

| Variable | Where set | Purpose | Default |
|---------|-----------|---------|---------|
| `ECONET_DATA_DIR` | systemd unit | CSV/Excel trait data root | `/srv/econet-shared/data` |
| `ECONET_CACHE_DIR` | systemd unit | SQLite cache directory (both DBs) | `/srv/econet-shared/cache` |
| `ECONET_LOG_DIR` | systemd unit | loguru file output directory | `/srv/econet-shared/logs` |
| `ECONET_HANDOFF_DIR` | systemd unit | Food-Web hand-off CSV write path | `/srv/econet-shared/handoff` |
| `ECONET_CACHE_TTL_DAYS` | systemd unit | Cache freshness TTL | `30` |
| `ECONET_SPECIES_DEADLINE_S` | systemd unit | Per-species lookup wall-clock cap | `30` |
| `ECONET_HTTP_TIMEOUT_S` | systemd unit | Default `httpx.AsyncClient` timeout | `15` |
| `ECONET_HARMONIZATION_VERSION` | systemd unit | Tag of the YAML-gen script run; checked at startup against `conf/harmonization.yaml` header | matches the committed YAML |
| `ECONET_MAX_SPECIES_PER_RUN` | systemd unit | Soft UI cap on batch size; protects against `MemoryMax` SIGKILL on hostile inputs | `500` |
| `ECONET_FISHBASE_CACHE_MAX_GB` | systemd unit | Hard cap on local DuckDB parquet cache (FishBase + SeaLifeBase combined → 8 GB) | `4` |
| `ECONET_ADMIN_TOKEN` | systemd unit (optional) | Settings-panel "Save shared" gate; unset → button disabled | unset |
| `ECONET_FRESHWATER_KEY` | `EnvironmentFile` (secrets) | freshwaterecology.info API key | required |
| `ECONET_ALGAEBASE_USER` | `EnvironmentFile` (secrets) | AlgaeBase auth user | required |
| `ECONET_ALGAEBASE_PASS` | `EnvironmentFile` (secrets) | AlgaeBase auth password | required |
| `ECONET_TRAITBANK_KEY` | `EnvironmentFile` (secrets) | EOL TraitBank key (template; not currently required) | empty |
| `ECONET_WORMS_KEY` | `EnvironmentFile` (secrets) | template; WoRMS does not currently require a key | empty |

Loaded via `pydantic.BaseSettings` in `traitresearch/settings.py` + `traitresearch/secrets.py`. Missing required keys cause sources requiring them to log `WARN` and route as `SKIPPED`.

## 11. Risks & open questions

| # | Risk | Mitigation |
|---|------|-----------|
| R1 | YAML drifts from R `harmonization_config.R` | YAML is **generated** from R config (§ 5.1) — drift impossible; parity test catches Python regressions. |
| R2 | SQLite write contention under concurrent R+Python use | P1 (WAL + busy_timeout in R) is a hard prerequisite; per-row UPSERT; logged contentions trigger investigation. |
| R3 | Upstream API drift (FishBase / WoRMS shape changes) | Nightly canary tests; `make refresh-canaries` for fixture refresh. |
| R4 | Phylogenetic-imputation algorithm subtly differs from R | Soft parity test (≥ 90 %) + agreement-threshold gate ported faithfully. |
| R5 | Shiny for Python module nesting depth on full feature parity | Smoke app with all module shells + UI screenshot diff in plan-execution Phase 1. |
| R6 | OneDrive sync conflicts on the project repo | Repo committed to git on day 1; `.git/` in OneDrive is *generally* fine (millions use it) but if conflicts surface, user has option to move the working tree out of OneDrive — risk re-evaluated at week 2 of plan. |
| R7 | License/redistribution risk (FishBase CC-BY-NC, freshwaterecology, AlgaeBase ToS) | (a) every CSV/Excel/Parquet export carries per-row source-attribution column; (b) `app/modules/downloads.py` shows a license-acknowledgement modal before first export, with a session-level "don't show again" remembered in browser local storage; (c) **handoff CSVs** at `/srv/econet-shared/handoff/` carry the same attribution column (no UI prompt, but the file itself is documented as restricted); (d) **plotly radar chart** has its toolbar download-as-PNG button replaced with a custom button that triggers the same license modal before allowing the PNG export, and stamps the species names + source list into the image footer; (e) public-facing footer on the deployment carries all source citations + license links; (f) commercial-use disclaimer in README. Tracked separately; legal review prior to public-facing announcement. |
| R8 | R adds a column Python silently can't write | UI banner if known column is missing; structured log at `WARN` level when an unknown column is observed; documentation reminds R team to file an issue when bumping the schema. |
| R9 | A single slow species blocks a 100-species batch | Per-species `asyncio.wait_for(timeout=ECONET_SPECIES_DEADLINE_S)` (default 30 s); deadline rows reported `partial=True`. |
| R10 | `raw_data` BLOB invisibility for Python-written rows degrades R UX | Documented; R UI shows "(no raw data)" for those rows. Trade-off accepted because alternative (Python writes a parallel `raw_data_python` JSON column) requires R-side schema change. |
| R11 | Estimate is optimistic | Honestly stated: 16–18 weeks (with realistic ceiling of 20 weeks) including prerequisites (P1–P4). See § 14.6. |
| R12 | Bus factor 1 | Operations runbook (§ 14) is part of MVP; spec + runbook + README + harmonization-regen R script let any Python developer pick up the project. |
| R13 | `ECONET_ADMIN_TOKEN` is a single shared admin gate (§ 5.5), not per-user — anyone with the URL who knows the token has admin rights | Acceptable for MVP per § 12 ("auth not in MVP scope"); tracked so it's not forgotten when proper auth lands. |
| R14 | RSQLite + aiosqlite WAL semantics may diverge in edge cases (long readers blocking checkpoint, mixed-version SQLite libs) | The cache round-trip parity test (§ 8.2) explicitly exercises a Python-write → R-read → R-write → Python-read sequence with WAL active. Failures would show up there. |
| R15 | Plotly PNG license footer (R7-d) is non-trivial: plotly.js modebar's `toImage` runs client-side; intercepting it requires replacing the modebar button (`config={modeBarButtonsToRemove:['toImage']}` + custom button calling `Plotly.toImage` after stamping an annotation layer) | Allotted 0.5 weeks in § 14.6 estimate; fallback if blocked: ship without footer, defer to v1.1 with a download-modal-only license acknowledgement. |
| R16 | DuckDB community-extension supply chain (a hostile or compromised parquet root could trigger arbitrary code via extension `LOAD`) | Mandatory extension lockdown PRAGMAs at every connection open (§ 8.1); HTTPS-only parquet root URLs (asserted at config-load); extension pre-installed at deploy time, never fetched at runtime. |
| R17 | API-key leakage into logs / tracebacks / cache `raw_data` BLOB | Loguru secret-redaction filter (§ 7); fixed-enum `SourceResult.reason` strings; credentials in `Authorization` headers not query strings; redaction filter unit-tested. |
| R18 | CSV / Excel formula injection on download | Cells starting with `= + - @ \t \r` are prefix-quoted with `'` before serialisation (§ 5.4); fixture row exercises this. |
| R19 | Multi-user rate-limit unfairness (User A's 100-species batch starves User B's 5-species lookup) | `traitresearch/orchestrator.py::FairScheduler`: round-robin scheduler over a per-session deque of pending species; on each per-host token-bucket grant, dequeue from the *next* session in round-robin order, not from the head of a single global queue. Sessions identified by Shiny `session.id`; entries auto-evicted on session disconnect. Sub-design (data structures, starvation guarantees, mid-batch disconnect handling) tracked as a planning task in § 15. UI shows "another batch is running; queue position: N" hint. |
| R20 | Symlink redirection of `harmonization_custom.json` to attacker-chosen path | Startup probe verifies symlink target equals expected absolute path, target file ownership is `root:econet-shared` mode 0664, parent dir not group-writable (§ 7.5 step 11). |
| R21 | Memory growth in long-running uvicorn worker | `MemoryHigh=2G` / `MemoryMax=3G` in systemd unit; weekly `econet-py-restart.timer`; eager-loaded data structures don't grow per-session (§ 7.5 step 9). |
| R22 | DuckDB parquet first-fetch latency (30–120 s) hit on first user request after fresh deploy | `ECONET_PARQUET_PREWARM=true` triggers a background pre-warm in startup probe (§ 7.5 step 8) before first user request. |
| R23 | `MemoryMax=3G` hard SIGKILL mid-batch could lose in-flight species results | `MemoryHigh=2G` soft throttle absorbs most growth; species-batch UI caps at 500 / Run; `Restart=on-failure RestartSec=5s` auto-recovers; partial UPSERTs already persisted are kept by 30-day TTL. Documented in § 10.2. |

## 12. Out of scope (deferred)

- Live REST/IPC integration with R EcoNeTool (file-based hand-off only).
- Bayesian imputation via PyMC (slot reserved in `impute/`; not built MVP).
- Phylogenetic-tree-based methods (`rphylopars` equivalent).
- A CLI entrypoint for headless lookups (trivial follow-up; uses the library directly).
- Observability beyond loguru file logs + journald (no Prometheus/OpenTelemetry).
- Re-port of EcoNeTool's biomass / food-web / metaweb / Rpath modules.
- Public anonymous access (laguna.ku.lt deployment is research-internal; auth not in MVP scope).

## 13. Acceptance criteria for "MVP done"

1. **All 17 lookup sources** (§ 4.5) callable via library; failure of any single source never breaks a batch; SHARK marked deprecated, not removed.
2. **Harmonization parity is hard 100%** on the 30-species fixture: every code (MS/FS/MB/EP/PR/RS/TT/ST) and per-trait source label matches the frozen R fixture. The parity test fails the build on any diff.
3. **Both R EcoNeTool and the Python app** can read each other's cache rows on laguna; `tests/parity/test_cache_roundtrip.py` proves this in CI for non-BLOB columns.
4. **Shiny UI** exposes (matching R EcoNeTool):
   - 3 main tabs (Species & Status / Found Traits / Trait Reference)
   - Configure-API-Keys modal
   - Validate-species-names button
   - Per-species live status icons during lookup
   - 4 stat value-boxes (Found / Complete / Partial / Not Found)
   - Source-badge legend with 13 colored badges
   - Plotly radar chart (8 axes)
   - Raw Trait Details collapsible
   - Harmonization settings panel with **all 8 ecosystem profiles**
   - Imputation panel (3 methods)
   - Offline-DB management (3 stat boxes + rebuild + DT preview)
   - Help modal with full ported `trait_help_content.R` content
   - Downloads: CSV / Excel / RDS-fallback / Parquet
   - Schema-mismatch banner (red) when applicable
   - All 5 test datasets including the hardcoded-list `baltic_top_predators`
   - "Download Reference PDF" + "Open Full Reference Guide" actions (per `R/ui/trait_research_ui.R:574,582`)
5. **Phylogenetic-imputation parity is soft ≥ 90 %** on fixture species (cache-state-dependent vote weights make exact matching brittle); the 10 % gap must have a documented reason.
6. **Performance** (split into four cases; all measured by `tests/perf/test_batch_latency.py`, ran manually + at release):
   - **Warm-fresh-complete** (every species hits cache, no re-fetch): ≤ 5 s for 100 species. Tighter than the previous "90 s" because there's no excuse — pure cache reads should be fast.
   - **Warm-partial** (cache hits but some traits missing → small live re-fetch): ≤ 90 s for 100 species.
   - **Cold-prewarmed** (parquet snapshots already cached on disk; first lookup of these species): ≤ 8 minutes for 100 species. WoRMS @ 2 rps × ~2 calls/sp = 100 s floor; freshwaterecology, AlgaeBase, EOL add another 60–120 s; the budget reflects honest per-host rate-limit floors.
   - **First-ever** (no parquet, no cache): ≤ 12 minutes; one-time path after fresh deploy. The pre-warm probe (§ 7.5 step 8) runs in background so users typically don't observe this.
7. **Observability:** for any species with no traits returned, the loguru log can answer "why" (which sources skipped/failed/empty) within 30 s of grep-ing.
8. **Deployability:** `deployment/bootstrap-laguna.sh` reproduces the laguna deployment from a clean Ubuntu VM in under 30 minutes (verified once at MVP-done).
9. **License/attribution + provenance:** every CSV/Excel/Parquet export *and* every handoff CSV carries (a) the same `#`-prefixed manifest header rows defined in § 5.4 (harmonization version, app git SHA, generation timestamp, license, per-source attribution), (b) a per-row `source` column, (c) per-trait `<MS|FS|…>_cached_at` ISO8601 columns so a reader can see how stale each value was at export time. The download modal shows license acknowledgement on first use per session; the radar-chart PNG export runs through a license-modal-gated custom button (or, if R15 fallback applies, the toolbar export is disabled and PNG is reachable only via the download modal); README has commercial-use disclaimer.

   **Wide-CSV trade-off.** With 8 trait codes + 8 `_imputed` flags + 8 `_source` labels + 8 `_cached_at` timestamps + `species`, `staleness`, `overall_confidence` columns, exports are ~36 columns wide. This is intentional — the provenance trail is *the* point. A `--narrow` export variant (just `species, MS, FS, MB, EP, PR` for legacy R consumers) is a follow-up under § 12 deferred items, not MVP.
10. **Documentation:** README + `docs/operations.md` runbook + `docs/adding_a_source.md` exist and were verified by a colleague-or-Claude doing a dry-run "add a new source" exercise.
11. **BLOB-asymmetry impact:** at least one fixture species has a Python-written cache row, and the R Shiny "Raw Trait Details" panel shows "(no raw data)" without crashing — verified manually at MVP-done.
12. **Imputed-vs-measured visual safety** (the most user-critical criterion):
    - The DT results table cells produced by imputation render with a distinct background color (e.g., amber) and a bold "imputed" subscript.
    - The radar chart renders measured vs imputed values as overlaid traces with hatching on imputed. **When N > 25 species, the default visualisation switches to a heatmap** (8 traits × N species) — the radar is still available via a "show as radar" toggle but is not the default, because 200+ traces saturate the SVG DOM and PNG export blocks the main thread.
    - Every CSV/Excel/Parquet export *and* the Food-Web hand-off CSV carries paired `<trait>_imputed: bool` columns.
    - A unit test loads an export with imputed values and asserts a downstream consumer cannot accidentally ignore the flag (column presence + non-null in every imputed row).
13. **Stale-cache transparency:** a fixture species with `staleness_state = STALE_REFRESH_FAILED` renders an amber clock icon in UI + carries the `staleness` column in exports; verified manually.
14. **Source discoverability:** every source badge has an on-hover tooltip showing one-line description + URL; verified visually.
15. **Onboarding tutorial:** the help modal opens by default to a "Quick Start" tab (3-step ribbon), not the technical reference; first-time visitors can produce a result on a test dataset within 60 s without reading other docs.

## 14. Operations runbook (lives at `docs/operations.md`; outline here)

### 14.1 Day-to-day

- **Restart the service:** `sudo systemctl restart econet-py.service`
- **Tail logs (live):** `journalctl -u econet-py -f` (stdout) + `tail -F /srv/econet-shared/logs/econet-py.log` (file)
- **Inspect cache size:** `sqlite3 /srv/econet-shared/cache/taxonomy.db 'select count(*) from species;'`
- **Force re-lookup of a species:** delete its row in `species` table or wait for 30-day TTL.

### 14.2 When the parity test fails

1. Run `tools/regenerate_parity_fixture.R` on a clean checkout of EcoNeTool at the latest tagged harmonization version.
2. Diff the new `r_harmonization_outputs.json` against the committed fixture.
3. If the diff is *expected* (R config was bumped intentionally), commit the new fixture + bump `harmonization-v<n>`.
4. If unexpected, the harmonization YAML or Python harmonization code has drifted — investigate and fix, don't bump the fixture.

### 14.3 When upstream APIs change

1. `make refresh-canaries` — hits live APIs and overwrites `tests/fixtures/<source>/*.json`.
2. `git diff` to see what changed in the response shape.
3. Update parsers in `lookup/<source>.py`.
4. Re-run unit + parity tests.

### 14.4 When the R team bumps the cache schema

1. Read the R-side migration in `cache_sqlite.R`.
2. Add the new columns to `traitresearch/cache/taxonomy_db.py` (or `offline_db.py`) DAO + the relevant pydantic model in `traitresearch/models/` (`taxonomy_cache.py` or `offline_cache.py`).
3. Update the schema-check expected-columns set.
4. Bump version, redeploy.

### 14.5 Cache backup & restore

**Backup** (automated by the nightly cron set up in § 9.1):

```bash
sqlite3 /srv/econet-shared/cache/taxonomy.db ".backup /srv/econet-shared/backups/taxonomy-$(date +%F).db"
find /srv/econet-shared/backups/ -mtime +14 -delete
```

`offline_traits.db` is regenerable via the rebuild CLI (§ 6.5); not backed up.

**Restore:**

1. Stop both apps: `sudo systemctl stop econet-py && sudo systemctl stop shiny-server` (the latter is the R EcoNeTool).
2. `cp /srv/econet-shared/backups/taxonomy-YYYY-MM-DD.db /srv/econet-shared/cache/taxonomy.db`
3. Start both: `sudo systemctl start shiny-server && sudo systemctl start econet-py`.
4. Run the cache round-trip parity smoke test on a known-cached species (`Gadus morhua`).

If `taxonomy.db` corrupts mid-flight (kernel panic during write), the `-wal` file may be ahead of the main DB; SQLite will recover automatically on next open. If it doesn't (rare, e.g. partial WAL write), restore from the previous nightly backup.

### 14.6 Adding a new lookup source

`docs/adding_a_source.md` walks through:

1. Create `traitresearch/lookup/<name>.py` implementing `LookupSource`.
2. Add an entry to `conf/sources.yaml` (base URL, timeout, rate limit).
3. Add a routing rule (or extend the fallback) in `conf/routing.yaml`.
4. Add a canary fixture under `tests/fixtures/<name>/<species>.json`.
5. Add a unit test using `pytest-httpx`.
6. Update `app/modules/database_select.py` if user-toggleable.
7. Update README.

### 14.7 Estimate (honest)

| Workstream | Weeks |
|------------|-------|
| Prerequisites P1–P4 (R-side WAL + busy_timeout, /srv/econet-shared/, R env-var path, FS/PR reconciliation) | 1.5–2 |
| 17 lookup sources (REST + CSV + XLSX) with rate limit + canaries + edge-case § 4.8 | 3.5–4 |
| Harmonization YAML-gen R script + Python `harmonize/` + parity infra (8 modalities) | 1.5–2 |
| Imputation: phylogenetic port + taxonomy_knn + IterativeImputer + soft parity | 1.5 |
| 16 Shiny modules including impute_panel + radar + schema banner + reference_pdf | 2.5–3 |
| Offline-DB rebuild: CLI `traitresearch.cli.rebuild_offline_db` + UI integration + cancel | 1 |
| Cache layer (taxonomy.db + offline_traits.db, two pydantic models) + WAL + schema check + migration drill + cache-roundtrip parity test | 1.5 |
| Deployment: `bootstrap-laguna.sh` + systemd unit + nginx snippet + service user + secrets file | 1.5 |
| Documentation (operations.md + adding_a_source.md + README + license footer + onboarding tutorial) + manual QA | 1.5 |
| Plotly PNG modebar interception + license footer (R7-d) | 0.5 |
| Excel-paste tolerance + species-input preview + multi-user FairScheduler + secret-redaction filter | 1 |
| Performance hardening: parquet pre-warm, eager-load CSVs, DuckDB lockdown, WAL maintenance + backups, memory caps + restart timer, formula-injection escape | 1 |
| **Total** | **18–20 weeks single dev + Claude** |

This is honest. The earlier 6–8 weeks figure underweighted the parity infrastructure, the 9 missed sources, the 3 missed harmonize modules, the second SQLite database, and deployment-environment work. The v3/v4 estimate added line items for the offline-DB rebuild CLI, cache round-trip test, P4 (FS/PR reconciliation), and plotly modebar interception. **The v6 estimate adds rows for the UX/security/performance findings from the round-5 different-angle review** — these aren't gold-plating; they're things that would otherwise surface in production as silent bugs (predictions published as observations, secret leakage, supply-chain risk, stale-cache invisibility, OOM kills, rate-limit unfairness). **Realistic ceiling, given the FishBase parquet/duckdb pivot, possible R-side bugs surfaced during P4, and any unforeseen API drift: 22 weeks.**

## 15. Notes for the implementation plan (not part of the design)

When this spec is fed into `superpowers:writing-plans`, the planner should know:

- **P-work is parallel-able with Python scaffolding.** Day 1 stakeholder asks: P2 (laguna sudo for `/srv/econet-shared/` setup) and P4 (FS/PR canonical-label decision). P1 (R WAL pragma) and P3 (R env-var path migration) can be picked up in week 2 in parallel with scaffolding.
- **Tombstone:** do not begin `harmonize/*.py` until the parity fixture is committed. Until then, `harmonize/` carries `raise NotImplementedError` stubs + unit tests against synthetic inputs.
- **Cache layer first.** Build `TaxonomyCacheEntry` + `OfflineCacheEntry` models, schema_check, DAOs, before the first source. Acceptance: synthetic `LookupResult` round-trips.
- **FishBase DuckDB spike in Week 1.** From laguna, run `duckdb -c "DESCRIBE 'https://<rfishbase-parquet-root>/species.parquet'"`. If it fails (egress filtering), pivot to bundled-parquet sync before designing `lookup/fishbase.py`.
- **Thin slice at Week 4** (the first demo): `Gadus morhua` → WoRMS taxonomy → `lookup/ontology.py` (CSV) → `harmonize/size.py` only → cache write → `app/modules/results_table.py` displays one row. ~5 of 17 sources unimplemented; ~13 of 16 modules stubbed.
- **Module build tiers** (build standalone first, cache-fed second, live-flow last):
  1. **Standalone:** `help_modal`, `trait_reference`, `reference_pdf`, `api_keys_modal`, `harmonization_settings`.
  2. **Cache-fed:** `results_table`, `results_radar`, `raw_details`, `offline_db`.
  3. **Live-flow:** `species_input`, `lookup_progress`, `impute_panel`, `downloads`, `database_select`, `schema_banner`.
- **DSL before sources that use it.** The `applies_when:` predicate engine + `Taxonomy.compute_flags()` ships before any source that needs them; sources without predicates (worms_attrs, fishbase) can ship earlier.
- **CI staging.** `lint`, `arch-check`, `test-fast` blocking from week 1. Parity tests `@pytest.mark.xfail(strict=False)` until ≈ week 8 (when the R fixture lands).
- **`FairScheduler` sub-design** (named in § 11 R19, not detailed). Pre-coding, the planner spends 0.5 day producing a one-page contract: per-session deque, eviction policy on `session.on_ended()`, interaction with per-host token bucket (a session whose next species needs a host that's exhausted is *skipped*, not *blocked*, so other sessions still progress), starvation lower bound (worst-case latency for a 1-species lookup behind an active 500-species batch), telemetry. Add to the implementation plan as a Phase-1 design task.
- **XLSX/CSV mutability invariant.** Eager-loaded polars DataFrames on `Lifecycle` are treated as **immutable**; sources call `.filter()`, `.join()`, `.select()` (which return new frames) but never `.with_columns(in_place=True)` or any in-place mutator. Enforced by code review.
- **Open decisions the planner must make before coding starts** (1-day decision-record sprint):
  1. asyncio fan-out primitive: `asyncio.wait`+`gather` (spec) vs `TaskGroup` (3.11+).
  2. Pydantic settings: `BaseSettings` per-module vs single `Settings` singleton.
  3. Test fixture format: JSON-on-disk vs `pytest.parametrize`.
  4. Rate-limit token bucket: in-process vs `aiolimiter`.
  5. Subprocess spawn for offline-DB: argv (spec) vs entry-point script.
  6. Module DI pattern: pass `Lifecycle` everywhere vs `contextvars`.
  7. DuckDB connection scope: per-request vs per-source vs process-global.
  8. Schema-check cache invalidation: SIGHUP only vs admin endpoint vs file-watch.
- **Implicit scaffolding (~3 days week-1 work):** `pyproject.toml` with hatchling, ruff/mypy config, pre-commit hooks, pytest skeleton, `arch-check.sh`, `Makefile` (`make refresh-canaries`), `.env.example`, GitHub Actions YAML for the 5 CI shapes, `conftest.py` with `Lifecycle` reset fixture, R 4.4 install on CI runners.
- **Demo milestones** (declare these as gate reviews):
  - **Week 4:** thin slice live (1 sp, 1 source, 1 modality, 1 module).
  - **Week 8:** 5 sources, 4 modalities, 6 modules, harmonization-parity green for 5 species.
  - **Week 12:** 17 sources, all 8 modalities, all 16 modules, parity 30-species green, imputation working locally.
  - **Week 18 (was 16):** deployed to laguna, R/Python share cache, food-web hand-off verified, docs done. **MVP.**
- **Critical path:** P4 → regen YAML+fixture → harmonize/ → first source → cache write → results_table → Phase-1 demo → broaden sources → parity green → imputation → polish → deploy. ~10 serial gates. Parallelism: source ports (after the first), standalone Shiny modules, docs, deployment scripts.

---

*End of design.*
