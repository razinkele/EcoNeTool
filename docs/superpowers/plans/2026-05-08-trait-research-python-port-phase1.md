# Trait Research Python Port — Phase 1: Bootstrap, Models, Cache, DuckDB Spike

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bootstrap the `EcoNeTool-py/` Python project, define every pydantic model the rest of the project will use, build the cache layer (both SQLite databases) so synthetic LookupResults round-trip cleanly, and verify FishBase parquet connectivity from laguna.ku.lt before any source port is written. By end of Phase 1, CI is green, the library is importable from a notebook, and any subsequent phase can build on a stable foundation.

**Architecture:** Pure-Python library `traitresearch/` with zero `shiny` imports. Pydantic v2 models split by responsibility (taxonomy, traits, two cache schemas). aiosqlite for cache I/O with explicit parameter binding and column allow-listing. A `Lifecycle` class holds shared state (no module globals). DuckDB connectivity to FishBase parquet root is verified by an integration test marked `@pytest.mark.network`.

**Tech Stack:** Python 3.11+, pydantic v2.7+, aiosqlite 0.20+, duckdb 0.10+, polars 1.0+, loguru 0.7+, pytest 8+ + pytest-asyncio + pytest-httpx, ruff 0.5+, mypy 1.10+, hatchling 1.25+ (build backend), pre-commit. Micromamba env: `shiny` (per user's global CLAUDE.md).

**Reference spec:** `…/EcoNeTool/docs/superpowers/specs/2026-05-08-trait-research-python-port-design.md` — every section number cited below refers to this file.

**External prerequisites (R-side, NOT in Phase 1):** Spec § 2.5 lists P1–P4 (R WAL pragma, `/srv/econet-shared/` setup, R env-var path migration, FS/PR reconciliation). These do NOT block Phase 1 — Phase 1 only writes to local `tmp_path` test DBs and never touches the shared cache. P1–P3 land before Phase 9 (deployment); P4 must land before Phase 2 (harmonization parity).

**Shell convention:** all shell snippets in this plan are written for **Git Bash on Windows** or **bash on Linux/macOS** (matches the user's existing EcoNeTool deployment scripts and CLAUDE.md `Bash(...)` permissions). On Windows users running native PowerShell, switch to Git Bash before running any Task 1 / arch-check / pre-commit step. The `Bash` tool in Claude Code routes through Git Bash automatically. **Every multi-command bash block should be preceded by `set -euo pipefail`** (failure-recovery review of v3) so silent partial failures become hard failures.

## Recovery playbook

The plan's happy path assumes commits succeed, the micromamba env exists, and prior tasks completed cleanly. When that's not the case, follow these recipes BEFORE escalating:

| Failure mode | Recovery |
|--------------|----------|
| `git commit` fails with "ruff modified files" (autofix) | `git add -u && git commit -m "..."` (re-stage + retry). Don't use `--no-verify`. |
| `git commit` fails with "mypy --strict" type error | Fix the annotation in the same task's file before re-running the commit. Never `# type: ignore` to silence. |
| `git commit` blocked by `check-added-large-files` | Inspect `git diff --cached --stat` for unexpected large file. Move it out of the staged area; rerun `.gitignore` or move it under `data/cache/` (gitignored). |
| `pip install -e ".[dev]"` resolver conflict | `micromamba list \| grep -E '(scikit-learn\|polars\|pydantic)'` to inspect existing pins. If a pinned-floor in `pyproject.toml` is genuinely incompatible with the env, bump *upward* (never downward) and re-run; do NOT widen ranges to paper over conflicts. |
| `micromamba: environment 'shiny' not found` | Run `micromamba create -n shiny -c conda-forge python=3.11 pip` then re-run `pip install -e ".[dev]"`. Bootstrapping a fresh env is documented in `README.md` (Task 1 Step 4). |
| Test "should fail" passes (red step misses) | The test assertion is too weak. Strengthen it before implementing — never proceed to green on a green-only red. |
| Test "should pass" fails with a *different* error than expected | Don't paper over: another module regressed. Run `pytest tests/unit -v --tb=short` and find the unrelated failure first. |
| Mid-task abort + restart | `git status` first. If the working tree is clean, the previous run committed; pick up at the next task. If dirty, the previous run partially completed — inspect `git diff` and either commit (if the diff matches the task's intent) or `git restore .` to reset. |
| `git tag phase1-complete` already exists | `git rev-parse phase1-complete >/dev/null 2>&1 && git tag -d phase1-complete; git tag -a phase1-complete -m "..."`. |
| `pre-commit install` fails (offline) | Defer the install commit; tests still work without hooks. Re-run `pre-commit install` next time online. |
| OneDrive sync grabs `.git/index.lock` mid-execution | Right-click `EcoNeTool-py/` in File Explorer → "Always keep on this device" once at bootstrap. Pause OneDrive sync if `git add` repeatedly fails with `Unable to create … index.lock`. |

These also belong in `docs/operations.md` (full Phase 10) but are surfaced here so Phase 1 isn't blocked by ops-doc absence.

**Phase 1 acceptance gates:**

1. `EcoNeTool-py/` exists as its own git repo (sibling of `EcoNeTool/`) with the spec copied into `docs/superpowers/specs/`.
2. `micromamba run -n shiny python -c "import traitresearch; print(traitresearch.__version__)"` works.
3. `pytest tests/unit -v` is fully green; `test_cache_roundtrip.py` round-trips a synthetic `LookupResult` through both DBs.
4. `pytest tests/unit/test_upsert_safety.py` proves UPSERTs use parameter binding only — no f-string-interpolated values.
5. `pytest -m network tests/integration/test_duckdb_spike.py` connects to the FishBase parquet root and runs `DESCRIBE` (run manually; not blocking CI).
6. CI (lint + arch-check + test-fast) green on `main`.
7. Phase 1 plan committed under `docs/superpowers/plans/`.

---

## Task Index

For dispatching subagents and tracking review velocity. Each row is one self-contained commit.

| # | Task | Files (primary) | Commit message |
|---|------|----------------|----------------|
| 1 | Bootstrap repo + spec copy | `.gitignore`, `.gitattributes`, `README.md`, `docs/` | `chore: bootstrap EcoNeTool-py repo …` |
| 2 | `pyproject.toml` + deps | `pyproject.toml`, `.python-version` | `chore: add pyproject.toml with pinned deps …` |
| 3 | Pre-commit hooks | `.pre-commit-config.yaml` | `chore: add pre-commit hooks …` |
| 4 | Package skeleton | `traitresearch/__init__.py` (+sub-pkgs) | `feat: scaffold traitresearch package …` |
| 5 | Arch-check + CI | `tools/arch-check.sh`, `.github/workflows/*.yml` | `ci: add lint, arch-check, test-fast workflows` |
| 5b | Contributing guide | `docs/contributing.md` | `docs: add contributing guide …` |
| 5c | Decision record | `docs/decisions/2026-05-08-phase1-decisions.md` | `docs: record Phase 1 design decisions …` |
| 6 | `Species` + `Taxonomy` | `models/taxonomy.py` + tests | `feat(models): add Species and Taxonomy …` |
| 7 | Enums + `TraitObservation` + `RawTraits` | `models/traits.py` + tests | `feat(models): add FetchStatus, StalenessState, TraitObservation, RawTraits` |
| 8 | `HarmonizedTraits` + `SourceResult` + `LookupResult` | `models/traits.py` (extended) + tests | `feat(models): add HarmonizedTraits, SourceResult, LookupResult …` |
| 9 | `TaxonomyCacheEntry` | `models/taxonomy_cache.py` + tests | `feat(models): add TaxonomyCacheEntry …` |
| 10 | `OfflineCacheEntry` | `models/offline_cache.py` + tests | `feat(models): add OfflineCacheEntry …` |
| 11 | `Settings` (env-var loader) | `settings.py` + tests | `feat(settings): add Settings model …` |
| 12 | `secrets.py` (API-key resolver) | `secrets.py` + tests | `feat(secrets): add env-var-only API-key resolver …` |
| 13 | `Lifecycle` class | `lifecycle.py` + tests | `feat(lifecycle): add Lifecycle class …` |
| 14 | `cache/connection.py` | `cache/connection.py` + tests | `feat(cache): add open_db with WAL …` |
| 15 | `cache/schema_check.py` | `cache/schema_check.py` + tests | `feat(cache): add schema introspection …` |
| 16 | `cache/taxonomy_db.py` (DAO) | `cache/taxonomy_db.py` + tests | `feat(cache): add taxonomy_db DAO …` |
| 17 | `cache/offline_db.py` (DAO) | `cache/offline_db.py` + tests | `feat(cache): add offline_db DAO …` |
| 17.5 | DDL-vs-models invariant tool | `tools/check_ddl_vs_models.py` | `tools: add DDL-vs-models invariant checker …` |
| 18 | UPSERT safety invariants | `tests/unit/test_upsert_safety.py` | `test: add UPSERT safety invariants …` |
| 19 | Cache round-trip tests | `tests/unit/test_cache_roundtrip.py` | `test(cache): add Python-only round-trip …` |
| 20 | Public API re-exports | `models/__init__.py` + tests | `feat(models): re-export public API …` |
| 21 | `duckdb_client.py` | `duckdb_client.py` + tests | `feat(duckdb): add open_duckdb wrapper …` |
| 22 | DuckDB spike (network) | `tests/integration/test_duckdb_spike.py` | `test(integration): add FishBase parquet … spike` |
| 23 | mypy --strict gate | (no new files; conditional fixes) | `chore: address mypy --strict findings` (if any) |
| 24 | ruff gate | (no new files; conditional fixes) | `chore: ruff autofix` (if any) |
| 25 | Phase 1 acceptance gate + tag | (none; tag only) | `chore: phase 1 acceptance gate` (if any) |

Total: 25 numbered tasks + 5b/5c (docs) + 17.5 (tools) = 28 commits maximum (some conditional). When dispatching a subagent for Task N, scan its row above to know what to expect in the diff.

**Prerequisites between tasks:** later tasks generally depend on earlier ones (models in 6–10 must exist before cache 14–17, lifecycle 13 before cache, cache before integration tests 18–19, etc.). When dispatching out of order is unavoidable, run `pytest tests/unit -v --collect-only` first to surface missing imports.

---

## File Structure

After Phase 1 completes, the new repo looks like:

```
EcoNeTool-py/
├── pyproject.toml                  hatchling backend; pinned versions per spec § 8.1
├── README.md                       skeleton
├── .gitignore                      Python defaults + .venv + cache
├── .pre-commit-config.yaml
├── .python-version                 3.11
├── ruff.toml                       lint + format config
├── conftest.py                     root pytest config (asyncio mode, network marker)
├── docs/
│   └── superpowers/
│       ├── specs/2026-05-08-trait-research-python-port-design.md  (copied)
│       └── plans/2026-05-08-trait-research-python-port-phase1.md  (this file, moved)
├── .github/workflows/
│   ├── lint.yml                    ruff + mypy
│   ├── arch-check.yml              shell script enforcing architectural rules
│   └── test-fast.yml               pytest unit + parity (skipped: not yet)
├── tools/
│   └── arch-check.sh               grep-based no-shiny-in-traitresearch check
├── traitresearch/
│   ├── __init__.py                 __version__ = "0.1.0"
│   ├── settings.py                 pydantic BaseSettings; env-var loader
│   ├── secrets.py                  API-key resolver (returns None if missing)
│   ├── lifecycle.py                Lifecycle class
│   ├── duckdb_client.py            DuckDB connection wrapper with extension lockdown
│   ├── models/
│   │   ├── __init__.py             public re-exports
│   │   ├── taxonomy.py             Species, Taxonomy
│   │   ├── traits.py               FetchStatus, StalenessState, TraitObservation,
│   │   │                            RawTraits, HarmonizedTraits, SourceResult, LookupResult
│   │   ├── taxonomy_cache.py       TaxonomyCacheEntry (mirrors spec § 9.2)
│   │   └── offline_cache.py        OfflineCacheEntry (extends per spec § 9.3)
│   └── cache/
│       ├── __init__.py
│       ├── connection.py           open_db() with PRAGMA setup
│       ├── schema_check.py         introspect PRAGMA table_info; cache result
│       ├── taxonomy_db.py          async DAO for taxonomy.db.species
│       └── offline_db.py           async DAO for offline_traits.db.species_traits
└── tests/
    ├── __init__.py
    ├── conftest.py                 fixtures: tmp_lifecycle, tmp_taxonomy_db, tmp_offline_db
    ├── unit/
    │   ├── test_models.py          serialise/round-trip, field defaults
    │   ├── test_settings.py        env-var loading, defaults, invalid paths
    │   ├── test_lifecycle.py       reset_for_tests behaviour
    │   ├── test_cache_schema.py    PRAGMA introspection vs model fields
    │   ├── test_cache_roundtrip.py synthetic LookupResult through both DBs
    │   └── test_upsert_safety.py   parameter binding invariants
    ├── integration/
    │   └── test_duckdb_spike.py    @pytest.mark.network, DESCRIBE FishBase parquet
    └── fixtures/
        └── synthetic_lookupresult.json
```

---

## Section A — Project bootstrap (7 tasks: 1, 2, 3, 4, 5, 5b, 5c)

### Task 1: Create EcoNeTool-py repo and copy the spec

**Deliverable:** A new sibling git repo at `…/Networks/EcoNeTool-py/` with `.gitignore`, `.gitattributes`, `README.md`, and the spec + Phase 1 plan moved into `docs/superpowers/`.

**Files:**
- Create: `EcoNeTool-py/` (new git repo, sibling of `EcoNeTool/`)
- Create: `EcoNeTool-py/.gitignore`
- Create: `EcoNeTool-py/.gitattributes`
- Create: `EcoNeTool-py/README.md`
- Copy: `EcoNeTool/docs/superpowers/specs/2026-05-08-trait-research-python-port-design.md` → `EcoNeTool-py/docs/superpowers/specs/`
- Move: `EcoNeTool/docs/superpowers/plans/2026-05-08-trait-research-python-port-phase1.md` → `EcoNeTool-py/docs/superpowers/plans/`

- [ ] **Step 0: Pre-flight environment checks.**

The plan assumes (a) a pre-existing `shiny` micromamba env per CLAUDE.md, (b) `git` is configured with a global user.email, (c) the EcoNeTool repo exists at the expected sibling path, (d) on Windows the `EcoNeTool-py` folder isn't subject to OneDrive sync races mid-execution. Verify all four:

```bash
set -euo pipefail

# (a) env exists
micromamba env list | awk '{print $1}' | grep -qx 'shiny' || {
    echo "ERROR: micromamba env 'shiny' not found." >&2
    echo "       Create it: micromamba create -n shiny -c conda-forge python=3.11 pip" >&2
    exit 1
}

# (b) git identity is set globally (otherwise empty-author commits)
EMAIL="$(git config --global user.email || true)"
[ -n "$EMAIL" ] || {
    echo "ERROR: git global user.email is unset. Run: git config --global user.email '...'" >&2
    exit 1
}

# (c) sibling EcoNeTool repo exists (the Phase 0 ref source)
PARENT='C:/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks'
[ -d "$PARENT/EcoNeTool/.git" ] || {
    echo "ERROR: $PARENT/EcoNeTool is not a git repo." >&2
    exit 1
}

# (d) detect re-run scenarios: if EcoNeTool-py/ already exists with commits,
# this is a partial restart — pick up at the next task that has uncommitted
# changes per `git status`. Don't blindly re-execute Step 1 (mkdir is fine,
# but Step 5 mv will fail and Step 6 commit will say "nothing to commit").
if [ -d "$PARENT/EcoNeTool-py/.git" ]; then
    echo "NOTE: $PARENT/EcoNeTool-py already exists; this is a re-run."
    echo "      Run 'git -C $PARENT/EcoNeTool-py status' to see where to resume."
    echo "      Task 1 is idempotent at the directory level but will skip the"
    echo "      mv/cp steps that have already happened."
fi
```

**On Windows only:** right-click the OneDrive system-tray icon → **Pause syncing → 2 hours** before continuing. OneDrive can grab `.git/index.lock` mid-commit, producing intermittent `Unable to create … index.lock` errors. Resume sync after Task 25.

> **Note on `$PARENT`:** the path above is the user's specific OneDrive location. If you're running this plan on a different machine, replace it with the parent directory of YOUR EcoNeTool checkout (`PARENT="$(cd "$(dirname "$(realpath ../EcoNeTool)")" && pwd)"` if you're inside the EcoNeTool repo). The variable's only requirement is that `$PARENT/EcoNeTool` is the existing repo.

- [ ] **Step 1: Create the directory tree.**

```bash
set -euo pipefail
PARENT='C:/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks'
mkdir -p "$PARENT/EcoNeTool-py/docs/superpowers/specs"
mkdir -p "$PARENT/EcoNeTool-py/docs/superpowers/plans"
```

- [ ] **Step 2: Initialise git, copy author identity from EcoNeTool.**

```bash
set -euo pipefail
PARENT='C:/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks'

cd "$PARENT/EcoNeTool-py"
git init

# Read author from the existing EcoNeTool repo using an absolute path (more
# robust than a relative `../EcoNeTool` that depends on cwd).
EMAIL="$(git -C "$PARENT/EcoNeTool" config user.email)"
NAME="$(git -C "$PARENT/EcoNeTool" config user.name)"
[ -n "$EMAIL" ] || { echo "ERROR: EcoNeTool has no user.email"; exit 1; }
[ -n "$NAME"  ] || { echo "ERROR: EcoNeTool has no user.name";  exit 1; }
git config user.email "$EMAIL"
git config user.name  "$NAME"
```

- [ ] **Step 3: Write `.gitignore`.**

```
# Python
__pycache__/
*.py[cod]
*$py.class
.pytest_cache/
.mypy_cache/
.ruff_cache/

# Virtualenvs (we use micromamba, but defend anyway)
.venv/
venv/
env/

# Build artifacts
build/
dist/
*.egg-info/

# Cache & data (never commit DBs or parquet snapshots)
*.db
*.db-wal
*.db-shm
data/cache/
data/fishbase/
data/sealifebase/
*.parquet

# IDE
.idea/
.vscode/
*.swp

# OS
.DS_Store
Thumbs.db

# Secrets
.env
.env.local
secrets.env
/etc/econet-py/

# Logs
*.log
```

Then write `.gitattributes` next to it (avoids LF↔CRLF noise on Windows + Linux CI):

```
# Default: text files use LF in the repo regardless of host OS
* text=auto eol=lf

# Shell scripts and Python source must be LF (some shells reject CRLF on shebang)
*.sh    text eol=lf
*.py    text eol=lf
*.toml  text eol=lf
*.yaml  text eol=lf
*.yml   text eol=lf
*.json  text eol=lf

# Windows-only: keep CRLF on .bat/.ps1 (cmd/PowerShell expect it)
*.bat   text eol=crlf
*.ps1   text eol=crlf

# Binary data — never line-end-converted
*.db    binary
*.parquet binary
*.xlsx  binary
```

- [ ] **Step 4: Write a skeleton `README.md`.**

```markdown
# EcoNeTool-py

Shiny for Python port of the trait-research feature of [EcoNeTool](../EcoNeTool/).
Marine ecology trait lookup from species names → harmonised codes (MS, FS, MB, EP, PR, RS, TT, ST).

**Status:** Phase 1 of 10 (bootstrap). Not usable yet.

## Setup

```bash
micromamba activate shiny
pip install -e ".[dev]"
pre-commit install
```

## Tests

```bash
pytest tests/unit -v
pytest -m network tests/integration -v   # requires net access
```

See `docs/superpowers/specs/2026-05-08-trait-research-python-port-design.md` for the full design.
```

- [ ] **Step 5: Copy the spec and move this plan.**

```bash
cp "$PARENT/EcoNeTool/docs/superpowers/specs/2026-05-08-trait-research-python-port-design.md" \
   "$PARENT/EcoNeTool-py/docs/superpowers/specs/"
mv "$PARENT/EcoNeTool/docs/superpowers/plans/2026-05-08-trait-research-python-port-phase1.md" \
   "$PARENT/EcoNeTool-py/docs/superpowers/plans/"
```

- [ ] **Step 6: Initial commit.**

```bash
set -euo pipefail
PARENT='C:/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks'
cd "$PARENT/EcoNeTool-py"
git add .gitignore .gitattributes README.md docs/
git commit -m "chore: bootstrap EcoNeTool-py repo with copied spec and Phase 1 plan"
```

---

### Task 2: Add `pyproject.toml` with pinned dependencies

**Files:**
- Create: `EcoNeTool-py/pyproject.toml`
- Create: `EcoNeTool-py/.python-version`

- [ ] **Step 1: Write `.python-version`.**

```
3.11
```

- [ ] **Step 2: Write `pyproject.toml`.**

```toml
[build-system]
requires = ["hatchling>=1.25"]
build-backend = "hatchling.build"

[project]
name = "traitresearch"
version = "0.1.0"
description = "Shiny for Python port of EcoNeTool trait research"
readme = "README.md"
requires-python = ">=3.11"
authors = [{name = "A. Razinkovas-Baziukas"}]
license = {text = "MIT"}
dependencies = [
    "shiny>=1.0,<2",
    "httpx[http2]>=0.27,<0.29",
    "aiosqlite>=0.20",
    "pydantic>=2.7,<3",
    "pydantic-settings>=2.0",
    "scikit-learn>=1.4,<2",
    "polars>=1.0",
    "fastexcel>=0.10",
    "duckdb>=0.10",
    "loguru>=0.7",
    "uvicorn[standard]>=0.30",
    "charset-normalizer>=3.0",
]

[project.optional-dependencies]
# Upper bounds on dev tools track the pre-commit hook revs in
# .pre-commit-config.yaml (ruff v0.5.0, mypy v1.10.0, pre-commit hooks v4.6.0).
# This prevents a `pip install -U` drift between the dev CLI and the hook layer.
dev = [
    "pytest>=8,<9",
    "pytest-asyncio>=0.23,<1",
    "pytest-httpx>=0.30,<1",
    "ruff>=0.5,<0.6",
    "mypy>=1.10,<1.12",
    "pre-commit>=3.7,<5",
]

[tool.hatch.build.targets.wheel]
packages = ["traitresearch"]

[tool.pytest.ini_options]
asyncio_mode = "auto"
markers = [
    "network: tests that hit live external APIs (skip in default CI)",
    "parity: cross-language R-vs-Python parity tests",
]
addopts = "-m 'not network'"
testpaths = ["tests"]

[tool.mypy]
strict = true
plugins = ["pydantic.mypy"]
python_version = "3.11"

[[tool.mypy.overrides]]
module = ["aiosqlite.*", "duckdb.*", "polars.*"]
ignore_missing_imports = true

[tool.ruff]
line-length = 100
target-version = "py311"

[tool.ruff.lint]
select = ["E", "F", "W", "I", "B", "UP", "ASYNC", "RUF"]
ignore = ["E501"]   # line-length policed by formatter

[tool.ruff.format]
quote-style = "double"
```

- [ ] **Step 3: Install in editable mode and verify.**

> **Windows + OneDrive note:** `pip install -e .` writes a `.pth` file in the env's site-packages that points back at this directory. If OneDrive marks any file in `EcoNeTool-py/` "online-only" (cloud-only), the editable install can break with a phantom `ModuleNotFoundError`. Right-click `EcoNeTool-py/` in File Explorer → **Always keep on this device** to keep the project pinned locally.

```bash
set -euo pipefail
micromamba run -n shiny pip install -e ".[dev]"
# Following line MUST print ModuleNotFoundError; the package has no __init__.py yet.
micromamba run -n shiny python -c "import traitresearch" 2>&1 | grep -q "ModuleNotFoundError" && echo "OK: import fails as expected (Task 4 will fix)" || { echo "ERROR: import unexpectedly succeeded"; exit 1; }
```

- [ ] **Step 4: Commit.**

```bash
git add pyproject.toml .python-version
git commit -m "chore: add pyproject.toml with pinned deps per spec § 8.1"
```

---

### Task 3: Add ruff config, pre-commit hooks, mypy entrypoint

**Files:**
- Create: `EcoNeTool-py/ruff.toml`  *(actually the config is in pyproject.toml — keep `ruff.toml` empty if duplication is unwanted; this task verifies what's already there)*
- Create: `EcoNeTool-py/.pre-commit-config.yaml`

- [ ] **Step 1: Write `.pre-commit-config.yaml`.**

```yaml
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.5.0
    hooks:
      - id: ruff
        args: [--fix, --exit-non-zero-on-fix]
      - id: ruff-format

  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.10.0
    hooks:
      - id: mypy
        additional_dependencies:
          - pydantic>=2.7
          - pydantic-settings>=2.0
        args: [--strict, --ignore-missing-imports]
        files: ^traitresearch/

  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.6.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer
      - id: check-yaml
      - id: check-toml
      - id: check-added-large-files
        args: [--maxkb=500]
```

- [ ] **Step 2: Install hooks.**

```bash
micromamba run -n shiny pre-commit install
```

- [ ] **Step 3: Run hooks on existing files.**

```bash
micromamba run -n shiny pre-commit run --all-files
```

Expected: pass (no Python files yet beyond an empty package).

- [ ] **Step 4: Commit.**

```bash
git add .pre-commit-config.yaml
git commit -m "chore: add pre-commit hooks (ruff, mypy, file-cleanup)"
```

---

### Task 4: Create the `traitresearch` package skeleton

**Files:**
- Create: `EcoNeTool-py/traitresearch/__init__.py`
- Create: `EcoNeTool-py/traitresearch/models/__init__.py`
- Create: `EcoNeTool-py/traitresearch/cache/__init__.py`

- [ ] **Step 1: Write `traitresearch/__init__.py`.**

```python
"""EcoNeTool-py — trait research library and Shiny app.

Pure-Python library; zero `shiny` imports allowed in this package.
See docs/superpowers/specs/2026-05-08-trait-research-python-port-design.md § 3.
"""

__version__ = "0.1.0"
```

- [ ] **Step 2: Write `traitresearch/models/__init__.py`.**

```python
"""Pydantic v2 models. Split by responsibility (taxonomy, traits, cache schemas)."""
```

- [ ] **Step 3: Write `traitresearch/cache/__init__.py`.**

```python
"""Cache layer — taxonomy.db (runtime) and offline_traits.db (rebuild target).

See spec § 9 for schemas and concurrency model.
"""
```

- [ ] **Step 4: Verify import.**

```bash
micromamba run -n shiny python -c "import traitresearch; print(traitresearch.__version__)"
```

Expected: `0.1.0`.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/
git commit -m "feat: scaffold traitresearch package with __init__ files"
```

---

### Task 5: Add architecture-check script and CI workflows

**Files:**
- Create: `EcoNeTool-py/tools/arch-check.sh`
- Create: `EcoNeTool-py/.github/workflows/lint.yml`
- Create: `EcoNeTool-py/.github/workflows/arch-check.yml`
- Create: `EcoNeTool-py/.github/workflows/test-fast.yml`

- [ ] **Step 1: Write `tools/arch-check.sh`.**

```bash
#!/usr/bin/env bash
# Enforce the two architectural rules from spec § 3:
#   1. traitresearch/ has zero shiny imports
#   2. app/ modules don't contain numpy/sklearn/pandas algorithm code
# Exit non-zero on violation.

set -euo pipefail

fail=0

# Rule 1: no shiny in traitresearch/
if grep -rE "^(import|from) shiny" traitresearch/ tests/unit tests/integration tests/parity 2>/dev/null; then
    echo "ARCH VIOLATION: 'shiny' imported inside traitresearch/ or tests/" >&2
    fail=1
fi

# Rule 2 (only checked once app/ exists in Phase 6+):
if [ -d app/modules ]; then
    if grep -rE "^(import|from) (numpy|sklearn|pandas)" app/modules/ 2>/dev/null; then
        echo "ARCH VIOLATION: numpy/sklearn/pandas imported in app/modules/ — keep algorithm code in traitresearch/" >&2
        fail=1
    fi
fi

if [ $fail -ne 0 ]; then
    exit 1
fi
echo "arch-check OK"
```

- [ ] **Step 2: Make it executable and run it.**

```bash
chmod +x tools/arch-check.sh
./tools/arch-check.sh
```

Expected output: `arch-check OK`.

- [ ] **Step 3: Write `.github/workflows/lint.yml`.**

```yaml
name: lint
on: [push, pull_request]
jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with: { python-version: "3.11" }
      - run: pip install -e ".[dev]"
      - run: ruff check .
      - run: ruff format --check .
      - run: mypy --strict traitresearch/
```

- [ ] **Step 4: Write `.github/workflows/arch-check.yml`.**

```yaml
name: arch-check
on: [push, pull_request]
jobs:
  arch:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: ./tools/arch-check.sh
```

- [ ] **Step 5: Write `.github/workflows/test-fast.yml`.**

```yaml
name: test-fast
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with: { python-version: "3.11" }
      - run: pip install -e ".[dev]"
      - run: pytest tests/unit -v
        # 'parity' marker tests run once Phase 2 lands; not required here
```

- [ ] **Step 6: Commit.**

```bash
git add tools/ .github/
git commit -m "ci: add lint, arch-check, test-fast workflows"
```

---

### Task 5b: Add `docs/contributing.md` with the spec § 8.5 forbidden-patterns checklist

**Files:**
- Create: `EcoNeTool-py/docs/contributing.md`

- [ ] **Step 1: Write the file.**

```markdown
# Contributing to EcoNeTool-py

## Forbidden patterns (CI rejects)

These rules are enforced by `ruff`, `mypy --strict`, and code review. They
implement spec § 8.5.

### Pydantic v2 — never use these v1 patterns:

- `class Config:` — use `model_config = ConfigDict(...)`
- `@validator` — use `@field_validator` / `@model_validator`
- `.dict()` / `.json()` — use `.model_dump()` / `.model_dump_json()`
- `parse_obj` — use `model_validate`
- `Field(..., regex=)` — use `Field(..., pattern=)`
- `allow_population_by_field_name` — use `populate_by_name=True`
- `Model.__fields__` — use `Model.model_fields`

### Asyncio — never use these patterns inside Shiny reactives:

- `asyncio.run(...)` — you're already in a running loop; use `await`.
- `loop.run_until_complete(...)` — same reason.
- `asyncio.gather(...)` without `return_exceptions=True` for fan-out work
  that should fail-soft (one source's failure must not abort the gather).

### Architecture rules (enforced by `tools/arch-check.sh`):

1. `traitresearch/` has zero `shiny` imports. The library is consumable from a
   notebook, CLI, or future FastAPI service.
2. `app/modules/*` may not contain `numpy`/`sklearn`/`pandas` algorithm code.
   UI modules render controls and delegate to `traitresearch/`.

### SQL invariants (spec § 9.5):

1. Column identifiers in UPSERTs come ONLY from `set(model.model_fields) ∩
   set(PRAGMA table_info)`. Never user input.
2. Values are bound with positional `?` parameters via
   `aiosqlite.execute(sql, params)`. **No f-string-interpolated values, ever.**
3. All harmonised TEXT values are length-capped at 256 chars and have control
   characters stripped at the model boundary (validators on TaxonomyCacheEntry
   and OfflineCacheEntry).

### Logging (spec § 7):

- Use `loguru`; never bare `print(...)`.
- Pass credentials in `Authorization` headers, never in URL query strings.
- The secret-redaction filter (Phase 8) strips known secret values from log
  output, but don't rely on it — never log a request URL with `?key=…`.

## Running the test suite

```bash
micromamba run -n shiny pytest tests/unit -v
micromamba run -n shiny pytest -m network tests/integration -v   # requires net
```

## Pre-commit hooks

Installed automatically by `pre-commit install`. They run `ruff`, `ruff format`,
and `mypy --strict` on every commit.
```

- [ ] **Step 2: Commit.**

```bash
git add docs/contributing.md
git commit -m "docs: add contributing guide with spec § 8.5 forbidden patterns"
```

---

### Task 5c: Add `docs/decisions/2026-05-08-phase1-decisions.md`

**Files:**
- Create: `EcoNeTool-py/docs/decisions/2026-05-08-phase1-decisions.md`

- [ ] **Step 1: Write the file.**

```markdown
# Phase 1 design decisions

Spec § 15 lists eight open decisions the planner must resolve before coding
starts. Phase 1 silently makes four of them. This file records the choice and
the rationale so Phase 2+ doesn't re-litigate.

## D1 — asyncio fan-out primitive: `asyncio.wait(FIRST_COMPLETED)`

**Decision:** use `asyncio.wait(..., return_when=FIRST_COMPLETED)` in the
orchestrator's per-species early-exit loop, not `asyncio.TaskGroup`.

**Reason:** The spec § 4.1 step 5 code sample uses `asyncio.wait`. `TaskGroup`
(Python 3.11+) is the modern primitive, but it raises `ExceptionGroup` on any
child failure and cancels siblings — incompatible with our fail-soft contract
(one source's failure must NOT abort the gather). `asyncio.wait` lets us
inspect each task's exception via `task.exception()` and continue.

**Phase 1 impact:** none — Phase 1 has no fan-out. Decision documented for
Phase 3 orchestrator.

## D2 — Pydantic settings: single `Settings` singleton

**Decision:** one `Settings` instance per process, instantiated lazily from
`traitresearch.settings.Settings()`.

**Reason:** Per-module `BaseSettings` would mean N round-trips through the
environment for the same variable. A singleton keeps the env-loader cost
amortised and makes mock-injection in tests trivial (`monkeypatch.setattr`).

**Phase 1 impact:** Task 11 builds the singleton class. No global instance is
constructed yet; that happens at app startup in Phase 3.

## D6 — Module DI: `Lifecycle` class, not `contextvars`

**Decision:** shared state lives on a `Lifecycle` instance passed (or
attribute-accessed) by the orchestrator. Not `contextvars.ContextVar`.

**Reason:** `contextvars` are surgical for request-scoped state in async web
frameworks, but our state is process-scoped (HTTP client, cache writability,
in-flight task sets). `Lifecycle` is also explicit at the call site, which is
easier to reason about than implicit context propagation.

**Phase 1 impact:** Task 13 builds the `Lifecycle` class with `reset_for_tests`.

## D7 — DuckDB connection scope: per-query context manager

**Decision:** open a fresh DuckDB connection inside `with open_duckdb() as con`
for each query (or short batch). Not a process-global pool.

**Reason:** DuckDB connections are cheap (millisecond open). A short-lived
connection makes it trivial to re-apply the extension lockdown PRAGMAs every
time and avoids leaking parquet-file-handle state across queries. Reconsider
if profiling shows the open cost dominates (unlikely for our workload).

**Phase 1 impact:** Task 21 builds `open_duckdb()` as a context manager.

---

The four remaining open decisions (test fixture format, rate-limit token
bucket, subprocess spawn pattern, schema-check cache invalidation) are touched
in later phases and will be recorded in subsequent decision files.
```

- [ ] **Step 2: Commit.**

```bash
mkdir -p docs/decisions
git add docs/decisions/2026-05-08-phase1-decisions.md
git commit -m "docs: record Phase 1 design decisions (D1, D2, D6, D7) per spec § 15"
```

---

## Section B — Models (5 tasks)

### Task 6: `models/taxonomy.py` — `Species` and `Taxonomy`

**Deliverable:** `Species` (input model with whitespace-strip + non-control validators) and `Taxonomy` (resolved model with `compute_flags()` returning `{phyto, macroalgae, freshwater}` per spec § 4.6), plus 5 passing tests.

**Files:**
- Create: `EcoNeTool-py/traitresearch/models/taxonomy.py`
- Create: `EcoNeTool-py/tests/__init__.py`
- Create: `EcoNeTool-py/tests/unit/__init__.py`
- Create: `EcoNeTool-py/tests/unit/test_taxonomy.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_taxonomy.py
"""Tests for Species and Taxonomy models."""

import pytest
from pydantic import ValidationError
from traitresearch.models.taxonomy import Species, Taxonomy


def test_species_canonical_strip_whitespace():
    sp = Species(name="  Gadus morhua  ")
    assert sp.name == "Gadus morhua"


def test_taxonomy_compute_flags_phyto_includes_known_phyla():
    tax = Taxonomy(
        canonical_name="Some diatom",
        aphia_id=1,
        kingdom="Chromista",
        phylum="Bacillariophyta",
        class_name="Bacillariophyceae",
    )
    flags = tax.compute_flags()
    assert flags["phyto"] is True
    assert flags["freshwater"] is False


def test_taxonomy_compute_flags_macroalgae_carve_out():
    # Phaeophyceae is in the macroalgae carve-out — must NOT be phyto-flagged
    # for routing purposes (per spec § 4.6).
    tax = Taxonomy(
        canonical_name="Fucus vesiculosus",
        aphia_id=145541,
        kingdom="Chromista",
        phylum="Ochrophyta",
        class_name="Phaeophyceae",
    )
    flags = tax.compute_flags()
    assert flags["macroalgae"] is True
    # phyto is True (Ochrophyta is in the allow-list), but the macroalgae flag
    # tells the orchestrator's source-selection layer to de-select BVOL/PTDB.
    assert flags["phyto"] is True


def test_taxonomy_freshwater_flag():
    tax = Taxonomy(
        canonical_name="Perca fluviatilis",
        aphia_id=151353,
        kingdom="Animalia",
        phylum="Chordata",
        class_name="Actinopterygii",
        is_marine=False,
        is_brackish=False,
    )
    assert tax.compute_flags()["freshwater"] is True


def test_taxonomy_requires_canonical_name():
    with pytest.raises(ValidationError):
        Taxonomy(canonical_name="", aphia_id=1, kingdom="x", phylum="y")
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_taxonomy.py -v
```

Expected: FAIL with `ModuleNotFoundError: No module named 'traitresearch.models.taxonomy'`.

- [ ] **Step 3: Write `traitresearch/models/taxonomy.py`.**

```python
"""Taxonomy models — Species (input) and Taxonomy (resolved)."""

from __future__ import annotations

from pydantic import BaseModel, ConfigDict, Field, field_validator

# Phyla considered "phytoplankton" for routing (spec § 4.6, R orchestrator.R:401-403).
_PHYTO_PHYLA = frozenset({
    "chlorophyta", "ochrophyta", "haptophyta", "dinoflagellata",
    "bacillariophyta", "rhodophyta", "cyanobacteria", "cryptophyta",
    "euglenozoa", "charophyta", "myzozoa", "miozoa",
})

# Classes that route as macroalgae instead of phytoplankton (spec § 4.6).
_MACROALGAE_CLASSES = frozenset({"phaeophyceae", "florideophyceae", "ulvophyceae"})


class Species(BaseModel):
    """User-supplied species name; canonical form, whitespace-stripped."""

    model_config = ConfigDict(str_strip_whitespace=True, frozen=True)

    name: str = Field(min_length=1, max_length=200)

    @field_validator("name")
    @classmethod
    def _no_control_chars(cls, v: str) -> str:
        if not v.isprintable():
            raise ValueError("species name contains non-printable characters")
        return v


class Taxonomy(BaseModel):
    """Resolved taxonomy from WoRMS or GBIF.

    All taxonomic-level fields are stored lower-case for stable matching;
    `canonical_name` keeps original casing for display.
    """

    # populate_by_name=True is required so callers can pass `class_name=...`
    # as a kwarg even though the field declares alias="class". Without this
    # flag, pydantic v2 accepts only the alias key.
    model_config = ConfigDict(
        str_strip_whitespace=True,
        frozen=False,
        populate_by_name=True,
    )

    canonical_name: str = Field(min_length=1)
    aphia_id: int | None = None
    kingdom: str | None = None
    phylum: str | None = None
    class_name: str | None = Field(default=None, alias="class")  # 'class' is reserved
    order: str | None = None
    family: str | None = None
    genus: str | None = None
    subphylum: str | None = None
    is_marine: bool | None = None
    is_brackish: bool | None = None

    def compute_flags(self) -> dict[str, bool]:
        """Computed routing flags per spec § 4.6.

        Keys: phyto, macroalgae, freshwater. Each is `bool` (no None).
        """
        phylum = (self.phylum or "").lower()
        cls = (self.class_name or "").lower()

        phyto = phylum in _PHYTO_PHYLA or cls.endswith("phyceae")
        macroalgae = cls in _MACROALGAE_CLASSES
        freshwater = self.is_marine is False and self.is_brackish is not True

        return {"phyto": phyto, "macroalgae": macroalgae, "freshwater": freshwater}
```

- [ ] **Step 4: Run the test to verify it passes.**

```bash
pytest tests/unit/test_taxonomy.py -v
```

Expected: 5 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/models/taxonomy.py tests/__init__.py tests/unit/__init__.py tests/unit/test_taxonomy.py
git commit -m "feat(models): add Species and Taxonomy with phyto/macroalgae/freshwater flags"
```

---

### Task 7: `models/traits.py` — enums + `TraitObservation` + `RawTraits`

**Files:**
- Create: `EcoNeTool-py/traitresearch/models/traits.py`
- Create: `EcoNeTool-py/tests/unit/test_traits_basic.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_traits_basic.py
"""Tests for TraitObservation, RawTraits, and the two enums defined alongside.

Note on enum tests: rather than re-stating the enum definition (mock theatre),
we assert that all enum members serialise round-trip through `model_dump` /
`model_validate` on a SourceResult / LookupResult. This catches *real*
regressions like accidentally renaming a member or breaking StrEnum's value
contract — what trivial `assert FetchStatus.OK.value == "ok"` cannot.
"""

import pytest
from traitresearch.models.traits import (
    FetchStatus,
    RawTraits,
    StalenessState,
    TraitObservation,
)


def test_fetch_status_all_values_round_trip():
    """Every FetchStatus value survives a string-cast round-trip."""
    for status in FetchStatus:
        assert FetchStatus(status.value) is status
    # Member set is exactly the four documented in spec § 4.2.
    assert {s.value for s in FetchStatus} == {"ok", "empty", "skipped", "failed"}


def test_staleness_state_all_values_round_trip():
    """Every StalenessState value survives a string-cast round-trip."""
    for s in StalenessState:
        assert StalenessState(s.value) is s
    assert {s.value for s in StalenessState} == {
        "fresh", "stale_refresh_ok", "stale_refresh_failed",
    }


def test_trait_observation_minimal():
    obs = TraitObservation(value=15.5, unit="cm")
    assert obs.value == 15.5
    assert obs.unit == "cm"
    assert obs.is_imputed is False
    assert obs.confidence is None


def test_trait_observation_with_provenance():
    obs = TraitObservation(
        value="benthic",
        source_record_id="WoRMS:126436",
        source_url="https://www.marinespecies.org/aphia.php?p=taxdetails&id=126436",
        harmonization_rule_id="EP.benthic_pattern",
        confidence=0.9,
    )
    assert obs.source_url is not None
    assert obs.harmonization_rule_id == "EP.benthic_pattern"


def test_raw_traits_defaults_empty():
    raw = RawTraits()
    assert raw.feeding_mode is None
    assert raw.size_cm is None
    assert raw.is_complete() is False


def test_raw_traits_is_complete_requires_all_modalities():
    raw = RawTraits(
        size_cm=TraitObservation(value=15.0, unit="cm"),
        feeding_mode=TraitObservation(value="predator"),
        mobility_text=TraitObservation(value="swimmer"),
        habitat=TraitObservation(value="pelagic"),
        protection_text=TraitObservation(value="soft"),
        reproduction=TraitObservation(value="broadcast"),
        temperature=TraitObservation(value="boreal"),
        salinity=TraitObservation(value="euhaline"),
    )
    assert raw.is_complete() is True
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_traits_basic.py -v
```

Expected: FAIL with `ModuleNotFoundError`.

- [ ] **Step 3: Write `traitresearch/models/traits.py` (enums + observation + raw).**

```python
"""Trait models — enums, observations, raw inputs.

`HarmonizedTraits`, `SourceResult`, `LookupResult` are added in Task 8.
"""

from __future__ import annotations

from datetime import datetime
from enum import StrEnum

from pydantic import BaseModel, ConfigDict, Field


class FetchStatus(StrEnum):
    OK = "ok"
    EMPTY = "empty"
    SKIPPED = "skipped"
    FAILED = "failed"


class StalenessState(StrEnum):
    FRESH = "fresh"
    STALE_REFRESH_OK = "stale_refresh_ok"
    STALE_REFRESH_FAILED = "stale_refresh_failed"


class TraitObservation(BaseModel):
    """A single trait observation from one source — value plus full provenance."""

    model_config = ConfigDict(str_strip_whitespace=True)

    value: float | str | None = None
    unit: str | None = None
    raw_text: str | None = None
    source_record_id: str | None = None       # e.g. WoRMS AphiaID, FishBase SpecCode
    source_url: str | None = None             # deep-link to the upstream record
    harmonization_rule_id: str | None = None  # e.g. "MS.size_threshold.MS5"
    confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    is_imputed: bool = False                  # True iff produced by impute/, not lookup/


# Trait modalities collected from sources before harmonisation. The 8 fields below
# correspond directly to the 8 harmonised codes (MS, FS, MB, EP, PR, RS, TT, ST)
# but use descriptive names while still raw.
class RawTraits(BaseModel):
    """Pre-harmonisation traits collected from one or more sources."""

    model_config = ConfigDict(str_strip_whitespace=True)

    size_cm: TraitObservation | None = None              # → MS
    feeding_mode: TraitObservation | None = None         # → FS
    mobility_text: TraitObservation | None = None        # → MB
    habitat: TraitObservation | None = None              # → EP
    protection_text: TraitObservation | None = None      # → PR
    reproduction: TraitObservation | None = None         # → RS
    temperature: TraitObservation | None = None          # → TT
    salinity: TraitObservation | None = None             # → ST

    # Auxiliary observations consumed by harmonisation but not 1:1 with codes.
    trophic_level: TraitObservation | None = None
    depth_min: TraitObservation | None = None
    depth_max: TraitObservation | None = None
    body_shape: TraitObservation | None = None
    ontology_scores: dict[str, float] = Field(default_factory=dict)

    def is_complete(self) -> bool:
        """True iff every primary modality has a non-None observation."""
        return all(
            getattr(self, m) is not None
            for m in (
                "size_cm", "feeding_mode", "mobility_text", "habitat",
                "protection_text", "reproduction", "temperature", "salinity",
            )
        )

    def merge(self, other: "RawTraits") -> None:
        """In-place merge: keep existing values, fill nulls from `other`.

        Fill order favours the earliest source that supplied the value.
        """
        for field in self.model_fields:
            if field == "ontology_scores":
                self.ontology_scores = {**other.ontology_scores, **self.ontology_scores}
                continue
            if getattr(self, field) is None and getattr(other, field) is not None:
                setattr(self, field, getattr(other, field))
```

- [ ] **Step 4: Run the test to verify it passes.**

```bash
pytest tests/unit/test_traits_basic.py -v
```

Expected: 6 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/models/traits.py tests/unit/test_traits_basic.py
git commit -m "feat(models): add FetchStatus, StalenessState, TraitObservation, RawTraits"
```

---

### Task 8: `models/traits.py` — `HarmonizedTraits`, `SourceResult`, `LookupResult`

**Files:**
- Modify: `EcoNeTool-py/traitresearch/models/traits.py:120-end`
- Create: `EcoNeTool-py/tests/unit/test_traits_results.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_traits_results.py
"""Tests for HarmonizedTraits, SourceResult, LookupResult."""

from datetime import datetime, timezone

import pytest
from traitresearch.models.taxonomy import Taxonomy
from traitresearch.models.traits import (
    FetchStatus,
    HarmonizedTraits,
    HarmonizedValue,
    LookupResult,
    RawTraits,
    SourceResult,
    StalenessState,
)


def test_harmonized_value_minimal():
    hv = HarmonizedValue(code="MS5", confidence=0.85, source="fishbase")
    assert hv.code == "MS5"
    assert hv.is_imputed is False


def test_harmonized_traits_all_optional():
    h = HarmonizedTraits()
    assert h.MS is None and h.ST is None
    assert h.is_complete() is False


def test_source_result_failed_carries_reason():
    sr = SourceResult(source="fishbase", status=FetchStatus.FAILED, reason="timeout")
    assert sr.status == FetchStatus.FAILED
    assert sr.reason == "timeout"
    assert sr.data == {}


def test_lookup_result_round_trip_dict():
    tax = Taxonomy(canonical_name="Gadus morhua", aphia_id=126436, phylum="Chordata")
    h = HarmonizedTraits(
        MS=HarmonizedValue(code="MS5", confidence=0.9, source="fishbase"),
    )
    lr = LookupResult(
        species_name="Gadus morhua",
        aphia_id=126436,
        taxonomy=tax,
        raw=RawTraits(),
        harmonized=h,
        sources_used=[],
        cache_status="miss",
        staleness_state=StalenessState.FRESH,
        overall_confidence=0.9,
        cached_at=datetime.now(timezone.utc),
    )
    dumped = lr.model_dump()
    re_loaded = LookupResult.model_validate(dumped)
    assert re_loaded.species_name == "Gadus morhua"
    assert re_loaded.harmonized.MS is not None
    assert re_loaded.harmonized.MS.code == "MS5"
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_traits_results.py -v
```

Expected: FAIL on `ImportError: cannot import name 'HarmonizedTraits'`.

- [ ] **Step 3: Append to `traitresearch/models/traits.py`.**

```python
# Append below the existing RawTraits class.

from typing import Literal

from .taxonomy import Taxonomy


class HarmonizedValue(BaseModel):
    """One harmonised trait code with provenance."""

    model_config = ConfigDict(str_strip_whitespace=True)

    code: str                                    # e.g. "MS5", "FS2"
    confidence: float = Field(ge=0.0, le=1.0)
    source: str                                  # e.g. "fishbase" or "phylogenetic"
    source_url: str | None = None
    harmonization_rule_id: str | None = None
    is_imputed: bool = False


class HarmonizedTraits(BaseModel):
    """The eight harmonised modalities. Any may be None if not derivable."""

    model_config = ConfigDict()

    MS: HarmonizedValue | None = None
    FS: HarmonizedValue | None = None
    MB: HarmonizedValue | None = None
    EP: HarmonizedValue | None = None
    PR: HarmonizedValue | None = None
    RS: HarmonizedValue | None = None
    TT: HarmonizedValue | None = None
    ST: HarmonizedValue | None = None

    def is_complete(self) -> bool:
        return all(getattr(self, m) is not None for m in ("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST"))


class SourceResult(BaseModel):
    """Outcome of one source's `fetch()` for one species."""

    model_config = ConfigDict(str_strip_whitespace=True)

    source: str                                              # e.g. "fishbase"
    status: FetchStatus
    reason: str | None = None                                # populated when status != OK
    latency_ms: int | None = None
    data: dict[str, "TraitObservation"] = Field(default_factory=dict)


class LookupResult(BaseModel):
    """The final per-species result returned to UI and persisted to cache."""

    model_config = ConfigDict()

    species_name: str
    aphia_id: int | None = None
    taxonomy: Taxonomy
    raw: RawTraits
    harmonized: HarmonizedTraits
    sources_used: list[SourceResult] = Field(default_factory=list)
    imputation_method: str | None = None        # "observed" | "phylogenetic" | "knn" | "iterative"
    cache_status: Literal["hit", "miss", "stale"] = "miss"
    staleness_state: StalenessState = StalenessState.FRESH
    overall_confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    cached_at: datetime
    refresh_attempted_at: datetime | None = None
    partial: bool = False                       # True if species deadline cancelled the fetch
```

- [ ] **Step 4: Run all model tests.**

```bash
pytest tests/unit/ -v
```

Expected: all PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/models/traits.py tests/unit/test_traits_results.py
git commit -m "feat(models): add HarmonizedTraits, SourceResult, LookupResult per spec § 4.2"
```

---

### Task 9: `models/taxonomy_cache.py` — `TaxonomyCacheEntry` mirroring spec § 9.2

**Files:**
- Create: `EcoNeTool-py/traitresearch/models/taxonomy_cache.py`
- Create: `EcoNeTool-py/tests/unit/test_taxonomy_cache_model.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_taxonomy_cache_model.py
"""Tests for TaxonomyCacheEntry — mirrors taxonomy.db.species schema (spec § 9.2)."""

from datetime import datetime, timezone

import pytest
from traitresearch.models.taxonomy_cache import TaxonomyCacheEntry


def test_taxonomy_cache_entry_required_fields():
    entry = TaxonomyCacheEntry(
        species_name="Gadus morhua",
        cached_at=datetime.now(timezone.utc),
        updated_at=datetime.now(timezone.utc),
    )
    assert entry.species_name == "Gadus morhua"
    assert entry.aphia_id is None


def test_taxonomy_cache_entry_full_record():
    now = datetime.now(timezone.utc)
    entry = TaxonomyCacheEntry(
        species_name="Gadus morhua",
        aphia_id=126436,
        kingdom="Animalia",
        phylum="Chordata",
        class_name="Actinopterygii",
        order_name="Gadiformes",
        family="Gadidae",
        genus="Gadus",
        size_cm=120.0,
        max_length_cm=200.0,
        MS="MS6",
        FS="FS3",
        MB="MB5",
        EP="EP2",
        PR="PR0",
        MS_confidence=0.9,
        FS_confidence=0.8,
        overall_confidence=0.85,
        primary_source="fishbase",
        cached_at=now,
        updated_at=now,
        lookup_count=1,
        last_accessed=now,
    )
    assert entry.MS == "MS6"
    assert entry.primary_source == "fishbase"


def test_taxonomy_cache_entry_fields_match_spec_exactly():
    """Spec § 9.2 column-by-column mapping. Must be exact — no missing AND no extras.

    The 37 names below mirror `R/functions/cache_sqlite.R:79-136` minus the `id`
    column (auto-assigned by SQLite) and the `raw_data` BLOB (Python-skip per § 9.4).
    """
    expected_fields = {
        "species_name", "aphia_id",
        "kingdom", "phylum", "class_name", "order_name", "family", "genus",
        "size_cm", "max_length_cm", "common_length_cm", "weight_g",
        "feeding_type", "feeding_mode", "mobility_type", "habitat", "depth_range_m",
        "MS", "FS", "MB", "EP", "PR",
        "MS_confidence", "FS_confidence", "MB_confidence",
        "EP_confidence", "PR_confidence", "overall_confidence",
        "MS_source", "FS_source", "MB_source",
        "EP_source", "PR_source", "primary_source",
        "cached_at", "updated_at", "lookup_count", "last_accessed",
    }
    assert len(expected_fields) == 37
    actual = set(TaxonomyCacheEntry.model_fields.keys())
    missing = expected_fields - actual
    extras = actual - expected_fields
    assert missing == set(), f"missing fields: {sorted(missing)}"
    assert extras == set(), f"unexpected extra fields: {sorted(extras)}"
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_taxonomy_cache_model.py -v
```

Expected: FAIL with `ModuleNotFoundError`.

- [ ] **Step 3: Write `traitresearch/models/taxonomy_cache.py`.**

```python
"""TaxonomyCacheEntry — pydantic model mirroring taxonomy.db.species schema.

Field set is derived column-by-column from `R/functions/cache_sqlite.R:79-136`.
The R-side `raw_data` BLOB column is intentionally omitted (spec § 9.4) — Python
neither reads nor writes it.
"""

from __future__ import annotations

from datetime import datetime

from pydantic import BaseModel, ConfigDict, Field, field_validator


def _strip_and_cap_text(v: str | None) -> str | None:
    """Strip control characters and cap at 256 chars (spec § 9.5)."""
    if v is None:
        return None
    cleaned = "".join(ch for ch in v if ch.isprintable() or ch == " ")
    return cleaned[:256] if len(cleaned) > 256 else cleaned


class TaxonomyCacheEntry(BaseModel):
    """One row of taxonomy.db.species. ID column is auto-assigned by SQLite."""

    # populate_by_name=True so callers can pass `class_name=...` as a kwarg
    # despite the alias="class" declaration below.
    model_config = ConfigDict(str_strip_whitespace=True, populate_by_name=True)

    # Identity
    species_name: str = Field(min_length=1, max_length=200)
    aphia_id: int | None = None

    # Taxonomy
    kingdom: str | None = None
    phylum: str | None = None
    class_name: str | None = Field(default=None, alias="class")
    order_name: str | None = None
    family: str | None = None
    genus: str | None = None

    # Body-size measurements (multiple sources, multiple units already canonicalised → cm)
    size_cm: float | None = None
    max_length_cm: float | None = None
    common_length_cm: float | None = None
    weight_g: float | None = None

    # Raw functional descriptors
    feeding_type: str | None = None
    feeding_mode: str | None = None
    mobility_type: str | None = None
    habitat: str | None = None
    depth_range_m: str | None = None

    # Harmonised codes (5 here; RS/TT/ST live on the offline cache only — see spec § 9.3)
    MS: str | None = None
    FS: str | None = None
    MB: str | None = None
    EP: str | None = None
    PR: str | None = None

    # Per-trait confidence
    MS_confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    FS_confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    MB_confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    EP_confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    PR_confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    overall_confidence: float | None = Field(default=None, ge=0.0, le=1.0)

    # Per-trait source label
    MS_source: str | None = None
    FS_source: str | None = None
    MB_source: str | None = None
    EP_source: str | None = None
    PR_source: str | None = None
    primary_source: str | None = None

    # Bookkeeping
    cached_at: datetime
    updated_at: datetime
    lookup_count: int | None = 0
    last_accessed: datetime | None = None

    # Validators — apply to every TEXT field that comes from upstream sources.
    # Strip control characters and cap length at the model boundary (spec § 9.5).
    # DAOs trust the model and never duplicate this work.
    # The validator runs on EVERY string field via mode="before"; this avoids
    # an explicit field-name list that drifts when subclasses (OfflineCacheEntry)
    # add new TEXT columns. Non-string inputs pass through unchanged.
    @field_validator("*", mode="before")
    @classmethod
    def _clean_text(cls, v: object) -> object:
        return _strip_and_cap_text(v) if isinstance(v, str) else v
```

- [ ] **Step 4: Run the test to verify it passes.**

```bash
pytest tests/unit/test_taxonomy_cache_model.py -v
```

Expected: 3 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/models/taxonomy_cache.py tests/unit/test_taxonomy_cache_model.py
git commit -m "feat(models): add TaxonomyCacheEntry per spec § 9.2 (37 columns)"
```

---

### Task 10: `models/offline_cache.py` — `OfflineCacheEntry` extending taxonomy_cache

**Files:**
- Create: `EcoNeTool-py/traitresearch/models/offline_cache.py`
- Create: `EcoNeTool-py/tests/unit/test_offline_cache_model.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_offline_cache_model.py
"""Tests for OfflineCacheEntry — extends TaxonomyCacheEntry per spec § 9.3."""

from datetime import datetime, timezone

from traitresearch.models.offline_cache import OfflineCacheEntry


def test_offline_cache_entry_extra_fields():
    now = datetime.now(timezone.utc)
    entry = OfflineCacheEntry(
        species_name="Gadus morhua",
        cached_at=now,
        updated_at=now,
        RS="RS1",
        TT="TT2",
        ST="ST5",
        trophic_level=4.0,
        depth_min=10.0,
        depth_max=600.0,
        is_hab=False,
        longevity_years=25.0,
        growth_rate=0.2,
        body_shape="elongated",
        imputation_method="observed",
    )
    assert entry.TT == "TT2"
    assert entry.imputation_method == "observed"


def test_offline_cache_entry_inherits_taxonomy_fields():
    now = datetime.now(timezone.utc)
    entry = OfflineCacheEntry(
        species_name="x", cached_at=now, updated_at=now,
        phylum="Chordata", MS="MS5",
    )
    assert entry.phylum == "Chordata"
    assert entry.MS == "MS5"


def test_offline_extra_fields_match_spec_section_9_3():
    expected_extra = {
        "RS", "TT", "ST",
        "trophic_level", "depth_min", "depth_max",
        "is_hab", "longevity_years", "growth_rate",
        "body_shape", "phyto_motility", "phyto_growth_form",
        "RS_confidence", "TT_confidence", "ST_confidence",
        "imputation_method",
    }
    fields = set(OfflineCacheEntry.model_fields.keys())
    missing = expected_extra - fields
    assert missing == set(), f"missing extras from spec § 9.3: {missing}"
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_offline_cache_model.py -v
```

Expected: FAIL.

- [ ] **Step 3: Write `traitresearch/models/offline_cache.py`.**

```python
"""OfflineCacheEntry — extends TaxonomyCacheEntry with spec § 9.3 extras."""

from __future__ import annotations

from pydantic import Field

from .taxonomy_cache import TaxonomyCacheEntry


class OfflineCacheEntry(TaxonomyCacheEntry):
    """One row of offline_traits.db.species_traits.

    Adds the columns introduced by R `migrate_offline_schema()`
    (`R/functions/cache_sqlite.R:746-763`).
    """

    # The remaining 3 harmonised codes
    RS: str | None = None
    TT: str | None = None
    ST: str | None = None

    # Per-trait confidence for the new codes
    RS_confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    TT_confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    ST_confidence: float | None = Field(default=None, ge=0.0, le=1.0)

    # Continuous traits used by imputation / Food Web
    trophic_level: float | None = None
    depth_min: float | None = None
    depth_max: float | None = None
    longevity_years: float | None = None
    growth_rate: float | None = None

    # Categorical traits
    is_hab: bool | None = None              # harmful-algal-bloom flag
    body_shape: str | None = None
    phyto_motility: str | None = None
    phyto_growth_form: str | None = None

    # Imputation provenance
    imputation_method: str | None = None    # "observed" | "phylogenetic" | "knn" | "iterative"
```

- [ ] **Step 4: Run the test.**

```bash
pytest tests/unit/test_offline_cache_model.py -v
```

Expected: 3 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/models/offline_cache.py tests/unit/test_offline_cache_model.py
git commit -m "feat(models): add OfflineCacheEntry per spec § 9.3"
```

---

## Section C — Settings, Secrets, Lifecycle (3 tasks)

### Task 11: `settings.py` — env-var loader via pydantic-settings

**Files:**
- Create: `EcoNeTool-py/traitresearch/settings.py`
- Create: `EcoNeTool-py/tests/unit/test_settings.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_settings.py
"""Tests for env-var-driven Settings."""

from pathlib import Path

import pytest
from traitresearch.settings import Settings


def test_settings_defaults_with_env(tmp_path, monkeypatch):
    monkeypatch.setenv("ECONET_DATA_DIR", str(tmp_path / "data"))
    monkeypatch.setenv("ECONET_CACHE_DIR", str(tmp_path / "cache"))
    monkeypatch.setenv("ECONET_LOG_DIR", str(tmp_path / "logs"))
    monkeypatch.setenv("ECONET_HANDOFF_DIR", str(tmp_path / "handoff"))
    (tmp_path / "data").mkdir()
    (tmp_path / "cache").mkdir()
    (tmp_path / "logs").mkdir()
    (tmp_path / "handoff").mkdir()

    s = Settings()
    assert s.cache_ttl_days == 30
    assert s.species_deadline_s == 30
    assert s.http_timeout_s == 15
    assert s.max_species_per_run == 500
    assert s.fishbase_cache_max_gb == 4
    assert s.parquet_prewarm is True


def test_settings_rejects_relative_path(tmp_path, monkeypatch):
    monkeypatch.setenv("ECONET_DATA_DIR", "data")  # not absolute
    monkeypatch.setenv("ECONET_CACHE_DIR", str(tmp_path / "cache"))
    monkeypatch.setenv("ECONET_LOG_DIR", str(tmp_path / "logs"))
    monkeypatch.setenv("ECONET_HANDOFF_DIR", str(tmp_path / "handoff"))
    with pytest.raises(ValueError, match="must be absolute"):
        Settings()


def test_settings_rejects_path_traversal(tmp_path, monkeypatch):
    bad = tmp_path / ".." / "x"
    monkeypatch.setenv("ECONET_DATA_DIR", str(bad))
    monkeypatch.setenv("ECONET_CACHE_DIR", str(tmp_path / "cache"))
    monkeypatch.setenv("ECONET_LOG_DIR", str(tmp_path / "logs"))
    monkeypatch.setenv("ECONET_HANDOFF_DIR", str(tmp_path / "handoff"))
    with pytest.raises(ValueError, match="\\.\\."):
        Settings()


def test_settings_rejects_nonexistent_path(tmp_path, monkeypatch):
    """Spec § 7.5 step 10: `ECONET_*_DIR` paths must exist on disk."""
    monkeypatch.setenv("ECONET_DATA_DIR", str(tmp_path / "does-not-exist"))
    monkeypatch.setenv("ECONET_CACHE_DIR", str(tmp_path / "cache"))
    monkeypatch.setenv("ECONET_LOG_DIR", str(tmp_path / "logs"))
    monkeypatch.setenv("ECONET_HANDOFF_DIR", str(tmp_path / "handoff"))
    (tmp_path / "cache").mkdir()
    (tmp_path / "logs").mkdir()
    (tmp_path / "handoff").mkdir()
    with pytest.raises(ValueError, match="does not exist"):
        Settings()
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_settings.py -v
```

Expected: FAIL with `ModuleNotFoundError`.

- [ ] **Step 3: Write `traitresearch/settings.py`.**

```python
"""Env-var driven settings via pydantic-settings.

Matches spec § 10.6 canonical env-var table.
"""

from __future__ import annotations

from pathlib import Path

from pydantic import field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """Process-wide settings. Read from env at construction; one instance per process."""

    model_config = SettingsConfigDict(
        env_prefix="ECONET_",
        env_file=None,
        case_sensitive=False,
        extra="ignore",
    )

    # Filesystem roots (validated absolute, no ..)
    data_dir: Path
    cache_dir: Path
    log_dir: Path
    handoff_dir: Path

    # Behaviour
    cache_ttl_days: int = 30
    species_deadline_s: int = 30
    http_timeout_s: int = 15
    max_species_per_run: int = 500
    fishbase_cache_max_gb: int = 4
    parquet_prewarm: bool = True

    # Optional admin gate
    admin_token: str | None = None

    # Harmonization version tag (matched against conf/harmonization.yaml header)
    harmonization_version: str | None = None

    @field_validator("data_dir", "cache_dir", "log_dir", "handoff_dir")
    @classmethod
    def _absolute_no_traversal_exists(cls, p: Path) -> Path:
        if not p.is_absolute():
            raise ValueError(f"path must be absolute: {p}")
        if ".." in p.parts:
            raise ValueError(f"path may not contain '..': {p}")
        if not p.exists():
            raise ValueError(f"path does not exist: {p}")
        return p
```

- [ ] **Step 4: Run the test.**

```bash
pytest tests/unit/test_settings.py -v
```

Expected: 3 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/settings.py tests/unit/test_settings.py
git commit -m "feat(settings): add Settings model with absolute-path + no-traversal validation"
```

---

### Task 12: `secrets.py` — API-key resolver returning None when absent

**Files:**
- Create: `EcoNeTool-py/traitresearch/secrets.py`
- Create: `EcoNeTool-py/tests/unit/test_secrets.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_secrets.py
"""Tests for the API-key resolver."""

import pytest
from traitresearch.secrets import get_secret, list_required_secrets


def test_get_secret_returns_value(monkeypatch):
    monkeypatch.setenv("ECONET_FRESHWATER_KEY", "abc-123")
    assert get_secret("ECONET_FRESHWATER_KEY") == "abc-123"


def test_get_secret_returns_none_when_unset(monkeypatch):
    monkeypatch.delenv("ECONET_FRESHWATER_KEY", raising=False)
    assert get_secret("ECONET_FRESHWATER_KEY") is None


def test_get_secret_strips_whitespace(monkeypatch):
    monkeypatch.setenv("ECONET_TRAITBANK_KEY", "  key-with-padding  ")
    assert get_secret("ECONET_TRAITBANK_KEY") == "key-with-padding"


def test_list_required_secrets_includes_known_keys():
    required = list_required_secrets()
    assert "ECONET_FRESHWATER_KEY" in required
    assert "ECONET_ALGAEBASE_USER" in required
    assert "ECONET_ALGAEBASE_PASS" in required
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_secrets.py -v
```

Expected: FAIL.

- [ ] **Step 3: Write `traitresearch/secrets.py`.**

```python
"""API-key resolver. All secrets are env-var-only; never in conf/ files.

The caller is responsible for handling None returns by routing the source as SKIPPED.
"""

from __future__ import annotations

import os
from typing import Final

# Spec § 10.6 secrets — keep this list authoritative.
REQUIRED_SECRETS: Final[tuple[str, ...]] = (
    "ECONET_FRESHWATER_KEY",
    "ECONET_ALGAEBASE_USER",
    "ECONET_ALGAEBASE_PASS",
)

OPTIONAL_SECRETS: Final[tuple[str, ...]] = (
    "ECONET_TRAITBANK_KEY",
    "ECONET_WORMS_KEY",
)


def get_secret(name: str) -> str | None:
    """Return the env-var value, stripped, or None if missing or empty."""
    raw = os.environ.get(name)
    if raw is None:
        return None
    stripped = raw.strip()
    return stripped or None


def list_required_secrets() -> tuple[str, ...]:
    return REQUIRED_SECRETS


def secret_values_for_redaction() -> set[str]:
    """All currently-set secret values, for the loguru redaction filter."""
    out: set[str] = set()
    for name in REQUIRED_SECRETS + OPTIONAL_SECRETS:
        v = get_secret(name)
        if v:
            out.add(v)
    return out
```

- [ ] **Step 4: Run the test.**

```bash
pytest tests/unit/test_secrets.py -v
```

Expected: 4 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/secrets.py tests/unit/test_secrets.py
git commit -m "feat(secrets): add env-var-only API-key resolver with redaction-set helper"
```

---

### Task 13: `lifecycle.py` — `Lifecycle` class with `reset_for_tests`

**Deliverable:** A `Lifecycle` class holding `active_fetches`, `background_upserts`, `http_client`, `cache_writable`, `shutting_down`, with `track_fetch()` / `track_upsert()` / `reset_for_tests()` methods. (`track_upsert` is consumed in Phase 3+ when sources start writing UPSERTs; in Phase 1 only `track_fetch` is exercised — both are part of the public API.)

**Files:**
- Create: `EcoNeTool-py/traitresearch/lifecycle.py`
- Create: `EcoNeTool-py/tests/unit/test_lifecycle.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_lifecycle.py
"""Tests for Lifecycle class."""

import asyncio

import pytest
from traitresearch.lifecycle import Lifecycle


def test_lifecycle_initial_state():
    lc = Lifecycle()
    assert lc.shutting_down is False
    assert lc.http_client is None
    assert lc.cache_writable is True
    assert lc.active_fetches == set()
    assert lc.background_upserts == set()


def test_lifecycle_reset_clears_task_sets():
    lc = Lifecycle()
    fake_task = object()
    lc.active_fetches.add(fake_task)        # type: ignore[arg-type]
    lc.background_upserts.add(fake_task)    # type: ignore[arg-type]
    lc.shutting_down = True
    lc.reset_for_tests()
    assert lc.active_fetches == set()
    assert lc.background_upserts == set()
    assert lc.shutting_down is False


@pytest.mark.asyncio
async def test_lifecycle_track_active_fetch():
    lc = Lifecycle()

    async def quick():
        return "ok"

    task = asyncio.create_task(quick())
    lc.track_fetch(task)
    assert task in lc.active_fetches
    await task
    # `add_done_callback` is scheduled via `loop.call_soon`; give the loop one
    # tick to drain pending callbacks before asserting removal.
    await asyncio.sleep(0)
    assert task not in lc.active_fetches


@pytest.mark.asyncio
async def test_lifecycle_removes_on_exception():
    """Tasks that raise still have their callback fire — ensure cleanup is robust."""
    lc = Lifecycle()

    async def boom():
        raise RuntimeError("oops")

    task = asyncio.create_task(boom())
    lc.track_fetch(task)
    with pytest.raises(RuntimeError):
        await task
    await asyncio.sleep(0)
    assert task not in lc.active_fetches
```

- [ ] **Step 2: Run the test to verify it fails.**

```bash
pytest tests/unit/test_lifecycle.py -v
```

Expected: FAIL.

- [ ] **Step 3: Write `traitresearch/lifecycle.py`.**

```python
"""Lifecycle — process-wide shared state held on a class instance.

Spec § 7.5: avoids module-level globals so pytest fixtures can construct
fresh instances without test cross-pollution.
"""

from __future__ import annotations

import asyncio
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    import httpx


class Lifecycle:
    """One instance per app process; tests construct their own."""

    def __init__(self) -> None:
        # `Any` parameter on Task is required for mypy --strict; we don't
        # narrow the result type because tracked tasks may return anything.
        self.active_fetches: set[asyncio.Task[Any]] = set()
        self.background_upserts: set[asyncio.Task[Any]] = set()
        self.http_client: "httpx.AsyncClient | None" = None
        self.cache_writable: bool = True
        self.shutting_down: bool = False

    def track_fetch(self, task: asyncio.Task[Any]) -> None:
        """Register a source-fetch task; auto-removed on completion."""
        self.active_fetches.add(task)
        task.add_done_callback(self.active_fetches.discard)

    def track_upsert(self, task: asyncio.Task[Any]) -> None:
        """Register a background UPSERT task; auto-removed on completion."""
        self.background_upserts.add(task)
        task.add_done_callback(self.background_upserts.discard)

    def reset_for_tests(self) -> None:
        """Clear in-memory state. Call from pytest fixtures.

        Does NOT close `http_client` — the caller is responsible for opening
        and closing a fresh client in each test fixture if needed.
        """
        self.active_fetches.clear()
        self.background_upserts.clear()
        self.shutting_down = False
        self.cache_writable = True
```

- [ ] **Step 4: Run the test.**

```bash
pytest tests/unit/test_lifecycle.py -v
```

Expected: 3 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/lifecycle.py tests/unit/test_lifecycle.py
git commit -m "feat(lifecycle): add Lifecycle class with task tracking and reset_for_tests"
```

- [ ] **Step 6: Mid-plan suite gate — run all unit tests.**

```bash
micromamba run -n shiny pytest tests/unit -v
```

Expected: all PASS (no regressions from earlier tasks). If any fail, fix in place before continuing to Section D.

---

## Section D — Cache layer (5 tasks + 1 invariant tool: 14, 15, 16, 17, 17.5, 18)

### Task 14: `cache/connection.py` — open with PRAGMA setup

**Files:**
- Create: `EcoNeTool-py/traitresearch/cache/connection.py`
- Create: `EcoNeTool-py/tests/conftest.py`
- Create: `EcoNeTool-py/tests/unit/test_cache_connection.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_cache_connection.py
"""Tests for cache.connection.open_db."""

import pytest
import aiosqlite
from traitresearch.cache.connection import open_db


@pytest.mark.asyncio
async def test_open_db_sets_busy_timeout(tmp_path):
    db_path = tmp_path / "test.db"
    async with open_db(db_path) as db:
        async with db.execute("PRAGMA busy_timeout") as cur:
            row = await cur.fetchone()
            assert row is not None and row[0] == 5000


@pytest.mark.asyncio
async def test_open_db_enables_wal(tmp_path):
    db_path = tmp_path / "test.db"
    async with open_db(db_path) as db:
        async with db.execute("PRAGMA journal_mode") as cur:
            row = await cur.fetchone()
            assert row is not None
            assert row[0].lower() == "wal"


@pytest.mark.asyncio
async def test_open_db_enables_foreign_keys(tmp_path):
    db_path = tmp_path / "test.db"
    async with open_db(db_path) as db:
        async with db.execute("PRAGMA foreign_keys") as cur:
            row = await cur.fetchone()
            assert row is not None and row[0] == 1


@pytest.mark.asyncio
async def test_open_db_sets_journal_size_limit(tmp_path):
    db_path = tmp_path / "test.db"
    async with open_db(db_path) as db:
        async with db.execute("PRAGMA journal_size_limit") as cur:
            row = await cur.fetchone()
            assert row is not None and row[0] == 67108864   # 64 MB
```

- [ ] **Step 2: Write the root pytest conftest.**

```python
# tests/conftest.py
"""Root pytest config — asyncio mode is set in pyproject.toml."""

import pytest


@pytest.fixture
def lifecycle():
    """Fresh Lifecycle per test."""
    from traitresearch.lifecycle import Lifecycle

    lc = Lifecycle()
    yield lc
    lc.reset_for_tests()
```

- [ ] **Step 3: Run the test to verify it fails.**

```bash
pytest tests/unit/test_cache_connection.py -v
```

Expected: FAIL with `ModuleNotFoundError`.

- [ ] **Step 4: Write `traitresearch/cache/connection.py`.**

```python
"""SQLite connection helper with PRAGMA setup for shared-cache concurrency.

Spec § 9.1: WAL mode + busy_timeout=5000 on every connection. WAL is sticky
once set, so this is idempotent across mixed R+Python access.
"""

from __future__ import annotations

from contextlib import asynccontextmanager
from pathlib import Path
from typing import AsyncIterator

import aiosqlite


@asynccontextmanager
async def open_db(path: Path | str) -> AsyncIterator[aiosqlite.Connection]:
    """Open `path` with WAL + busy_timeout + foreign_keys.

    Usage:
        async with open_db(settings.cache_dir / "taxonomy.db") as db:
            await db.execute(...)
    """
    db = await aiosqlite.connect(str(path))
    try:
        await db.execute("PRAGMA journal_mode = WAL")
        await db.execute("PRAGMA busy_timeout = 5000")
        await db.execute("PRAGMA foreign_keys = ON")
        await db.execute("PRAGMA journal_size_limit = 67108864")  # 64 MB
        await db.commit()
        yield db
    finally:
        await db.close()
```

- [ ] **Step 5: Run the test.**

```bash
pytest tests/unit/test_cache_connection.py -v
```

Expected: 3 PASS.

- [ ] **Step 6: Commit.**

```bash
git add traitresearch/cache/connection.py tests/conftest.py tests/unit/test_cache_connection.py
git commit -m "feat(cache): add open_db with WAL + busy_timeout + journal_size_limit"
```

---

### Task 15: `cache/schema_check.py` — column introspection

**Files:**
- Create: `EcoNeTool-py/traitresearch/cache/schema_check.py`
- Create: `EcoNeTool-py/tests/unit/test_schema_check.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_schema_check.py
"""Tests for PRAGMA-table_info introspection and column-set comparison."""

import aiosqlite
import pytest
from traitresearch.cache.connection import open_db
from traitresearch.cache.schema_check import (
    SchemaMismatchError,
    columns_for_table,
    verify_columns_present,
)


@pytest.mark.asyncio
async def test_columns_for_table_returns_set(tmp_path):
    db_path = tmp_path / "x.db"
    async with open_db(db_path) as db:
        await db.execute("CREATE TABLE t (a INTEGER, b TEXT)")
        await db.commit()
        cols = await columns_for_table(db, "t")
    assert cols == {"a", "b"}


@pytest.mark.asyncio
async def test_verify_columns_present_passes_when_subset(tmp_path):
    db_path = tmp_path / "x.db"
    async with open_db(db_path) as db:
        await db.execute("CREATE TABLE t (a INTEGER, b TEXT, c REAL)")
        await db.commit()
        # Python knows {a, b}; DB has {a, b, c} — extra column c is OK
        await verify_columns_present(db, "t", required={"a", "b"})  # no exception


@pytest.mark.asyncio
async def test_verify_columns_present_raises_on_missing(tmp_path):
    db_path = tmp_path / "x.db"
    async with open_db(db_path) as db:
        await db.execute("CREATE TABLE t (a INTEGER)")
        await db.commit()
        with pytest.raises(SchemaMismatchError, match="missing.*b"):
            await verify_columns_present(db, "t", required={"a", "b"})


@pytest.mark.asyncio
async def test_verify_columns_present_case_insensitive(tmp_path):
    """R EcoNeTool may write columns with different casing (e.g. `Phylum`).

    SQLite is case-insensitive on identifiers; the verifier must be too.
    """
    db_path = tmp_path / "x.db"
    async with open_db(db_path) as db:
        await db.execute("CREATE TABLE t (Phylum TEXT, MS TEXT)")
        await db.commit()
        await verify_columns_present(db, "t", required={"phylum", "ms"})  # no exception
```

- [ ] **Step 2: Run the test.**

```bash
pytest tests/unit/test_schema_check.py -v
```

Expected: FAIL.

- [ ] **Step 3: Write `traitresearch/cache/schema_check.py`.**

```python
"""Schema check — PRAGMA table_info introspection.

Spec § 9.5: introspect once at startup, cache the result, never re-check
per connection. Re-introspection on SIGHUP only.
"""

from __future__ import annotations

import aiosqlite


class SchemaMismatchError(RuntimeError):
    """Raised when the DB lacks a column Python expects to write."""


async def columns_for_table(db: aiosqlite.Connection, table: str) -> set[str]:
    """Return the set of column names defined for `table` with original casing.

    Returned with original case so DAO INSERT statements emit identifiers
    that match the DDL exactly (SQLite is case-insensitive on identifiers but
    case-preserving on display, and any portable consumer expects the original).
    Use `verify_columns_present` for case-insensitive existence checks.
    """
    out: set[str] = set()
    async with db.execute(f"PRAGMA table_info({table})") as cur:
        async for row in cur:
            # row schema: (cid, name, type, notnull, dflt_value, pk)
            out.add(row[1])
    return out


async def verify_columns_present(
    db: aiosqlite.Connection,
    table: str,
    required: set[str],
) -> None:
    """Raise if any column in `required` is missing from `table` (case-insensitive).

    Extra columns in the DB are tolerated (R may have added new ones). The
    comparison is case-insensitive because R EcoNeTool may write `Phylum`
    where Python expects `phylum`; SQLite treats the two as the same column.
    """
    actual = await columns_for_table(db, table)
    actual_lower = {c.lower() for c in actual}
    required_lower = {c.lower() for c in required}
    missing = required_lower - actual_lower
    if missing:
        raise SchemaMismatchError(
            f"table {table!r} missing columns: {sorted(missing)}"
        )
```

- [ ] **Step 4: Run the test.**

```bash
pytest tests/unit/test_schema_check.py -v
```

Expected: 3 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/cache/schema_check.py tests/unit/test_schema_check.py
git commit -m "feat(cache): add schema introspection + verify_columns_present"
```

---

### Task 16: `cache/taxonomy_db.py` — `get` + `upsert` with bound parameters

**Deliverable:** Async DAO with `get_by_species()` (read with `row_factory` save/restore) + `upsert_entry()` (column allow-listed UPSERT, all values bound positionally, case-insensitive column matching for shared-DB safety). Plus the `_TAXONOMY_DDL` constant for tests/CLI use only (Python NEVER runs DDL on the shared DB per spec § 9.6). Three passing tests.

**Files:**
- Create: `EcoNeTool-py/traitresearch/cache/taxonomy_db.py`
- Create: `EcoNeTool-py/tests/unit/test_taxonomy_db.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_taxonomy_db.py
"""Tests for taxonomy.db DAO."""

from datetime import datetime, timezone
from pathlib import Path

import pytest
from traitresearch.cache.connection import open_db
from traitresearch.cache.taxonomy_db import (
    create_taxonomy_schema,
    get_by_species,
    upsert_entry,
)
from traitresearch.models.taxonomy_cache import TaxonomyCacheEntry


@pytest.fixture
async def fresh_db(tmp_path):
    db_path = tmp_path / "taxonomy.db"
    async with open_db(db_path) as db:
        await create_taxonomy_schema(db)
        await db.commit()
    return db_path


@pytest.mark.asyncio
async def test_get_returns_none_for_missing(fresh_db: Path):
    async with open_db(fresh_db) as db:
        assert await get_by_species(db, "Nope") is None


@pytest.mark.asyncio
async def test_upsert_then_get_round_trips(fresh_db: Path):
    now = datetime.now(timezone.utc)
    entry = TaxonomyCacheEntry(
        species_name="Gadus morhua",
        aphia_id=126436,
        kingdom="Animalia",
        phylum="Chordata",
        MS="MS6",
        FS="FS3",
        MS_confidence=0.9,
        primary_source="fishbase",
        cached_at=now,
        updated_at=now,
    )
    async with open_db(fresh_db) as db:
        await upsert_entry(db, entry)
        await db.commit()
        out = await get_by_species(db, "Gadus morhua")
    assert out is not None
    assert out.aphia_id == 126436
    assert out.MS == "MS6"
    assert out.MS_confidence == pytest.approx(0.9)


@pytest.mark.asyncio
async def test_upsert_updates_existing_row(fresh_db: Path):
    now = datetime.now(timezone.utc)
    e1 = TaxonomyCacheEntry(
        species_name="Gadus morhua", MS="MS5", cached_at=now, updated_at=now,
    )
    e2 = TaxonomyCacheEntry(
        species_name="Gadus morhua", MS="MS6", cached_at=now, updated_at=now,
    )
    async with open_db(fresh_db) as db:
        await upsert_entry(db, e1)
        await upsert_entry(db, e2)
        await db.commit()
        out = await get_by_species(db, "Gadus morhua")
    assert out is not None
    assert out.MS == "MS6"
```

- [ ] **Step 2: Run the test.**

```bash
pytest tests/unit/test_taxonomy_db.py -v
```

Expected: FAIL.

- [ ] **Step 3: Write `traitresearch/cache/taxonomy_db.py`.**

```python
"""DAO for taxonomy.db.species — read + upsert.

Per spec § 9.5, the UPSERT uses ONLY columns Python knows about (intersected
with PRAGMA table_info on the live DB). Values are bound positionally; never
f-string interpolated.
"""

from __future__ import annotations

from typing import Any

import aiosqlite

from ..models.taxonomy_cache import TaxonomyCacheEntry
from .schema_check import columns_for_table

# DDL — MUST match R-side `cache_sqlite.R:79-136`. The Python app does NOT run
# this on a shared DB (R owns the schema, per spec § 9.6); it's used only by
# tests (against an empty tmp DB) and by the offline-DB rebuild CLI in Phase 7.
_TAXONOMY_DDL = """
CREATE TABLE IF NOT EXISTS species (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    species_name TEXT UNIQUE NOT NULL,
    aphia_id INTEGER,
    kingdom TEXT, phylum TEXT, class TEXT, order_name TEXT, family TEXT, genus TEXT,
    size_cm REAL, max_length_cm REAL, common_length_cm REAL, weight_g REAL,
    feeding_type TEXT, feeding_mode TEXT, mobility_type TEXT,
    habitat TEXT, depth_range_m TEXT,
    MS TEXT, FS TEXT, MB TEXT, EP TEXT, PR TEXT,
    MS_confidence REAL, FS_confidence REAL, MB_confidence REAL,
    EP_confidence REAL, PR_confidence REAL, overall_confidence REAL,
    MS_source TEXT, FS_source TEXT, MB_source TEXT,
    EP_source TEXT, PR_source TEXT, primary_source TEXT,
    cached_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    lookup_count INTEGER DEFAULT 0,
    last_accessed TIMESTAMP,
    raw_data BLOB
);
CREATE INDEX IF NOT EXISTS idx_species_genus ON species(genus);
CREATE INDEX IF NOT EXISTS idx_species_family ON species(family);
CREATE INDEX IF NOT EXISTS idx_species_phylum ON species(phylum);
CREATE INDEX IF NOT EXISTS idx_species_MS ON species(MS);
CREATE INDEX IF NOT EXISTS idx_species_FS ON species(FS);
"""

# pydantic-side field name → DB column name (handles `class_name` → `class`).
_FIELD_TO_COLUMN: dict[str, str] = {"class_name": "class"}


def _column_for(field: str) -> str:
    return _FIELD_TO_COLUMN.get(field, field)


async def create_taxonomy_schema(db: aiosqlite.Connection) -> None:
    """Create the species table + indexes. For tests and the offline-DB CLI only."""
    await db.executescript(_TAXONOMY_DDL)


async def get_by_species(
    db: aiosqlite.Connection, species_name: str,
) -> TaxonomyCacheEntry | None:
    """Read one row by species_name (case-sensitive); returns None if absent.

    Saves and restores `db.row_factory` so it doesn't leak across calls — the
    caller's connection state survives untouched.
    """
    saved_factory = db.row_factory
    db.row_factory = aiosqlite.Row
    try:
        async with db.execute(
            "SELECT * FROM species WHERE species_name = ? LIMIT 1",
            (species_name,),
        ) as cur:
            row = await cur.fetchone()
    finally:
        db.row_factory = saved_factory
    if row is None:
        return None
    payload: dict[str, Any] = dict(row)
    if "class" in payload:
        payload["class_name"] = payload.pop("class")
    payload.pop("id", None)
    payload.pop("raw_data", None)        # never read R-serialised BLOB (spec § 9.4)
    return TaxonomyCacheEntry.model_validate(payload)


async def upsert_entry(
    db: aiosqlite.Connection, entry: TaxonomyCacheEntry,
) -> None:
    """UPSERT a row. Writes only fields Python knows AND DB has.

    Control-character stripping and length capping happen at the model boundary
    (TaxonomyCacheEntry validators); this DAO trusts its input and just writes.
    Column matching is case-insensitive (R may write `Phylum` while Python's
    DDL has `phylum`) — the actual identifier emitted in the SQL uses the
    DB-side casing returned by PRAGMA.
    """
    db_cols = await columns_for_table(db, "species")
    db_cols_lower = {c.lower(): c for c in db_cols}   # lower → original-case map
    payload: dict[str, Any] = {}
    for field in entry.model_fields:
        col = _column_for(field)
        actual_col = db_cols_lower.get(col.lower())
        if actual_col is None:
            continue
        payload[actual_col] = getattr(entry, field)

    if "species_name" not in {k.lower() for k in payload}:
        raise ValueError("upsert_entry requires species_name")

    cols = list(payload.keys())
    placeholders = ", ".join("?" for _ in cols)
    non_pk = [c for c in cols if c.lower() != "species_name"]
    if not non_pk:
        # Nothing to update besides the conflict key. Fall back to INSERT … OR IGNORE
        # so an existing row is not touched and a new one is created if absent.
        sql = (
            f"INSERT OR IGNORE INTO species ({', '.join(cols)}) "
            f"VALUES ({placeholders})"
        )
    else:
        update_clause = ", ".join(f"{c}=excluded.{c}" for c in non_pk)
        sql = (
            f"INSERT INTO species ({', '.join(cols)}) "
            f"VALUES ({placeholders}) "
            f"ON CONFLICT(species_name) DO UPDATE SET {update_clause}"
        )
    await db.execute(sql, tuple(payload[c] for c in cols))
```

- [ ] **Step 4: Run the test.**

```bash
pytest tests/unit/test_taxonomy_db.py -v
```

Expected: 3 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/cache/taxonomy_db.py tests/unit/test_taxonomy_db.py
git commit -m "feat(cache): add taxonomy_db DAO with column-allow-listed UPSERT"
```

---

### Task 17: `cache/offline_db.py` — read-only DAO (with rebuild-CLI write path)

**Deliverable:** Async DAO for `offline_traits.db.species_traits` with `get_offline_by_species()` + `upsert_offline_entry()` mirroring Task 16's pattern. Two passing tests; rebuild-CLI write path tested for completeness even though only the Phase 7 rebuild CLI uses it at runtime.

**Files:**
- Create: `EcoNeTool-py/traitresearch/cache/offline_db.py`
- Create: `EcoNeTool-py/tests/unit/test_offline_db.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_offline_db.py
"""Tests for offline_traits.db DAO (read-only path; rebuild CLI is Phase 7)."""

from datetime import datetime, timezone

import pytest
from traitresearch.cache.connection import open_db
from traitresearch.cache.offline_db import (
    create_offline_schema,
    get_offline_by_species,
    upsert_offline_entry,   # used only by the rebuild CLI; tests it here for completeness
)
from traitresearch.models.offline_cache import OfflineCacheEntry


@pytest.mark.asyncio
async def test_offline_round_trip(tmp_path):
    db_path = tmp_path / "offline.db"
    now = datetime.now(timezone.utc)
    entry = OfflineCacheEntry(
        species_name="Gadus morhua",
        TT="TT2",
        ST="ST5",
        trophic_level=4.1,
        cached_at=now,
        updated_at=now,
    )
    async with open_db(db_path) as db:
        await create_offline_schema(db)
        await upsert_offline_entry(db, entry)
        await db.commit()
        out = await get_offline_by_species(db, "Gadus morhua")
    assert out is not None
    assert out.TT == "TT2"
    assert out.trophic_level == pytest.approx(4.1)


@pytest.mark.asyncio
async def test_offline_get_missing_returns_none(tmp_path):
    db_path = tmp_path / "offline.db"
    async with open_db(db_path) as db:
        await create_offline_schema(db)
        await db.commit()
        assert await get_offline_by_species(db, "Nope") is None
```

- [ ] **Step 2: Run the test.**

```bash
pytest tests/unit/test_offline_db.py -v
```

Expected: FAIL.

- [ ] **Step 3: Write `traitresearch/cache/offline_db.py`.**

```python
"""DAO for offline_traits.db.species_traits.

Read path is the runtime-hot path (a cache-tier check before live lookups).
Upsert path is used only by the offline-DB rebuild CLI in Phase 7.
"""

from __future__ import annotations

from typing import Any

import aiosqlite

from ..models.offline_cache import OfflineCacheEntry
from .schema_check import columns_for_table
from .taxonomy_db import _FIELD_TO_COLUMN, _column_for  # reuse mapping

# Spec § 9.3 — extends taxonomy.db.species with the 16 extra columns from
# `migrate_offline_schema()` in R `cache_sqlite.R:746-763`.
_OFFLINE_DDL = """
CREATE TABLE IF NOT EXISTS species_traits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    species_name TEXT UNIQUE NOT NULL,
    aphia_id INTEGER,
    kingdom TEXT, phylum TEXT, class TEXT, order_name TEXT, family TEXT, genus TEXT,
    size_cm REAL, max_length_cm REAL, common_length_cm REAL, weight_g REAL,
    feeding_type TEXT, feeding_mode TEXT, mobility_type TEXT,
    habitat TEXT, depth_range_m TEXT,
    MS TEXT, FS TEXT, MB TEXT, EP TEXT, PR TEXT,
    RS TEXT, TT TEXT, ST TEXT,
    MS_confidence REAL, FS_confidence REAL, MB_confidence REAL,
    EP_confidence REAL, PR_confidence REAL,
    RS_confidence REAL, TT_confidence REAL, ST_confidence REAL,
    overall_confidence REAL,
    MS_source TEXT, FS_source TEXT, MB_source TEXT,
    EP_source TEXT, PR_source TEXT, primary_source TEXT,
    trophic_level REAL, depth_min REAL, depth_max REAL,
    is_hab INTEGER, longevity_years REAL, growth_rate REAL,
    body_shape TEXT, phyto_motility TEXT, phyto_growth_form TEXT,
    imputation_method TEXT,
    cached_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    lookup_count INTEGER DEFAULT 0,
    last_accessed TIMESTAMP,
    raw_data BLOB
);
CREATE INDEX IF NOT EXISTS idx_st_genus ON species_traits(genus);
CREATE INDEX IF NOT EXISTS idx_st_family ON species_traits(family);
CREATE INDEX IF NOT EXISTS idx_st_phylum ON species_traits(phylum);
"""


async def create_offline_schema(db: aiosqlite.Connection) -> None:
    await db.executescript(_OFFLINE_DDL)


async def get_offline_by_species(
    db: aiosqlite.Connection, species_name: str,
) -> OfflineCacheEntry | None:
    """Read one offline-traits row; restores caller's row_factory on exit."""
    saved_factory = db.row_factory
    db.row_factory = aiosqlite.Row
    try:
        async with db.execute(
            "SELECT * FROM species_traits WHERE species_name = ? LIMIT 1",
            (species_name,),
        ) as cur:
            row = await cur.fetchone()
    finally:
        db.row_factory = saved_factory
    if row is None:
        return None
    payload: dict[str, Any] = dict(row)
    if "class" in payload:
        payload["class_name"] = payload.pop("class")
    payload.pop("id", None)
    payload.pop("raw_data", None)
    # SQLite stores booleans as 0/1; pydantic accepts both
    return OfflineCacheEntry.model_validate(payload)


async def upsert_offline_entry(
    db: aiosqlite.Connection, entry: OfflineCacheEntry,
) -> None:
    """UPSERT one offline-traits row. Same model-trust contract as taxonomy_db."""
    db_cols = await columns_for_table(db, "species_traits")
    db_cols_lower = {c.lower(): c for c in db_cols}
    payload: dict[str, Any] = {}
    for field in entry.model_fields:
        col = _column_for(field)
        actual_col = db_cols_lower.get(col.lower())
        if actual_col is None:
            continue
        payload[actual_col] = getattr(entry, field)

    if "species_name" not in {k.lower() for k in payload}:
        raise ValueError("upsert_offline_entry requires species_name")

    cols = list(payload.keys())
    placeholders = ", ".join("?" for _ in cols)
    non_pk = [c for c in cols if c.lower() != "species_name"]
    if not non_pk:
        sql = (
            f"INSERT OR IGNORE INTO species_traits ({', '.join(cols)}) "
            f"VALUES ({placeholders})"
        )
    else:
        update_clause = ", ".join(f"{c}=excluded.{c}" for c in non_pk)
        sql = (
            f"INSERT INTO species_traits ({', '.join(cols)}) "
            f"VALUES ({placeholders}) "
            f"ON CONFLICT(species_name) DO UPDATE SET {update_clause}"
        )
    await db.execute(sql, tuple(payload[c] for c in cols))
```

- [ ] **Step 4: Run the test.**

```bash
pytest tests/unit/test_offline_db.py -v
```

Expected: 2 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/cache/offline_db.py tests/unit/test_offline_db.py
git commit -m "feat(cache): add offline_db DAO mirroring spec § 9.3"
```

- [ ] **Step 6: Mid-plan suite gate — run all unit tests.**

```bash
micromamba run -n shiny pytest tests/unit -v
```

Expected: all PASS. The cache layer is now complete enough to support Sections E–G; if anything regresses here, fix before continuing.

Continue to Task 17.5 (the DDL-vs-models invariant tool) below.

---

### Task 17.5: DDL-vs-models invariant checker

**Deliverable:** A standalone Python script `tools/check_ddl_vs_models.py` that compares each cache table's DDL column list against the corresponding pydantic model's field set. Catches the most common drift regression — someone adds a model field but forgets the DDL, or vice versa.

**Files:**
- Create: `EcoNeTool-py/tools/check_ddl_vs_models.py`

- [ ] **Step 1: Write `tools/check_ddl_vs_models.py`.**

```python
"""Verify each SQL DDL column appears in the corresponding pydantic model
(after the `class_name` ↔ `class` alias mapping), and vice-versa. Run as a
standalone script; exits non-zero on mismatch.
"""

from __future__ import annotations

import re
import sys

from traitresearch.cache.taxonomy_db import _TAXONOMY_DDL
from traitresearch.cache.offline_db import _OFFLINE_DDL
from traitresearch.cache.taxonomy_db import _FIELD_TO_COLUMN
from traitresearch.models.taxonomy_cache import TaxonomyCacheEntry
from traitresearch.models.offline_cache import OfflineCacheEntry


def _ddl_columns(ddl: str, table: str) -> set[str]:
    create = re.search(rf"CREATE TABLE IF NOT EXISTS {table} \((.*?)\);", ddl, re.S)
    assert create is not None, f"no CREATE TABLE for {table}"
    body = create.group(1)
    cols: set[str] = set()
    for line in body.splitlines():
        line = line.strip().rstrip(",")
        if not line or line.upper().startswith(("PRIMARY", "UNIQUE", "FOREIGN")):
            continue
        # First whitespace-delimited token of each column line is the column name.
        cols.add(line.split()[0])
    # Drop the auto-PK and the BLOB column we deliberately don't model.
    cols.discard("id")
    cols.discard("raw_data")
    return cols


def _model_columns(model_cls) -> set[str]:
    return {
        _FIELD_TO_COLUMN.get(f, f) for f in model_cls.model_fields
    }


def main() -> int:
    failures: list[str] = []
    for table, ddl, model in [
        ("species",        _TAXONOMY_DDL, TaxonomyCacheEntry),
        ("species_traits", _OFFLINE_DDL,  OfflineCacheEntry),
    ]:
        ddl_cols = _ddl_columns(ddl, table)
        mod_cols = _model_columns(model)
        ddl_only = ddl_cols - mod_cols
        mod_only = mod_cols - ddl_cols
        if ddl_only:
            failures.append(f"{table}: in DDL but not in model: {sorted(ddl_only)}")
        if mod_only:
            failures.append(f"{table}: in model but not in DDL: {sorted(mod_only)}")
    if failures:
        for f in failures:
            print(f"DDL/model drift: {f}", file=sys.stderr)
        return 1
    print("DDL ↔ model invariants OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
```

- [ ] **Step 2: Run the checker.**

```bash
micromamba run -n shiny python tools/check_ddl_vs_models.py
```

Expected stdout: `DDL ↔ model invariants OK`. If it errors, fix the drift between Tasks 9/10 (models) and Tasks 16/17 (DDL) before continuing.

- [ ] **Step 3: Commit.**

```bash
git add tools/check_ddl_vs_models.py
git commit -m "tools: add DDL-vs-models invariant checker for cache layer"
```

---

### Task 18: `test_upsert_safety.py` — parameter-binding invariants

**Files:**
- Create: `EcoNeTool-py/tests/unit/test_upsert_safety.py`

- [ ] **Step 1: Write the test.**

```python
# tests/unit/test_upsert_safety.py
"""Invariants per spec § 9.5: UPSERTs use only bound parameters."""

import re
from datetime import datetime, timezone

import pytest
from traitresearch.cache.connection import open_db
from traitresearch.cache.taxonomy_db import (
    create_taxonomy_schema,
    get_by_species,
    upsert_entry,
)
from traitresearch.models.taxonomy_cache import TaxonomyCacheEntry


@pytest.mark.asyncio
async def test_upsert_uses_bound_parameters(tmp_path, monkeypatch):
    """Verify the SQL passed to aiosqlite has one ? per non-PK column AND that
    no value appears f-string-interpolated into the SQL text."""
    db_path = tmp_path / "x.db"
    captured: list[tuple[str, tuple]] = []

    import aiosqlite as _ais

    real_execute = _ais.Connection.execute

    async def spy_execute(self, sql, *args, **kwargs):
        params = args[0] if args else None
        if sql.strip().upper().startswith("INSERT INTO SPECIES"):
            captured.append((sql, params))
        return await real_execute(self, sql, *args, **kwargs)

    monkeypatch.setattr(_ais.Connection, "execute", spy_execute)

    now = datetime.now(timezone.utc)
    # Adversarial value — would be SQL-injection if interpolated.
    injection = "'); DROP TABLE species; --"
    entry = TaxonomyCacheEntry(
        species_name="Adversarial sp.",
        feeding_mode=injection,
        cached_at=now,
        updated_at=now,
    )
    async with open_db(db_path) as db:
        await create_taxonomy_schema(db)
        await upsert_entry(db, entry)
        await db.commit()
        async with db.execute(
            "SELECT feeding_mode FROM species WHERE species_name = ?",
            ("Adversarial sp.",),
        ) as cur:
            row = await cur.fetchone()
    assert row is not None
    assert row[0] == injection                  # stored verbatim, not executed

    assert captured, "expected at least one INSERT execute call"
    sql, params = captured[0]
    n_placeholders = sql.count("?")
    assert isinstance(params, tuple)
    assert n_placeholders == len(params), \
        f"placeholders ({n_placeholders}) != params ({len(params)})"

    # Adversarial value must NOT appear inline in the SQL.
    assert "DROP TABLE" not in sql, f"adversarial value leaked into SQL: {sql}"

    # Generated SQL should match the documented shape.
    assert re.search(r"^\s*INSERT INTO species \(", sql) is not None
    assert "ON CONFLICT(species_name) DO UPDATE SET" in sql


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "idx, poison, expected_stored",
    [
        # All printable — round-trips verbatim.
        (0, "'); DROP TABLE species; --",                "'); DROP TABLE species; --"),
        (1, "' UNION SELECT * FROM sqlite_master--",     "' UNION SELECT * FROM sqlite_master--"),
        (2, 'x"; ATTACH DATABASE \'/etc/shadow\' AS p; --',
            'x"; ATTACH DATABASE \'/etc/shadow\' AS p; --'),
        # Newlines are non-printable — validator strips them, NOT-tautologically:
        # the test explicitly enumerates the expected post-strip string.
        (3, "'\nDELETE FROM species\n--",                "'DELETE FROM species--"),
        # Mixed: tab + null byte + printable — both stripped.
        (4, "x\t\x00 OR 1=1--",                          "x OR 1=1--"),
    ],
)
async def test_upsert_neutralises_injection_variants(
    tmp_path, idx: int, poison: str, expected_stored: str,
):
    """Multiple injection shapes; each round-trips to a hard-coded expected string.

    The expected output is hand-computed per row (not derived from the same
    filter the validator uses) so the test is load-bearing — a regression in
    the strip rule WILL fail this test.
    """
    now = datetime.now(timezone.utc)
    db_path = tmp_path / "x.db"
    species = f"Adversarial-{idx}"   # deterministic across runs
    entry = TaxonomyCacheEntry(
        species_name=species,
        feeding_mode=poison,
        cached_at=now,
        updated_at=now,
    )
    async with open_db(db_path) as db:
        await create_taxonomy_schema(db)
        await upsert_entry(db, entry)
        await db.commit()
        async with db.execute(
            "SELECT feeding_mode FROM species WHERE species_name = ?",
            (species,),
        ) as cur:
            row = await cur.fetchone()
        # The species table must still exist
        async with db.execute(
            "SELECT count(*) FROM sqlite_master WHERE name = 'species'"
        ) as cur:
            count_row = await cur.fetchone()
    assert row is not None
    assert row[0] == expected_stored, (
        f"poison={poison!r} expected={expected_stored!r} stored={row[0]!r}"
    )
    assert count_row is not None and count_row[0] == 1   # table not dropped


@pytest.mark.asyncio
async def test_upsert_caps_string_at_256_chars(tmp_path):
    """The TaxonomyCacheEntry model strips and caps at 256 chars (spec § 9.5)."""
    now = datetime.now(timezone.utc)
    db_path = tmp_path / "x.db"
    long_value = "a" * 300
    entry = TaxonomyCacheEntry(
        species_name="Long-string sp.",
        feeding_mode=long_value,
        cached_at=now,
        updated_at=now,
    )
    # The model validator should have already truncated.
    assert entry.feeding_mode is not None
    assert len(entry.feeding_mode) == 256

    async with open_db(db_path) as db:
        await create_taxonomy_schema(db)
        await upsert_entry(db, entry)
        await db.commit()
        out = await get_by_species(db, "Long-string sp.")
    assert out is not None
    assert out.feeding_mode is not None
    assert len(out.feeding_mode) == 256


@pytest.mark.asyncio
async def test_taxonomy_entry_strips_control_chars():
    """Spec § 9.5: control characters are stripped, not stored, not raised on."""
    now = datetime.now(timezone.utc)
    entry = TaxonomyCacheEntry(
        species_name="Ctrl sp.",
        feeding_mode="bad\x00val\x07ue",
        cached_at=now,
        updated_at=now,
    )
    assert entry.feeding_mode == "badvalue"
```

- [ ] **Step 2: Run the test.**

```bash
pytest tests/unit/test_upsert_safety.py -v
```

Expected: PASS (the implementation from Task 16 already binds parameters).

- [ ] **Step 3: Commit.**

```bash
git add tests/unit/test_upsert_safety.py
git commit -m "test: add UPSERT safety invariants per spec § 9.5"
```

---

## Section E — Cache round-trip integration (2 tasks)

### Task 19: `test_cache_roundtrip.py` — synthetic LookupResult through both DBs

**Files:**
- Create: `EcoNeTool-py/tests/fixtures/__init__.py`
- Create: `EcoNeTool-py/tests/unit/test_cache_roundtrip.py`

- [ ] **Step 1: Write the test.**

```python
# tests/unit/test_cache_roundtrip.py
"""End-to-end cache round-trip with synthetic data.

Acceptance criterion 3 of Phase 1.
The R-side parity test (Python writes → R reads, both ways) is added in Phase 2;
this test exercises Python-only round-trip.
"""

from datetime import datetime, timezone

import pytest
from traitresearch.cache.connection import open_db
from traitresearch.cache.offline_db import (
    create_offline_schema,
    get_offline_by_species,
    upsert_offline_entry,
)
from traitresearch.cache.taxonomy_db import (
    create_taxonomy_schema,
    get_by_species,
    upsert_entry,
)
from traitresearch.models.offline_cache import OfflineCacheEntry
from traitresearch.models.taxonomy_cache import TaxonomyCacheEntry


@pytest.mark.asyncio
async def test_taxonomy_db_full_round_trip(tmp_path):
    now = datetime.now(timezone.utc)
    src = TaxonomyCacheEntry(
        species_name="Mytilus edulis",
        aphia_id=140480,
        kingdom="Animalia",
        phylum="Mollusca",
        class_name="Bivalvia",
        order_name="Mytilida",
        family="Mytilidae",
        genus="Mytilus",
        size_cm=10.0,
        feeding_mode="filter feeder",
        MS="MS5",
        FS="FS6",
        MB="MB1",
        EP="EP3",
        PR="PR4",
        MS_confidence=0.95,
        overall_confidence=0.85,
        MS_source="biotic",
        primary_source="biotic",
        cached_at=now,
        updated_at=now,
        lookup_count=1,
    )
    db_path = tmp_path / "taxonomy.db"
    async with open_db(db_path) as db:
        await create_taxonomy_schema(db)
        await upsert_entry(db, src)
        await db.commit()
        rt = await get_by_species(db, "Mytilus edulis")

    assert rt is not None
    for field in src.model_fields:
        assert getattr(rt, field) == getattr(src, field), f"diff at {field}"


@pytest.mark.asyncio
async def test_offline_db_full_round_trip(tmp_path):
    now = datetime.now(timezone.utc)
    src = OfflineCacheEntry(
        species_name="Calanus finmarchicus",
        aphia_id=104464,
        phylum="Arthropoda",
        class_name="Hexanauplia",
        MS="MS1",
        FS="FS1",
        RS="RS1",
        TT="TT2",
        ST="ST5",
        trophic_level=2.5,
        depth_min=10.0,
        depth_max=300.0,
        is_hab=False,
        body_shape="copepod",
        imputation_method="observed",
        cached_at=now,
        updated_at=now,
    )
    db_path = tmp_path / "offline.db"
    async with open_db(db_path) as db:
        await create_offline_schema(db)
        await upsert_offline_entry(db, src)
        await db.commit()
        rt = await get_offline_by_species(db, "Calanus finmarchicus")

    assert rt is not None
    # Compare extras explicitly
    assert rt.RS == "RS1"
    assert rt.TT == "TT2"
    assert rt.ST == "ST5"
    assert rt.trophic_level == pytest.approx(2.5)
    assert rt.imputation_method == "observed"


@pytest.mark.asyncio
async def test_taxonomy_db_round_trip_with_none_values(tmp_path):
    """A row with explicit None on most fields must round-trip without
    silently turning them into empty strings or surprising defaults."""
    now = datetime.now(timezone.utc)
    src = TaxonomyCacheEntry(
        species_name="Sparse sp.",
        cached_at=now,
        updated_at=now,
        # All other fields default to None
    )
    db_path = tmp_path / "taxonomy.db"
    async with open_db(db_path) as db:
        await create_taxonomy_schema(db)
        await upsert_entry(db, src)
        await db.commit()
        rt = await get_by_species(db, "Sparse sp.")
    assert rt is not None
    assert rt.aphia_id is None
    assert rt.MS is None
    assert rt.MS_confidence is None
    assert rt.MS_source is None
    assert rt.primary_source is None
    assert rt.size_cm is None


@pytest.mark.asyncio
async def test_offline_db_round_trip_with_none_values(tmp_path):
    """Same None-preservation contract for the offline DB — important because
    offline_traits.db has different column types (e.g. is_hab INTEGER) that
    interact differently with SQLite NULL than taxonomy.db's TEXT/REAL columns.
    """
    now = datetime.now(timezone.utc)
    src = OfflineCacheEntry(
        species_name="Sparse offline sp.",
        cached_at=now,
        updated_at=now,
    )
    db_path = tmp_path / "offline.db"
    async with open_db(db_path) as db:
        await create_offline_schema(db)
        await upsert_offline_entry(db, src)
        await db.commit()
        rt = await get_offline_by_species(db, "Sparse offline sp.")
    assert rt is not None
    # Extras specific to offline_db
    assert rt.RS is None
    assert rt.TT is None
    assert rt.ST is None
    assert rt.trophic_level is None
    assert rt.depth_min is None
    assert rt.depth_max is None
    assert rt.is_hab is None
    assert rt.body_shape is None
    assert rt.imputation_method is None
```

- [ ] **Step 2: Add `tests/fixtures/__init__.py`.**

```python
# tests/fixtures/__init__.py
```

- [ ] **Step 3: Run the test.**

```bash
pytest tests/unit/test_cache_roundtrip.py -v
```

Expected: 2 PASS.

- [ ] **Step 4: Run the full unit suite.**

```bash
pytest tests/unit -v
```

Expected: all PASS, no warnings beyond pydantic v2 deprecation noise from third-party libs.

- [ ] **Step 5: Commit.**

```bash
git add tests/fixtures/__init__.py tests/unit/test_cache_roundtrip.py
git commit -m "test(cache): add Python-only round-trip for both DB schemas"
```

---

### Task 20: Re-export models from package root and add public API test

**Files:**
- Modify: `EcoNeTool-py/traitresearch/models/__init__.py`
- Create: `EcoNeTool-py/tests/unit/test_public_api.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_public_api.py
"""The library's public API — what consumers import."""

def test_public_imports_resolve():
    from traitresearch.models import (
        FetchStatus,
        HarmonizedTraits,
        HarmonizedValue,
        LookupResult,
        OfflineCacheEntry,
        RawTraits,
        SourceResult,
        Species,
        StalenessState,
        Taxonomy,
        TaxonomyCacheEntry,
        TraitObservation,
    )
    # Touch each so the import isn't dead-code-eliminated by future tooling.
    assert FetchStatus.OK.value == "ok"
    assert HarmonizedTraits is not None
    assert HarmonizedValue is not None
    assert LookupResult is not None
    assert OfflineCacheEntry is not None
    assert RawTraits is not None
    assert SourceResult is not None
    assert Species is not None
    assert StalenessState.FRESH.value == "fresh"
    assert Taxonomy is not None
    assert TaxonomyCacheEntry is not None
    assert TraitObservation is not None


def test_models_dunder_all_matches_public_names():
    """`__all__` must list exactly the public names exposed at the package root."""
    from traitresearch import models

    declared = set(models.__all__)
    public_names = {
        n for n in dir(models)
        if not n.startswith("_") and n[0].isupper()
    }
    extra = declared - public_names
    missing = public_names - declared
    assert extra == set(), f"__all__ lists names that don't exist: {extra}"
    assert missing == set(), f"public names missing from __all__: {missing}"
```

- [ ] **Step 2: Run to confirm it fails.**

```bash
pytest tests/unit/test_public_api.py -v
```

Expected: FAIL on `ImportError`.

- [ ] **Step 3: Update `traitresearch/models/__init__.py`.**

```python
"""Public model re-exports.

`from traitresearch.models import LookupResult` is the canonical way to consume
these — internal modules may import from sub-modules, callers should not.
"""

from .offline_cache import OfflineCacheEntry
from .taxonomy import Species, Taxonomy
from .taxonomy_cache import TaxonomyCacheEntry
from .traits import (
    FetchStatus,
    HarmonizedTraits,
    HarmonizedValue,
    LookupResult,
    RawTraits,
    SourceResult,
    StalenessState,
    TraitObservation,
)

__all__ = [
    "FetchStatus",
    "HarmonizedTraits",
    "HarmonizedValue",
    "LookupResult",
    "OfflineCacheEntry",
    "RawTraits",
    "SourceResult",
    "Species",
    "StalenessState",
    "Taxonomy",
    "TaxonomyCacheEntry",
    "TraitObservation",
]
```

- [ ] **Step 4: Run.**

```bash
pytest tests/unit/test_public_api.py -v
```

Expected: PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/models/__init__.py tests/unit/test_public_api.py
git commit -m "feat(models): re-export public API from traitresearch.models"
```

---

## Section F — DuckDB connectivity spike (2 tasks)

### Task 21: `duckdb_client.py` — locked-down client wrapper

**Files:**
- Create: `EcoNeTool-py/traitresearch/duckdb_client.py`
- Create: `EcoNeTool-py/tests/unit/test_duckdb_client.py`

- [ ] **Step 1: Write the failing test.**

```python
# tests/unit/test_duckdb_client.py
"""Tests for the locked-down DuckDB client wrapper.

Network-touching tests live in tests/integration/.
"""

import pytest
from traitresearch.duckdb_client import open_duckdb


def test_open_duckdb_runs_lockdown_pragmas(tmp_path):
    """All extension-related toggles are off after open."""
    with open_duckdb() as con:
        rows = con.execute("SELECT current_setting('autoinstall_known_extensions')").fetchone()
        assert rows is not None
        assert rows[0] in ("false", False, 0)
        rows = con.execute("SELECT current_setting('autoload_known_extensions')").fetchone()
        assert rows is not None
        assert rows[0] in ("false", False, 0)
        rows = con.execute("SELECT current_setting('allow_community_extensions')").fetchone()
        assert rows is not None
        assert rows[0] in ("false", False, 0)


def test_open_duckdb_basic_query():
    with open_duckdb() as con:
        result = con.execute("SELECT 1 + 1 AS x").fetchone()
        assert result is not None
        assert result[0] == 2


def test_open_duckdb_rejects_community_extension_install():
    """Lockdown is real: a hostile community-extension install must error."""
    import duckdb

    with open_duckdb() as con:
        # `INSTALL … FROM community` is the canonical hostile-source path.
        # `allow_community_extensions = false` should reject it.
        with pytest.raises((duckdb.Error, duckdb.HTTPException, duckdb.IOException)):
            con.execute("INSTALL prql FROM community")
```

- [ ] **Step 2: Run.**

```bash
pytest tests/unit/test_duckdb_client.py -v
```

Expected: FAIL.

- [ ] **Step 3: Write `traitresearch/duckdb_client.py`.**

```python
"""DuckDB connection wrapper with mandatory extension lockdown.

Spec § 8.1: every connection runs the lockdown PRAGMAs immediately after open;
httpfs is pre-installed at deploy time, never fetched at runtime.
"""

from __future__ import annotations

from contextlib import contextmanager
from typing import Iterator

import duckdb


@contextmanager
def open_duckdb(database: str = ":memory:") -> Iterator[duckdb.DuckDBPyConnection]:
    """Open a DuckDB connection with the spec § 8.1 lockdown applied.

    Use as `with open_duckdb() as con:`. `httpfs` is loaded if available;
    on dev machines without httpfs pre-installed it's a no-op (silently caught
    so unit tests pass without network privileges).
    """
    con = duckdb.connect(database)
    try:
        con.execute("SET autoinstall_known_extensions = false")
        con.execute("SET autoload_known_extensions   = false")
        con.execute("SET allow_community_extensions  = false")
        con.execute("SET allow_unsigned_extensions   = false")
        try:
            con.execute("LOAD httpfs")
        except duckdb.Error:
            # httpfs not pre-installed; remote parquet won't work but local
            # queries still do. Different DuckDB versions raise IOException,
            # HTTPException, or NotImplementedException — `duckdb.Error` is the
            # parent class so we catch all of them. Production deploys must
            # pre-install via bootstrap-laguna.sh.
            pass
        yield con
    finally:
        con.close()
```

- [ ] **Step 4: Run.**

```bash
pytest tests/unit/test_duckdb_client.py -v
```

Expected: 2 PASS.

- [ ] **Step 5: Commit.**

```bash
git add traitresearch/duckdb_client.py tests/unit/test_duckdb_client.py
git commit -m "feat(duckdb): add open_duckdb wrapper with extension lockdown per spec § 8.1"
```

---

### Task 22: Integration test — `DESCRIBE` FishBase parquet root

**Deliverable:** A `@pytest.mark.network` integration test that opens a DuckDB connection (with the spec § 8.1 lockdown), runs `DESCRIBE SELECT * FROM '<parquet_root>'`, and asserts schema+row availability. Skipped by default; **must pass on laguna.ku.lt before Phase 4 begins** (else execute spec § 14 fallback to bundled-parquet sync).

**Files:**
- Create: `EcoNeTool-py/tests/integration/__init__.py`
- Create: `EcoNeTool-py/tests/integration/test_duckdb_spike.py`

- [ ] **Step 1: Write the integration test.**

```python
# tests/integration/test_duckdb_spike.py
"""DuckDB connectivity spike — verify FishBase parquet root is reachable.

Marked @pytest.mark.network so default `pytest` skips it. Must be run manually
on the laguna dev box (or with `pytest -m network` locally with internet access)
BEFORE Phase 4 begins.

This is the spec § 14 "FishBase DuckDB spike" task. Failure means the planner
should pivot to bundled-parquet sync (cron'd `wget` from a workstation with
open egress) before designing `lookup/fishbase.py`.
"""

import pytest
from traitresearch.duckdb_client import open_duckdb

# Pinned at spec-write time. If rfishbase upstream moves the parquet root, this
# URL must be updated and conf/sources.yaml::fishbase.parquet_root regenerated.
_FISHBASE_PARQUET_ROOT = (
    "https://fishbase.ropensci.org/fishbase/species.parquet"
)


@pytest.mark.network
def test_duckdb_describes_fishbase_species_parquet():
    """DESCRIBE returns at least 5 columns including 'Species'."""
    with open_duckdb() as con:
        rows = con.execute(f"DESCRIBE SELECT * FROM '{_FISHBASE_PARQUET_ROOT}'").fetchall()
    columns = {r[0] for r in rows}
    assert "Species" in columns or "species" in columns, \
        f"expected a Species column in {sorted(columns)[:10]}"
    assert len(columns) > 5, f"expected > 5 columns, got {len(columns)}"


@pytest.mark.network
def test_duckdb_selects_one_species_from_fishbase():
    """SELECT a single known species without bulk download."""
    with open_duckdb() as con:
        rows = con.execute(
            f"SELECT * FROM '{_FISHBASE_PARQUET_ROOT}' "
            "WHERE Species = 'morhua' AND Genus = 'Gadus' LIMIT 1"
        ).fetchall()
    assert len(rows) == 1, "expected exactly one row for Gadus morhua"
```

- [ ] **Step 2: Add the package init.**

```python
# tests/integration/__init__.py
```

- [ ] **Step 3: Confirm the test is skipped by default.**

```bash
pytest tests/integration -v
```

Expected output includes `2 deselected` (network marker is excluded by `addopts` in pyproject).

- [ ] **Step 4: Run with the network marker (manual; on laguna or a host with egress).**

```bash
pytest -m network tests/integration -v
```

Expected on laguna: 2 PASS. If FAIL → planner must execute the spec § 14 fallback (bundled-parquet sync) before Phase 4.

- [ ] **Step 5: Commit.**

```bash
git add tests/integration/__init__.py tests/integration/test_duckdb_spike.py
git commit -m "test(integration): add FishBase parquet DuckDB connectivity spike"
```

---

## Section G — CI green, README, and Phase 1 close (3 tasks)

### Task 23: Confirm `mypy --strict` passes

**Files:**
- Modify: pre-existing files only (fixes if any).

- [ ] **Step 1: Run mypy on the package.**

```bash
micromamba run -n shiny mypy --strict traitresearch/
```

Expected: `Success: no issues found in N source files`. If issues surface, fix in place — typical fixes:
- Add return types to functions missing them.
- Replace `Any` with concrete types where pydantic dictates them.
- Use `cast()` for the `aiosqlite.Row` → `dict[str, Any]` step in the DAOs (already typed via `dict(row)` which mypy accepts).

- [ ] **Step 2: If you fixed anything, commit.**

```bash
git add -u
git commit -m "chore: address mypy --strict findings"
```

(If nothing to fix, skip the commit.)

---

### Task 24: Confirm `ruff` passes and the arch-check stays green

**Files:**
- Modify: pre-existing files only.

- [ ] **Step 1: Run ruff and the arch-check.**

```bash
micromamba run -n shiny ruff check .
micromamba run -n shiny ruff format --check .
./tools/arch-check.sh
```

Expected: all three exit 0.

- [ ] **Step 2: If ruff flags anything, run autofix and re-check.**

```bash
micromamba run -n shiny ruff check . --fix
micromamba run -n shiny ruff format .
git diff
```

If autofix changed code, review the diff and commit.

```bash
git add -u
git commit -m "chore: ruff autofix"
```

---

### Task 25: Run the full Phase 1 acceptance gate

**Files:** none (verification step).

- [ ] **Step 1: From a clean working tree, run the unit suite.**

```bash
git status                         # working tree clean
micromamba run -n shiny pytest tests/unit -v
```

Expected: all tests PASS, ~20+ tests, no warnings beyond third-party.

- [ ] **Step 2: Verify the public import.**

```bash
micromamba run -n shiny python -c "import traitresearch; from traitresearch.models import LookupResult; print(traitresearch.__version__, LookupResult)"
```

Expected: `0.1.0 <class 'traitresearch.models.traits.LookupResult'>`.

- [ ] **Step 3: Tag the commit.**

```bash
git tag -a phase1-complete -m "Phase 1 of EcoNeTool-py: bootstrap, models, cache, DuckDB spike"
git log --oneline -5
```

- [ ] **Step 4: Final commit (if any pending changes from the gate run).**

```bash
git status
# If nothing changed, no commit needed. Otherwise:
git add -u && git commit -m "chore: phase 1 acceptance gate"
```

---

## Phase Progression (after Phase 1)

The remaining phases are NOT detailed in this document — each gets its own plan written when its predecessor ships. Listed here at acceptance-gate level for visibility:

**Phase 2 — Harmonization + parity infrastructure** (~2 weeks)
- Acceptance: P4 reconciliation done in EcoNeTool repo; YAML-gen R script writes `conf/harmonization.yaml`; 8 `harmonize/` modules pass unit tests; cross-language harmonization parity test green for the 30-species fixture.
- Critical decisions held over: confirm exact regex grammar emitted by R YAML-gen; verify `tools/harmonization_yaml_gen.serialised_fields.txt` is feasible.

**Phase 3 — First end-to-end thin slice** (~1 week)
- Acceptance: routing engine + `applies_when:` DSL + WoRMS resolver + `lookup/ontology.py` + minimal Shiny app (`species_input` + `results_table`) renders `Gadus morhua` with one harmonised modality (size). Demo-able.

**Phase 4 — Broaden sources** (~4 weeks)
- Acceptance: all 17 sources implemented + canary-tested; DuckDB FishBase + SeaLifeBase under spec § 8.1 lockdown; pre-warm probe wired to startup; eager-loaded CSVs/XLSX on `Lifecycle`.

**Phase 5 — Imputation** (~1.5 weeks)
- Acceptance: phylogenetic + taxonomy_knn + IterativeImputer all implement `Imputer` protocol; soft parity ≥ 90 % vs R for fixture species; imputed values carry `is_imputed=True` end-to-end.

**Phase 6 — Shiny modules** (~3 weeks; partly parallel with Phase 5)
- Acceptance: all 16 modules implemented in the build-tier order (standalone → cache-fed → live-flow); radar default-to-heatmap when N>25; per-trait fetch report; schema-mismatch banner.

**Phase 7 — Offline-DB rebuild** (~1 week)
- Acceptance: `traitresearch.cli.rebuild_offline_db` CLI + `async @reactive.effect` integration + cancel button.

**Phase 8 — Hardening + observability** (~2 weeks)
- Acceptance: loguru secret-redaction filter; lifecycle path/symlink probes; runtime TOCTOU defence; FairScheduler implementation per § 11 R19 + sub-design doc; Excel-paste tolerance; CSV formula-injection escape; performance suite green per spec § 13.6.

**Phase 9 — Deployment + ops** (~1.5 weeks)
- Acceptance: `bootstrap-laguna.sh` runs idempotently on a fresh Ubuntu VM in ≤ 30 min (spec § 13.8); systemd unit + nginx snippet + cron jobs; R-side P1–P3 landed.

**Phase 10 — Documentation + final QA** (~1.5 weeks)
- Acceptance: README + `docs/operations.md` + `docs/adding_a_source.md` + onboarding tutorial; manual sign-off against all 15 acceptance criteria in spec § 13.

---

## Self-Review (post-write)

**Plan revision history:**

- **v1** (initial draft) — 25 tasks, ~3100 lines, 7 acceptance gates.
- **v2** — added Task 5b (`docs/contributing.md`), Task 5c (`docs/decisions/...`), `populate_by_name=True` on alias-using models, mid-plan suite gates after Sections C and D, model-level `_strip_and_cap_text` validator (resolving spec § 9.5 strip-vs-raise conflict), enum round-trip tests (Task 7), None round-trip tests (Tasks 19), additional injection variants (Task 18), `Lifecycle` class with `set[asyncio.Task[Any]]` typing, and a non-tautological injection-variant assertion using hand-coded expected values.
- **v3** — added Task 17.5 (`tools/check_ddl_vs_models.py` invariant tool), `INSERT OR IGNORE` fallback in DAOs, `row_factory` save/restore, `Settings` existence check, OfflineCacheEntry None round-trip, broader DuckDB exception handling, and a `@field_validator("*", mode="before")` so subclass TEXT fields inherit cleaning.
- **v4** (this revision) — added Recovery playbook section, Task Index table, Task 1 Step 0 pre-flight env+git+OneDrive checks, `.gitattributes` for line endings, absolute `git -C` with non-empty assertion in Task 1 Step 2, OneDrive "always keep on this device" note in Task 2 Step 3, upper version pins on dev tools matching pre-commit hook revs, `Deliverable:` lines on the high-traffic tasks, fixed stale section task counts, broadened DuckDB exception parent class.

**Spec coverage:** Phase 1 implements spec § 3 (project layout) skeleton, § 4.2 (model definitions), § 7.5 (Lifecycle class — partial; full startup probes come in Phase 3+ as sources land), § 8.1 (DuckDB extension lockdown), § 9.1/§ 9.2/§ 9.3/§ 9.5 (cache layer + schema check + UPSERT safety), § 10.6 (env-var canonical list — Settings model). Sections out of scope by design and tracked in phase progression: § 4.3 (LookupSource protocol — Phase 4), § 4.4 (Imputer protocol — Phase 5), § 4.5 (sources, Phase 4), § 4.6 (routing, Phase 3), § 4.8 (edge cases, Phase 4), § 5 (harmonization, Phase 2), § 6 (imputation, Phase 5), § 7.5 startup probes for sources/parquet (Phase 3+), § 11 (risks — Lifecycle handles R10/R21 partially), § 13 acceptance (all in later phases), § 14 ops runbook (Phase 10).

**Placeholder scan:** No "TBD", "TODO", "implement later" in the steps. Every code block is complete code that runs. Every `git commit -m` message is concrete. Every test step shows the exact `pytest` command and expected outcome.

**Type consistency:** `LookupResult.staleness_state` typed as `StalenessState` in Task 8; matches Task 7's enum. `TaxonomyCacheEntry.MS` typed as `str | None` in Task 9; matches `HarmonizedValue.code: str` in Task 8 and the column-by-column mapping in spec § 9.2. The `class_name` field with alias `"class"` is used identically in Task 6 (Taxonomy), Task 9 (TaxonomyCacheEntry — inherited by Task 10 OfflineCacheEntry), and the DAOs in Tasks 16–17 (`_FIELD_TO_COLUMN`). Both Taxonomy and TaxonomyCacheEntry carry `populate_by_name=True` so callers can pass `class_name=...` as a kwarg. `Lifecycle.active_fetches: set[asyncio.Task[Any]]` in Task 13 satisfies `mypy --strict`. The wildcard `@field_validator("*", mode="before")` in Task 9 propagates to OfflineCacheEntry (Task 10) so all TEXT fields — current and future — get stripped and capped uniformly.

**Known caveats / forward debt explicitly accepted in Phase 1:**

- The shared-DB write path (Phase 7+) will need to handle case-mismatched columns under live R+Python operation. Phase 1's case-insensitive lookup in `db_cols_lower` covers this proactively, but no test exercises the actual mixed-case shared-DB scenario; that test arrives in Phase 7's parity work.
- Task 17.5's invariant checker is a development-time tool, not a runtime check; the same DDL drift would still let production write succeed. A runtime SchemaMismatchError path exists (Task 15) and will be wired into the lifecycle startup probe in Phase 3.
- The hardcoded `$PARENT='C:/Users/arturas.baziukas/...'` in Task 1 is the user's specific OneDrive path. Phase 1 Step 0 includes a derivation hint for other hosts. Not a portability bug at MVP scope; revisit if a second developer joins.

**v4 is ready for execution.** Successive review rounds (8 across two angles) found no remaining substantive issues — only style/cosmetic items absorbed inline.

---

*End of Phase 1 plan.*
