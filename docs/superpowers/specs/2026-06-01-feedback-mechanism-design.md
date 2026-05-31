# In-App Feedback Mechanism + Triage Skill — Design

**Date:** 2026-06-01
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Approved (brainstorm). Pending 3-agent spec review + user review.
**Scope:** Two subsystems sharing one store contract: (1) an in-app bug/
suggestion **feedback mechanism** (Shiny), and (2) a dev-side **triage skill**
that reads entries, drives the fix, and marks them addressed. Plan phases:
**P1** store + submit form, **P2** gated admin panel, **P3** triage skill.

---

## 1. Goal & non-goals

### Goal
Let any EcoNeTool user report a bug or suggestion from inside the app, persist
it server-side across deploys, and give the maintainer a repeatable workflow to
review entries, fix the underlying issue, and mark the entry addressed — with
the status visible in the app.

### Non-goals (YAGNI)
- No real user auth/accounts (the admin view uses a simple env-keyed gate,
  explicitly low-stakes — §4.2).
- No email notifications; no attachments/screenshots; no GitHub-issue sync
  (the store interface is clean enough to add a skill-side mirror later).
- No rate-limiting beyond an empty-description guard.
- The triage skill is a documented workflow (instructions), not unit-tested code.

## 2. Architecture

- **Pure store** — `R/functions/feedback_store.R`: SQLite-backed helpers
  (`feedback_insert`, `feedback_list`, `feedback_mark_addressed`,
  `feedback_summary`), each opening/closing its own DBI connection, returning
  structured results. No Shiny dependency → unit-testable against a temp DB.
- **Shiny module** — `R/modules/feedback_server.R` + UI built into the existing
  `app.R` dashboard shell: a header **submit** trigger + modal, and a **gated
  admin** panel.
- **Triage skill** — `.claude/skills/feedback/SKILL.md`, modelled on the local
  `deploy` skill (YAML frontmatter + workflow steps). SSH-based; operates on the
  production DB.

The store is the single contract both the app and the skill consume.

## 3. The store — `feedback_store.R`

### 3.1 Location & durability
DB at **`data/feedback/feedback.db`**. Rationale: `data/` is skipped by
`deploy-windows.ps1 -SkipData` and preserved by the `cp -rT` / per-file `scp`
deploy flow, so submissions survive deploys (unlike anything under code dirs).
A path resolver creates `data/feedback/` on first use
(`dir.create(recursive = TRUE, showWarnings = FALSE)`).

**Open item (verify at implementation):** confirm the `shiny` server user can
write to `data/feedback/` on laguna; if `data/` is not writable but `cache/` is
(the app already writes `cache/offline_traits.db`), fall back to
`cache/feedback/feedback.db`. The resolver picks the first writable of the two
and the chosen path is logged once via `warning()`.

### 3.2 Concurrency
Multiple Shiny sessions may submit at once. On every connection open:
`PRAGMA journal_mode = WAL; PRAGMA busy_timeout = 5000;` (WAL was already
identified as a needed pattern in project notes). Writes are single-row inserts;
WAL + busy_timeout makes concurrent submits safe without app-level locking.

### 3.3 Schema
Table `feedback`, created idempotently (`CREATE TABLE IF NOT EXISTS`):

| column           | type    | notes                                            |
|------------------|---------|--------------------------------------------------|
| `id`             | INTEGER | PRIMARY KEY AUTOINCREMENT                         |
| `created_at`     | TEXT    | ISO-8601 UTC, set on insert                      |
| `type`           | TEXT    | `bug` or `suggestion`                            |
| `description`    | TEXT    | required, non-empty                              |
| `submitter_email`| TEXT    | optional (NA/empty allowed)                      |
| `app_version`    | TEXT    | from the VERSION file / app version constant     |
| `tab`            | TEXT    | active sidebar tab at submit time                |
| `session_id`     | TEXT    | Shiny `session$token`                            |
| `status`         | TEXT    | `open` (default) or `addressed`                  |
| `addressed_at`   | TEXT    | ISO-8601, set when marked addressed              |
| `addressed_by`   | TEXT    | who marked it (e.g. `skill` / admin label)       |
| `fix_commit`     | TEXT    | git SHA of the fix (optional)                    |
| `resolution_note`| TEXT    | free text (optional)                             |

New columns added later must be nullable so old DBs keep working (project
convention); reads use `SELECT` of explicit columns guarded by a
`DBI::dbListFields()` intersection, mirroring `lookup_offline_traits`.

### 3.4 Functions (the contract)
```r
feedback_insert(type, description, submitter_email = NA, app_version = NA,
                tab = NA, session_id = NA, db_path = NULL)
  -> list(success, id | NA, error)        # validates type ∈ {bug,suggestion}
                                          # and non-empty description

feedback_list(status = NULL, db_path = NULL)
  -> data.frame(all columns)              # status = NULL -> all; else filter

feedback_mark_addressed(id, fix_commit = NA, resolution_note = NA,
                        addressed_by = "skill", db_path = NULL)
  -> list(success, error)                 # sets status='addressed', addressed_at

feedback_summary(db_path = NULL)
  -> list(open = n, addressed = n, total = n)
```
All wrap DB work in `tryCatch(..., error = function(e) { warning(...); <failure> })`
(production keeps no `message()` logs). `db_path = NULL` resolves §3.1; tests
pass an explicit temp path.

## 4. In-app UI — `feedback_server.R`

### 4.1 Submit (public)
- A **"Feedback"** `actionButton` (icon `comment-dots`) added to `app.R`'s
  `dashboardHeader` right-side controls.
- Opens a `bsModal` (matching the existing help/api-key modal pattern):
  `radioButtons` type (Bug / Suggestion), `textAreaInput` description (required),
  optional `textInput` email, Submit + Cancel.
- On submit: guard non-empty description (else inline warning, no insert), call
  `feedback_insert()` with auto-captured `app_version`, `tab`
  (`input$<sidebarId>`), `session_id` (`session$token`); on success → success
  `showNotification` + close modal + clear fields; on failure → warning toast.

### 4.2 Admin panel (gated)
- Rendered **only** when the URL query `?admin=<key>` (read via
  `session$clientData$url_search` / `parseQueryString`) equals env
  `FEEDBACK_ADMIN_KEY`. Non-admins never receive the panel UI (server-side gate,
  not just CSS-hidden).
- **Security note (documented limitation):** a URL key is low-stakes access
  control suitable for non-sensitive maintainer convenience, NOT real auth; the
  key travels in the URL. Acceptable for feedback management; do not reuse for
  anything sensitive.
- Panel: a `DT::datatable` of `feedback_list()` (filterable by status), a row
  selector + **"Mark addressed"** button (optional resolution note) calling
  `feedback_mark_addressed(..., addressed_by = "admin")`, and a small
  `feedback_summary()` valueBox row. Re-reads on a `reactiveVal` trigger after
  marking.

## 5. Triage skill — `.claude/skills/feedback/SKILL.md`

Local skill (YAML frontmatter `name`, `description`, `disable-model-invocation`)
modelled on `deploy`. Operates on the **production** DB over SSH (it is not in
git — prod user data). Workflow:

1. **Pull open entries:** `ssh razinka@laguna.ku.lt "sqlite3 -header -csv
   /srv/shiny-server/EcoNeTool/data/feedback/feedback.db 'SELECT id,created_at,
   type,description,tab,app_version,submitter_email FROM feedback WHERE
   status=\"open\" ORDER BY created_at'"`. (Resolve the path per §3.1; fall back
   to `cache/feedback/...`.)
2. **Present** the open entries to the user as a table.
3. **Per entry chosen for action:** reproduce/analyse; fix in the repo using
   `superpowers:test-driven-development`; commit (message references the
   feedback id, e.g. `fix(feedback#12): …`); deploy via the `deploy` skill.
4. **Mark addressed in the server store:** `ssh razinka@laguna.ku.lt "sqlite3
   /srv/shiny-server/EcoNeTool/data/feedback/feedback.db \"UPDATE feedback SET
   status='addressed', addressed_at='<ISO>', addressed_by='skill',
   fix_commit='<sha>', resolution_note='<note>' WHERE id=<id>\""`. The app's
   admin panel then shows it addressed.
5. Report a summary (addressed ids + commits; entries left open with reasons).

The skill must: never invent a fix without reproducing; never mark addressed
before the fix is committed+deployed; quote `description` text safely in SQL
(single-quote-escape) to avoid injection/breakage from user text.

## 6. Data flow
submit (public) → `feedback.db` `status=open` → admin panel / skill list open →
maintainer fixes + commits + deploys → skill (or admin) marks `addressed` in the
server DB → app reflects status.

## 7. Error handling
- All store calls return structured `list(success, …)`; UI branches on it. Submit
  failure → "Couldn't save your feedback, please try again" toast; never a crash.
- `error = function(e)` closures `warning()` (not `message()`); use `<<-` for any
  outer mutation (project conventions).
- Admin gate: missing/empty `FEEDBACK_ADMIN_KEY` env → admin panel never renders
  (fail closed). Bad/absent query key → no panel.
- Skill: a failed SSH/`sqlite3` step stops and reports; never partial-marks.

## 8. Testing
- **`feedback_store.R` (TDD core, temp DB, no Shiny):** insert→list round-trip;
  `type` validation (reject non-bug/suggestion); empty-description rejected;
  status filter; `mark_addressed` sets status/addressed_at and is reflected in
  `feedback_list("addressed")`; `feedback_summary` counts; idempotent table
  create on a fresh path; defensive read against a DB missing a later column.
  Never gate `expect_*` behind `if` (use `skip_if`).
- **Module/UI:** parse-check + an app-context smoke test (the dashboard builds
  with the header button + the gate logic; `feedback_server` sources). Reactive
  wiring not unit-tested.
- **Skill:** not unit-tested (instructions); a manual dry-run is the
  verification.

## 9. Open items (resolve at implementation)
- Server write-perm for `data/feedback/` vs `cache/feedback/` (§3.1); create +
  chmod the dir on laguna once, like the `r-libs/` setup.
- Exact sidebar input id for the active `tab` (`input$<id>` of the bs4Dash
  `sidebarMenu`).
- App version source (VERSION file vs an existing version constant).
- Where the admin panel mounts (a gated `tabItem` vs a collapsible box on an
  existing tab).

## Appendix: reference patterns
- **SQLite + DBI + defensive SELECT:** `R/functions/trait_lookup/orchestrator.R`
  (`lookup_offline_traits`, `dbListFields` guard); `cache/offline_traits.db`
  usage in `R/modules/rpath_server.R` (`offline_db_*`).
- **CSV logging precedent:** `R/functions/error_logging.R`.
- **Modal pattern:** the help / API-keys `bsModal` in `R/ui/trait_research_ui.R`.
- **Local skill format:** `.claude/skills/deploy/SKILL.md`.
- **Deploy persistence of `data/`:** project deploy notes (`-SkipData`, `cp -rT`).
- **Conventions (`warning()`/`<<-`/`skip_if`, nullable new columns):** `CLAUDE.md`.
