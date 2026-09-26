# Deep-analysis 2026-09 remediation: overview

- **Status:** Draft, awaiting user review.
- **Date:** 2026-09-26
- **Source:** `docs/econetool-deep-analysis-2026-09-26.md` (84 findings; 12 remediation batches).

The 12 report batches are grouped into five sub-projects. Each has its own spec, and gets its own
implementation plan just before it is executed so line numbers stay current.

| Sub-project | Spec | Report batches | Findings in scope |
|---|---|---|---|
| A. Network science correctness | `2026-09-26-fix-a-network-science-correctness-design.md` | 1, 2, 7 | 18 + N1-N3 (new) |
| B. Platform safety | `2026-09-26-fix-b-platform-safety-design.md` | 3, 4, 11 | 18 |
| C. Trait pipeline correctness | `2026-09-26-fix-c-trait-pipeline-correctness-design.md` | 5, 6, 8 | 27 |
| D. Spatial state + conventions | `2026-09-26-fix-d-spatial-state-and-conventions-design.md` | 10, 12 | 16 + F23b (new) |
| E. Rpath dynamics | `2026-09-26-fix-e-rpath-dynamics-design.md` | 9 | 5 |

Every finding was re-verified against `master` @ `7a96789` while the specs were written. None was
dropped. Each spec's appendix records narrowed or corrected sub-claims.

## Decisions (made by the user, binding on all specs)

- **Order:** F1 hotfix first, then A through E.
- **MTI/KS:** Ulanowicz-Puccia MTI with Libralato keystoneness. FC uses Q/B x B when present and a
  biomass proxy otherwise.
- **Admin gating:** "session-only for users". Sliders work per session. Saving the server-default
  config and rebuilding the offline DB require the admin password.
- **Dead UI:** wire the control if its backend exists; otherwise remove it.
- **Trophic levels:** non-converged or unreachable nodes return `NA` plus `warning()`.
- **Edge contract:** prey -> predator everywhere (`trophic_levels.R:55-58`). Four of the five bundled
  metawebs have swapped CSV columns, which a one-off migration script fixes in A1.

## Merge order and versions

| # | PR | Version | Depends on |
|---|---|---|---|
| 1 | B0: F1/F76 harmonization loop hotfix | 1.4.5 | - |
| 2 | A1: edge contract + MTI/KS + TL (atomic) | - | B0 |
| 3 | A2: Rpath TL and diagnostics | - | B0 |
| 4 | A3: finalize and import | 1.5.0 (release after A3) | A1 |
| 5 | B2: deploy and CI hardening | patch | A |
| 6 | B1: harmonization settings safety | patch | B0, A |
| 7 | B3: security and admin gating | patch | B1 |
| 8 | C (three PRs, one per batch) | 1.6.0 | B (admin-gated DB rebuild, config-hash cache) |
| 9 | D1 spatial state | patch | A1 (shares `spatial_analysis.R`) |
| 10 | D2 conventions sweep | patch | C (shares trait lookup files) |
| 11 | E1 Rpath params schema | 1.7.0 | A2 |
| 12 | E2, E3 simulation and edit loop | patch | E1 |

`VERSION` reads 1.4.4 today, while `R/config.R:294` reads 1.4.2 and the CHANGELOG head reads 1.4.3.
B0 aligns all three.

## Shared rules for every PR

- TDD: the failing known-answer or regression test comes before each fix.
- Run `testthat::test_dir("tests/testthat")` plus a parse check per edited file. The legacy
  `tests/run_all_tests.R` needs `dggridR` and cannot run locally.
- Re-check each cited line against the current code before editing; earlier PRs move lines.
- Follow the CLAUDE.md conventions: `warning()` in error handlers, `<<-` in error closures,
  `app_path()`, `get_harm_config()`, `skip_if()` instead of if-gated expectations.
- Deploy after A and after B. Until B2 lands, clear `/home/razinka/EcoNeTool_staging` before each
  upload, because `cp -rT` would otherwise copy stale files from earlier deploys (spec B, F5).

## Questions for the user during spec review

1. **Keystone rule (A).** Libralato KS is log-scale, so `KS > 1` no longer works. The proposal is
   "top quartile of KS" with p < 0.05 for Keystone. The alternative is plain ranking with no status.
2. **Zonation words (C).** "subtidal", "sublittoral", "intertidal" and "littoral" would no longer set
   an EP code; taxonomy or depth rules decide instead.
3. **Mobility vocabulary (C).** MB codes are unified on the food-web model's meaning (MB2 = passive
   floater).
4. **Placeholder calibration (C).** New MS1/MS6 matrix columns and the PR1 row copy their neighbours;
   new database weights are judgement calls.
5. **Threshold semantics (C).** Switching to a strict `p > threshold` at the default 0.05 removes
   links sitting exactly at the "implausible" floor.
