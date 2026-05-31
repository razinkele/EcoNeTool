# BIAS acoustic survey indices

`bias_indices.csv` holds the ICES **BIAS** (Baltic International Acoustic
Survey, autumn) abundance index series for the clupeid Ecopath groups that
BITS bottom-trawl cannot index: herring (AphiaID 126417, stock
`her.27.25-2932`) and sprat (AphiaID 126425, stock `spr.27.22-32`).

BIAS has no clean web-service API (unlike DATRAS/BITS). The series are
transcribed by hand from a citeable WGBIFS / WGBFAS report and committed here
as a versioned snapshot. The `source` column records the **specific report +
table** per row (not just a portal URL).

It lives in `R/config/` (not `data/`) because the laguna deploy runs
`deploy-windows.ps1 -SkipData`, which skips `data/`; a file there would never
reach the server and the BIAS path would stay dormant.

## Schema

| column            | type    | notes                                          |
|-------------------|---------|------------------------------------------------|
| `aphia_id`        | integer | 126417 (herring) or 126425 (sprat)             |
| `stock`           | text    | ICES stock key (summing is scoped to one stock) |
| `sd`              | text    | subdivision label                              |
| `year`            | integer | survey year                                    |
| `abundance_index` | numeric | acoustic abundance index (>= 0, clean numeric) |
| `unit`            | text    | documentation only (trend-only; not used in math) |
| `source`          | text    | per-row citation: report + table               |

`aggregate_bias_series()` only reads `aphia_id` / `stock` / `year` /
`abundance_index`; `sd` / `unit` / `source` are required by the schema guard
for provenance, not by the math.

## Invariants (correctness depends on these)

- **Single stock per aphia_id.** All rows for an AphiaID must belong to ONE ICES
  stock. AphiaID 126417 spans several herring stocks (her.27.25-2932,
  her.27.28 Gulf of Riga, her.27.20-24 Western Baltic); the reader sums per
  stock, so mixing stocks would double-count across management units.
- **Non-overlapping subdivisions.** Rows summed for a (stock, year) must be
  mutually exclusive SDs. Herring SD 28 = **28.2 only** (open central Baltic);
  exclude **28.1 (Gulf of Riga)** which belongs to her.27.28.
- **Sprat SD 22-24** autumn BIAS coverage is partial; note the exact survey in
  `source` and do not assume one BIAS series cleanly spans the whole
  spr.27.22-32 area.

## Refreshing

When WGBIFS/WGBFAS publishes a new report, append/replace rows and update the
`source`. Header-only state is valid: the BIAS panel then falls back to ICES
SAG SSB (same as before this feature).

## Sources

- ICES acoustic data portal: https://acoustic.ices.dk
- WGBIFS report (Thünen mirror): https://literatur.thuenen.de/digbib_extern/dn066592.pdf
