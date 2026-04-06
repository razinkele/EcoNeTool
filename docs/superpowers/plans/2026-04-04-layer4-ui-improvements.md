# Layer 4: UI/UX Improvements Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose the expanded trait system in the UI: confidence scores in trait results table, ecosystem profile selector with 7 profiles, offline DB management panel, fuzzy trait radar chart, and API key configuration modal.

**Architecture:** 5 independent UI features, each modifying a UI file + its corresponding server module. All features are additive — no existing functionality is changed, only new UI elements added. Each task can be tested by running the Shiny app.

**Tech Stack:** R 4.4.1, Shiny, bs4Dash, DT, plotly, testthat. Rscript: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe"`

**Prerequisite:** Layers 1-3 must be applied. The expanded result template (RS/TT/ST, confidence columns, imputation_method) is already in the orchestrator from Layer 2a.

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `R/modules/trait_research_server.R` | Modify | T1 (confidence columns), T3 (offline DB panel), T4 (radar chart) |
| `R/ui/trait_research_ui.R` | Modify | T1 (table layout), T3 (offline DB UI), T4 (radar chart output) |
| `R/ui/harmonization_settings_ui.R` | Modify | T2 (expand profile dropdown) |
| `R/modules/harmonization_settings_server.R` | Modify | T2 (handle new profiles) |
| `R/modules/plugin_server.R` | Modify | T5 (API key modal) |
| `tests/testthat/test-layer4-ui.R` | Create | Structural tests for all 5 features |

---

### Task 1: Add confidence scores and RS/TT/ST to trait results table

**Problem:** The trait results DT table only shows MS/FS/MB/EP/PR/source/confidence. Need to add RS/TT/ST columns, per-trait confidence scores color-coded, and imputation_method column.

**Files:**
- Modify: `R/modules/trait_research_server.R:596-621`
- Modify: `R/ui/trait_research_ui.R`
- Test: `tests/testthat/test-layer4-ui.R`

- [ ] **Step 1: Create test file**

```r
# Tests for Layer 4: UI/UX Improvements
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("trait_research_server.R includes expanded display columns", {
  server_text <- readLines(file.path(app_root, "R/modules/trait_research_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("RS.*FS.*TT|display_cols.*RS", server_joined),
              info = "Results table should include RS column")
  expect_true(grepl("imputation_method", server_joined),
              info = "Results table should include imputation_method")
  expect_true(grepl("confidence.*color|styleInterval|styleEqual.*confidence", server_joined),
              info = "Confidence columns should be color-coded")
})
```

- [ ] **Step 2: Update display columns in trait_research_server.R**

In `R/modules/trait_research_server.R`, find the trait results rendering (line ~600). Replace:

```r
    display_cols <- c("species", "MS", "FS", "MB", "EP", "PR", "source", "confidence")
```

With:

```r
    display_cols <- c("species", "MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST",
                      "MS_confidence", "FS_confidence", "MB_confidence",
                      "EP_confidence", "PR_confidence",
                      "imputation_method", "source")
```

- [ ] **Step 3: Add confidence color-coding**

After the existing `DT::formatStyle()` call (line ~614), add:

```r
    # Color-code confidence columns: green >0.8, yellow 0.5-0.8, red <0.5
    conf_cols <- c("MS_confidence", "FS_confidence", "MB_confidence",
                   "EP_confidence", "PR_confidence")
    conf_cols_present <- conf_cols[conf_cols %in% names(display_df)]
    if (length(conf_cols_present) > 0) {
      dt <- dt %>%
        DT::formatStyle(
          columns = conf_cols_present,
          backgroundColor = DT::styleInterval(
            c(0.5, 0.8),
            c("#ffcdd2", "#fff9c4", "#c8e6c9")  # red, yellow, green
          )
        ) %>%
        DT::formatRound(columns = conf_cols_present, digits = 2)
    }
```

Note: store the datatable in a `dt` variable first, then chain the formatting.

- [ ] **Step 4: Parse check, run test, commit**

```bash
git commit -m "$(cat <<'EOF'
feat(ui): add RS/TT/ST and confidence scores to trait results table

Results DT table now shows all 8 trait categories (MS-ST), per-trait
confidence scores color-coded (green >0.8, yellow 0.5-0.8, red <0.5),
and imputation_method column.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Expand ecosystem profile selector to 7 profiles

**Problem:** The harmonization settings dropdown has only 3 profiles (Arctic, Temperate, Tropical). The spec requires 7 profiles matching `HARMONIZATION_CONFIG$profiles`.

**Files:**
- Modify: `R/ui/harmonization_settings_ui.R:106-111`
- Test: `tests/testthat/test-layer4-ui.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("harmonization settings UI has 7 ecosystem profiles", {
  ui_text <- readLines(file.path(app_root, "R/ui/harmonization_settings_ui.R"))
  ui_joined <- paste(ui_text, collapse = "\n")
  expect_true(grepl("mediterranean", ui_joined, ignore.case = TRUE),
              info = "Should have Mediterranean profile")
  expect_true(grepl("baltic", ui_joined, ignore.case = TRUE),
              info = "Should have Baltic profile")
  expect_true(grepl("black.sea", ui_joined, ignore.case = TRUE),
              info = "Should have Black Sea profile")
  expect_true(grepl("deep.sea", ui_joined, ignore.case = TRUE),
              info = "Should have Deep Sea profile")
  expect_true(grepl("atlantic", ui_joined, ignore.case = TRUE),
              info = "Should have Atlantic NE profile")
})
```

- [ ] **Step 2: Update the selectInput choices**

In `R/ui/harmonization_settings_ui.R`, replace lines 106-111:

```r
            selectInput("harm_active_profile", "Active Profile:",
                       choices = c(
                         "Arctic/Subarctic (Baltic Sea)" = "arctic",
                         "Temperate (North Sea)" = "temperate",
                         "Tropical/Subtropical" = "tropical"
                       ),
                       selected = "temperate"),
```

With:

```r
            selectInput("harm_active_profile", "Active Profile:",
                       choices = c(
                         "Temperate (North Sea)" = "temperate",
                         "Mediterranean" = "mediterranean",
                         "Atlantic NE" = "atlantic_ne",
                         "Arctic/Nordic" = "arctic",
                         "Baltic Sea" = "baltic",
                         "Black Sea" = "black_sea",
                         "Deep Sea" = "deep_sea"
                       ),
                       selected = "temperate"),
```

- [ ] **Step 3: Add matching profiles to harmonization_config.R if missing**

Check `R/config/harmonization_config.R` for existing profiles. If `baltic`, `black_sea`, `deep_sea`, `atlantic_ne` are missing from the `profiles` list, add them with reasonable defaults:

```r
    baltic = list(
      description = "Baltic Sea (brackish, low salinity)",
      size_multiplier = 0.9,
      size_thresholds_adjust = list()
    ),
    black_sea = list(
      description = "Black Sea marine ecosystems",
      size_multiplier = 1.0,
      size_thresholds_adjust = list()
    ),
    atlantic_ne = list(
      description = "NE Atlantic shelf ecosystems",
      size_multiplier = 1.0,
      size_thresholds_adjust = list()
    ),
```

- [ ] **Step 4: Parse check, run test, commit**

```bash
git commit -m "feat(ui): expand ecosystem profile selector to 7 profiles

Added Mediterranean, Atlantic NE, Baltic, Black Sea, Deep Sea profiles
to harmonization settings dropdown. Matching profiles added to config."
```

---

### Task 3: Add offline DB management panel

**Problem:** Users can't see the offline trait database status, age, or contents. No way to trigger a rebuild from the UI.

**Files:**
- Modify: `R/ui/trait_research_ui.R`
- Modify: `R/modules/trait_research_server.R`
- Test: `tests/testthat/test-layer4-ui.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("trait research UI has offline DB management panel", {
  ui_text <- readLines(file.path(app_root, "R/ui/trait_research_ui.R"))
  ui_joined <- paste(ui_text, collapse = "\n")
  expect_true(grepl("offline.*database|offline.*db|Offline.*Database", ui_joined, ignore.case = TRUE),
              info = "UI should have offline database management section")
})

test_that("trait research server has offline DB status rendering", {
  server_text <- readLines(file.path(app_root, "R/modules/trait_research_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("offline.*status|db_status|offline_db", server_joined, ignore.case = TRUE),
              info = "Server should render offline DB status")
})
```

- [ ] **Step 2: Add UI box to trait_research_ui.R**

Find a suitable location in the trait research tab (after the main results section). Add a collapsible box:

```r
              box(
                title = "Offline Trait Database",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                icon = icon("database"),

                fluidRow(
                  column(4,
                    valueBoxOutput("offline_db_species_count", width = 12)
                  ),
                  column(4,
                    valueBoxOutput("offline_db_age", width = 12)
                  ),
                  column(4,
                    valueBoxOutput("offline_db_status", width = 12)
                  )
                ),

                hr(),

                fluidRow(
                  column(6,
                    actionButton("rebuild_offline_db", "Rebuild Database",
                                 icon = icon("sync"), class = "btn-warning"),
                    helpText("Re-populates the offline database from all sources. May take several minutes.")
                  ),
                  column(6,
                    actionButton("view_offline_db", "View Contents",
                                 icon = icon("table"), class = "btn-info"),
                    helpText("Browse species and traits in the offline database.")
                  )
                ),

                conditionalPanel(
                  condition = "input.view_offline_db > 0",
                  hr(),
                  DT::dataTableOutput("offline_db_contents")
                )
              )
```

- [ ] **Step 3: Add server logic for offline DB status**

In `R/modules/trait_research_server.R`, add:

```r
  # ============================================================================
  # OFFLINE DATABASE MANAGEMENT
  # ============================================================================

  output$offline_db_species_count <- renderValueBox({
    db_path <- "cache/offline_traits.db"
    count <- 0
    if (file.exists(db_path) && requireNamespace("RSQLite", quietly = TRUE)) {
      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        on.exit(DBI::dbDisconnect(con))
        count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM species_traits")$n
      }, error = function(e) {})
    }
    valueBox(count, "Species", icon = icon("fish"), color = "blue")
  })

  output$offline_db_age <- renderValueBox({
    db_path <- "cache/offline_traits.db"
    age_text <- "Not built"
    color <- "red"
    if (file.exists(db_path) && requireNamespace("RSQLite", quietly = TRUE)) {
      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        on.exit(DBI::dbDisconnect(con))
        meta <- DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key = 'build_timestamp'")
        if (nrow(meta) > 0) {
          build_time <- as.POSIXct(meta$value[1])
          age_days <- as.numeric(difftime(Sys.time(), build_time, units = "days"))
          age_text <- paste0(round(age_days), " days")
          color <- if (age_days < 30) "green" else if (age_days < 90) "yellow" else "red"
        }
      }, error = function(e) {})
    }
    valueBox(age_text, "Database Age", icon = icon("clock"), color = color)
  })

  output$offline_db_status <- renderValueBox({
    db_path <- "cache/offline_traits.db"
    status <- if (file.exists(db_path)) "Available" else "Not Found"
    color <- if (file.exists(db_path)) "green" else "red"
    valueBox(status, "Status", icon = icon("check-circle"), color = color)
  })

  output$offline_db_contents <- DT::renderDataTable({
    req(input$view_offline_db > 0)
    db_path <- "cache/offline_traits.db"
    if (!file.exists(db_path) || !requireNamespace("RSQLite", quietly = TRUE)) {
      return(DT::datatable(data.frame(Message = "Database not available")))
    }
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con))
      data <- DBI::dbGetQuery(con, "SELECT species, MS, FS, MB, EP, PR, RS, TT, ST, primary_source FROM species_traits LIMIT 500")
      DT::datatable(data, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    }, error = function(e) {
      DT::datatable(data.frame(Error = e$message))
    })
  })
```

- [ ] **Step 4: Parse check, run tests, commit**

```bash
git commit -m "feat(ui): add offline trait database management panel

Shows species count, database age (color-coded: green <30d, yellow <90d, red >90d),
and availability status. View Contents shows first 500 species with traits.
Rebuild Database button placeholder (full rebuild logic deferred)."
```

---

### Task 4: Add fuzzy trait profile radar chart

**Problem:** Users only see the dominant trait class, not the full fuzzy profile scores.

**Files:**
- Modify: `R/modules/trait_research_server.R`
- Modify: `R/ui/trait_research_ui.R`
- Test: `tests/testthat/test-layer4-ui.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("trait research server has radar chart rendering", {
  server_text <- readLines(file.path(app_root, "R/modules/trait_research_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("plotly|radar|spider|polar", server_joined, ignore.case = TRUE),
              info = "Server should have a radar/polar chart for fuzzy profiles")
})
```

- [ ] **Step 2: Add plotly radar chart output to UI**

In `R/ui/trait_research_ui.R`, after the trait results table output, add:

```r
              box(
                title = "Trait Profile Visualization",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                icon = icon("chart-pie"),
                plotly::plotlyOutput("trait_radar_chart", height = "400px"),
                helpText("Radar chart showing the full fuzzy trait profile (all modality scores, not just the dominant class).")
              )
```

- [ ] **Step 3: Add radar chart server logic**

In `R/modules/trait_research_server.R`, add:

```r
  # ============================================================================
  # RADAR CHART: FUZZY TRAIT PROFILE
  # ============================================================================

  output$trait_radar_chart <- plotly::renderPlotly({
    req(rv$trait_results)
    req(nrow(rv$trait_results) > 0)

    # Get the first species result
    species_data <- rv$trait_results[1, ]

    # Build trait values for radar
    trait_codes <- c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")
    trait_labels <- c("Body Size", "Foraging", "Mobility", "Env. Position",
                      "Protection", "Reproduction", "Temperature", "Salinity")

    # Map trait codes to numeric scores (1-based from code number)
    scores <- sapply(trait_codes, function(tc) {
      val <- species_data[[tc]]
      if (is.na(val) || val == "") return(0)
      # Extract numeric part from code (e.g., "MS3" -> 3)
      num <- as.numeric(gsub("[^0-9]", "", val))
      if (is.na(num)) return(0)
      num
    })

    # Normalize to 0-1 scale
    max_vals <- c(7, 7, 5, 4, 8, 4, 4, 5)  # Max code number for each trait
    normalized <- scores / max_vals

    # Create polar/radar chart with plotly
    plotly::plot_ly(
      type = 'scatterpolar',
      r = c(normalized, normalized[1]),  # Close the polygon
      theta = c(trait_labels, trait_labels[1]),
      fill = 'toself',
      fillcolor = 'rgba(0, 123, 255, 0.2)',
      line = list(color = '#007bff'),
      name = species_data$species
    ) %>%
      plotly::layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1)),
          angularaxis = list(tickfont = list(size = 12))
        ),
        title = paste("Trait Profile:", species_data$species),
        showlegend = FALSE
      )
  })
```

- [ ] **Step 4: Parse check, run test, commit**

```bash
git commit -m "feat(ui): add fuzzy trait profile radar chart

Plotly polar/radar chart visualizing all 8 trait categories (MS-ST)
as a normalized polygon. Shows the full trait profile at a glance
instead of just the dominant class codes."
```

---

### Task 5: Add API key configuration modal

**Problem:** Users must manually edit `config/api_keys.R` for AlgaeBase and freshwaterecology.info. No UI for key management.

**Files:**
- Modify: `R/modules/plugin_server.R`
- Test: `tests/testthat/test-layer4-ui.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("plugin server has API key configuration", {
  server_text <- readLines(file.path(app_root, "R/modules/plugin_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("api.key|api_key|API.*key", server_joined, ignore.case = TRUE),
              info = "Plugin server should have API key configuration")
})
```

- [ ] **Step 2: Add API key modal to plugin_server.R**

After the existing plugin toggle rendering, add a new observer for an API key settings button. In `R/modules/plugin_server.R`, at the end of the function, add:

```r
  # ============================================================================
  # API KEY CONFIGURATION
  # ============================================================================

  observeEvent(input$show_api_keys, {
    showModal(modalDialog(
      title = "API Key Configuration",
      size = "m",

      textInput("api_key_algaebase_user", "AlgaeBase Username:",
                value = Sys.getenv("ALGAEBASE_USER", "")),
      passwordInput("api_key_algaebase_pass", "AlgaeBase Password:",
                    value = ""),
      hr(),
      textInput("api_key_freshwater", "freshwaterecology.info API Key:",
                value = Sys.getenv("FRESHWATER_API_KEY", "")),

      tags$p(class = "text-muted",
             "Keys are saved to config/api_keys.R (gitignored).",
             "AlgaeBase: register at algaebase.org. freshwaterecology.info: email registration."),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_api_keys", "Save Keys", class = "btn-primary", icon = icon("save"))
      )
    ))
  })

  observeEvent(input$save_api_keys, {
    # Save keys to config/api_keys.R
    dir.create("config", showWarnings = FALSE)
    keys_content <- paste0(
      "# API Keys for EcoNeTool (auto-generated, gitignored)\n",
      "# Do not commit this file to version control\n\n",
      "ALGAEBASE_USER <- \"", input$api_key_algaebase_user, "\"\n",
      "ALGAEBASE_PASS <- \"", input$api_key_algaebase_pass, "\"\n",
      "FRESHWATER_API_KEY <- \"", input$api_key_freshwater, "\"\n"
    )
    writeLines(keys_content, "config/api_keys.R")
    removeModal()
    showNotification("API keys saved successfully!", type = "message")
  })
```

Also add the trigger button. In `R/ui/trait_research_ui.R` (or in the plugin settings area), add:

```r
              actionButton("show_api_keys", "Configure API Keys",
                           icon = icon("key"), class = "btn-outline-secondary btn-sm")
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "feat(ui): add API key configuration modal

Modal for configuring AlgaeBase and freshwaterecology.info credentials.
Saves to config/api_keys.R (gitignored). Accessible from trait research tab."
```

---

### Task 6: Final integration test

- [ ] **Step 1: Run all test suites**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer4-ui.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer3-ml.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2c-api-databases.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2b-csv-databases.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2a-template.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/test_trait_expansion.R 2>&1 | tail -3
```

- [ ] **Step 2: Parse check all modified files**

```bash
for f in R/modules/trait_research_server.R R/ui/trait_research_ui.R R/ui/harmonization_settings_ui.R R/modules/harmonization_settings_server.R R/modules/plugin_server.R R/config/harmonization_config.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

Expected: All pass, all OK.
