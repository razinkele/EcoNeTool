#' Dashboard Tab UI
#'
#' Creates the dashboard tab showing overview statistics and quick start guide.
#'
#' @return A tabItem for the dashboard
dashboard_ui <- function() {
      # ========================================================================
      # DASHBOARD TAB
      # ========================================================================
      tabItem(
        tabName = "dashboard",

        # Summary value boxes (dynamic)
        fluidRow(
          valueBoxOutput("box_species", width = 3),
          valueBoxOutput("box_links", width = 3),
          valueBoxOutput("box_groups", width = 2),
          valueBoxOutput("box_period", width = 2),
          valueBoxOutput("box_location", width = 2)
        ),

        # Functional Groups and Quick Start
        fluidRow(
          box(
            title = "Functional Groups",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            HTML("
                <p><span style='color: orange; font-size: 20px;'>●</span> <strong>Benthos</strong> - Bottom-dwelling organisms</p>
                <p><span style='color: darkgrey; font-size: 20px;'>●</span> <strong>Detritus</strong> - Organic matter</p>
                <p><span style='color: blue; font-size: 20px;'>●</span> <strong>Fish</strong> - Fish species</p>
                <p><span style='color: green; font-size: 20px;'>●</span> <strong>Phytoplankton</strong> - Primary producers</p>
                <p><span style='color: cyan; font-size: 20px;'>●</span> <strong>Zooplankton</strong> - Small drifting organisms</p>
                  <h4>Ecological Interaction Network Explorer</h4>
            ")
          ),
          box(
            title = "Quick Start",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            HTML("
              <div style='padding: 10px;'>
                <ol>
                  <li><strong>Food Web Network:</strong> Explore the interactive network visualization</li>
                  <li><strong>Topological Metrics:</strong> View structural properties of the food web</li>
                  <li><strong>Biomass Analysis:</strong> Examine biomass-weighted metrics</li>
                  <li><strong>Energy Fluxes:</strong> Analyze energy flow patterns</li>
                </ol>
                <p style='margin-top: 15px;'><em>Click on the sidebar menu items to navigate!</em></p>
              </div>
            ")
          )
        ),

        # Welcome box with detailed information
        fluidRow(
          box(
            title = "🌊 Welcome to EcoNeTool - Marine Food Web Network Analysis Tool",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            HTML(paste0(
              "<div style='font-size: 15px; line-height: 1.8;'>
                <p><strong>Interactive Shiny Dashboard</strong> for analyzing trophic interactions, biomass distributions,
                and energy fluxes in marine ecosystems</p>

                <hr style='border-top: 2px solid #3c8dbc; margin: 20px 0;'>

                <h4>📊 Analysis Features</h4>

                <div style='background-color: #f4f4f4; padding: 15px; border-radius: 5px; margin: 15px 0;'>
                  <h5 style='color: #3c8dbc; margin-top: 0;'>🕸️ Network Visualization</h5>
                  <ul style='margin-bottom: 0;'>
                    <li><strong>Interactive Graphs:</strong> Force-directed and trophic-level layouts with zoom/pan</li>
                    <li><strong>Color Coding:</strong> By functional groups (Benthos, Detritus, Fish, Phytoplankton, Zooplankton)</li>
                    <li><strong>Node Sizing:</strong> Proportional to species biomass</li>
                    <li><strong>Edge Weights:</strong> Interaction strength or energy flux visualization</li>
                  </ul>
                </div>

                <div style='background-color: #f4f4f4; padding: 15px; border-radius: 5px; margin: 15px 0;'>
                  <h5 style='color: #00a65a; margin-top: 0;'>📈 Topological Metrics</h5>
                  <ul style='margin-bottom: 0;'>
                    <li><code>trophiclevels()</code> - Iterative calculation of trophic positions</li>
                    <li><code>get_topological_indicators()</code> - Network structure analysis (S, C, G, V, TL, Omnivory)</li>
                    <li><strong>Species Richness:</strong> Total number of taxa in the food web</li>
                    <li><strong>Connectance:</strong> Proportion of realized trophic links</li>
                    <li><strong>Generality & Vulnerability:</strong> Mean prey/predator counts</li>
                  </ul>
                </div>

                <div style='background-color: #f4f4f4; padding: 15px; border-radius: 5px; margin: 15px 0;'>
                  <h5 style='color: #f39c12; margin-top: 0;'>⚖️ Biomass-Weighted Analysis</h5>
                  <ul style='margin-bottom: 0;'>
                    <li><code>get_node_weighted_indicators()</code> - Quantitative metrics accounting for biomass</li>
                    <li><strong>Node-Weighted Connectance:</strong> Biomass-adjusted link density</li>
                    <li><strong>Node-Weighted Generality/Vulnerability:</strong> Importance-scaled feeding patterns</li>
                    <li><strong>Biomass Distributions:</strong> Size spectrum and functional group analysis</li>
                  </ul>
                </div>

                <div style='background-color: #f4f4f4; padding: 15px; border-radius: 5px; margin: 15px 0;'>
                  <h5 style='color: #dd4b39; margin-top: 0;'>⚡ Energy Flux Analysis</h5>
                  <ul style='margin-bottom: 0;'>
                    <li><code>calculate_losses()</code> - Metabolic losses via allometric scaling
                      <br><em style='font-size: 12px;'>Brown et al. (2004) 'Toward a metabolic theory of ecology', Ecology</em></li>
                    <li><code>get_fluxweb_results()</code> - Energy flux calculations using metabolic theory
                      <br><em style='font-size: 12px;'>Gauzens et al. (2019) 'fluxweb: An R package to easily estimate energy fluxes in food webs', Methods in Ecology and Evolution</em></li>
                    <li><code>fluxind()</code> - Link-weighted indicators (lwC, lwG, lwV) via Shannon diversity
                      <br><em style='font-size: 12px;'>Bersier et al. (2002) 'Quantitative descriptors of food web matrices', Ecology</em></li>
                    <li><strong>Temperature-Adjusted:</strong> T=3.5°C for Gulf of Riga conditions
                      <br><em style='font-size: 12px;'>Gillooly et al. (2001) 'Effects of size and temperature on metabolic rate', Science</em></li>
                    <li><strong>Flux Units:</strong> kJ/day/km² with log-scale visualization
                      <br><em style='font-size: 12px;'>Barnes et al. (2018) 'Energy flux: The link between multitrophic biodiversity and ecosystem functioning', Trends in Ecology &amp; Evolution</em></li>
                  </ul>
                </div>

                <div style='background-color: #f4f4f4; padding: 15px; border-radius: 5px; margin: 15px 0;'>
                  <h5 style='color: #605ca8; margin-top: 0;'>🔑 Keystone Species Identification</h5>
                  <ul style='margin-bottom: 0;'>
                    <li><code>calculate_mti()</code> - Mixed Trophic Impact matrix (ECOPATH method)</li>
                    <li><code>calculate_keystoneness()</code> - Keystoneness index (impact/biomass ratio)</li>
                    <li><strong>Species Classification:</strong> Keystone, Dominant, or Rare</li>
                    <li><strong>Impact Visualization:</strong> Heatmaps showing direct & indirect effects</li>
                    <li><em>Reference: Libralato et al. (2006), Ecological Modelling</em></li>
                  </ul>
                </div>

                <div style='background-color: #f4f4f4; padding: 15px; border-radius: 5px; margin: 15px 0;'>
                  <h5 style='color: #00c0ef; margin-top: 0;'>🎨 Visualization Functions</h5>
                  <ul style='margin-bottom: 0;'>
                    <li><code>plotfw()</code> - Food web plotting with trophic level layout</li>
                    <li><strong>Interactive Networks:</strong> visNetwork integration with hover tooltips</li>
                    <li><strong>Static Plots:</strong> Publication-quality figures with ggplot2-style</li>
                    <li><strong>Export Options:</strong> Download plots and data tables</li>
                  </ul>
                </div>

                <hr style='border-top: 2px solid #3c8dbc; margin: 20px 0;'>

                <h4>📊 Current Dataset</h4>
                <div style='background-color: #e8f4f8; padding: 15px; border-left: 4px solid #3c8dbc; margin: 15px 0;'>
                  <ul style='margin: 0;'>
                    <li><strong>Source:</strong> Gulf of Riga food web (Frelat & Kortsch, 2020)</li>
                    <li><strong>Location:</strong> Baltic Sea, Gulf of Riga</li>
                    <li><strong>Period:</strong> 1979-2016 (37 years)</li>
                    <li><strong>Taxa:</strong> 34 species across 5 functional groups</li>
                    <li><strong>Links:</strong> 207 trophic interactions</li>
                    <li><strong>Data Type:</strong> Temporally-averaged food web</li>
                  </ul>
                </div>

                <hr style='border-top: 1px solid #ddd; margin: 20px 0;'>

                <h4>🚀 Getting Started</h4>
                <ol style='font-size: 14px;'>
                  <li><strong>Explore the Network:</strong> Navigate to <em>Food Web Network</em> tab to see interactive visualization</li>
                  <li><strong>View Metrics:</strong> Check <em>Topological Metrics</em> for structural properties</li>
                  <li><strong>Analyze Biomass:</strong> Visit <em>Biomass Analysis</em> for node-weighted indicators</li>
                  <li><strong>Energy Flows:</strong> Examine <em>Energy Fluxes</em> for metabolic theory-based calculations</li>
                  <li><strong>Find Keystones:</strong> Use <em>Keystoneness Analysis</em> to identify key species</li>
                  <li><strong>Import Data:</strong> Upload your own food web via <em>Data Import</em> tab</li>
                </ol>

                <hr style='border-top: 1px solid #ddd; margin: 20px 0;'>

                <div style='background-color: #fff3cd; padding: 12px; border-left: 4px solid #f39c12; margin: 15px 0;'>
                  <strong>📝 ",
              get_version("full"),
              "</strong><br>
                  <small>Released: ",
              ECONETOOL_VERSION$RELEASE_DATE,
              "</small><br>
                  <small>
                    • 4 new database integrations (SeaLifeBase, freshwaterecology.info, AlgaeBase, SHARK)<br>
                    • Detailed progress tracking for automated trait lookup<br>
                    • Dedicated Automated Lookup tab with comprehensive database info<br>
                    • Centralized version management system
                  </small>
                </div>

                <p style='text-align: center; margin-top: 20px; font-size: 13px; color: #666;'>
                  <strong>Scientific Foundations:</strong> Brown et al. (2004) • Bersier et al. (2002) •
                  Libralato et al. (2006) • Williams & Martinez (2004) • Gauzens et al. (2019)
                </p>
              </div>"
            ))
          )
        )
      )

      # ========================================================================
}
