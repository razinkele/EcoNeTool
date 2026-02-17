# Helper functions for generating help content in Trait Food Web module

#' Generate Quick Start HTML content
#' @export
generate_trait_help_quickstart <- function() {
  HTML("
    <div class='well'>
      <h5>Getting Started in 5 Steps</h5>
      <ol>
        <li><strong>Load Data:</strong> Go to 'Data Input' tab and choose a method:
          <ul>
            <li><strong>Automated Lookup:</strong> Enter species names, select databases, click 'Run'</li>
            <li><strong>Upload CSV:</strong> Prepare a CSV file with trait codes</li>
            <li><strong>Example Dataset:</strong> Try pre-built examples first</li>
            <li><strong>Manual Entry:</strong> Create a blank template to fill in</li>
          </ul>
        </li>
        <li><strong>Review Data:</strong> Go to 'Trait Editor' tab
          <ul>
            <li>Click cells to edit trait assignments</li>
            <li>Click 'Validate Trait Data' to check for errors</li>
            <li>Review statistics and trait distributions</li>
          </ul>
        </li>
        <li><strong>Set Threshold:</strong> Go to 'Network' tab
          <ul>
            <li>Adjust probability threshold slider (recommended: 0.05-0.15)</li>
            <li>Lower threshold = more interactions (denser network)</li>
            <li>Higher threshold = fewer interactions (sparser network)</li>
          </ul>
        </li>
        <li><strong>Construct Network:</strong> Click 'Construct Food Web'
          <ul>
            <li>Wait for calculation (instant for <20 species)</li>
            <li>View network visualization</li>
            <li>Check network properties (connectance should be 0.1-0.3)</li>
          </ul>
        </li>
        <li><strong>Analyze & Export:</strong>
          <ul>
            <li>Explore probability heatmap</li>
            <li>Review probability matrices ('Probability Matrices' tab)</li>
            <li>Download adjacency matrix or network object</li>
          </ul>
        </li>
      </ol>
    </div>

    <div class='alert alert-info'>
      <strong><i class='fa fa-lightbulb'></i> Pro Tips:</strong>
      <ul>
        <li>Start with example datasets to learn the interface</li>
        <li>Use automated lookup for fish species (excellent FishBase coverage)</li>
        <li>Validate data before constructing to catch errors early</li>
        <li>Experiment with different thresholds to find optimal connectance</li>
        <li>Compare your network statistics to empirical food webs (connectance 0.1-0.3)</li>
      </ul>
    </div>

    <div class='alert alert-warning'>
      <strong><i class='fa fa-exclamation-triangle'></i> Common Issues:</strong>
      <ul>
        <li><strong>No interactions predicted:</strong> Lower threshold or check trait diversity</li>
        <li><strong>Network too dense:</strong> Raise threshold or review trait assignments</li>
        <li><strong>Automated lookup fails:</strong> Check internet connection and species names (use scientific names)</li>
        <li><strong>Validation errors:</strong> Review error messages, fix invalid trait codes</li>
      </ul>
    </div>
  ")
}


#' Generate Trait Dimensions HTML content
#' @export
generate_trait_help_dimensions <- function() {
  HTML("
    <div class='well'>
      <h5><strong>The Five Trait Dimensions</strong></h5>
      <p>Trait-based food webs use five categorical traits to predict species interactions:</p>
    </div>

    <h5>1. Size Class (MS)</h5>
    <table class='table table-striped table-bordered'>
      <thead>
        <tr><th>Code</th><th>Range (cm)</th><th>Examples</th><th>Ecological Group</th></tr>
      </thead>
      <tbody>
        <tr><td><strong>MS1</strong></td><td>&lt; 0.1</td><td>Bacteria, picoplankton</td><td>Microorganisms</td></tr>
        <tr><td><strong>MS2</strong></td><td>0.1 - 1</td><td>Copepod nauplii, small diatoms</td><td>Mesoplankton</td></tr>
        <tr><td><strong>MS3</strong></td><td>1 - 5</td><td>Large copepods, small fish</td><td>Macroplankton</td></tr>
        <tr><td><strong>MS4</strong></td><td>5 - 20</td><td>Shrimp, gobies, crabs</td><td>Small nekton</td></tr>
        <tr><td><strong>MS5</strong></td><td>20 - 50</td><td>Herring, mackerel, plaice</td><td>Medium nekton</td></tr>
        <tr><td><strong>MS6</strong></td><td>50 - 150</td><td>Cod, tuna, large fish</td><td>Large nekton</td></tr>
        <tr><td><strong>MS7</strong></td><td>&gt; 150</td><td>Sharks, marine mammals</td><td>Megafauna</td></tr>
      </tbody>
    </table>
    <p><em>Principle:</em> Gape limitation - consumers can only eat prey within a limited size range (typically 0.1-0.5 of consumer size).</p>
    <hr>

    <h5>2. Foraging Strategy (FS)</h5>
    <table class='table table-striped table-bordered'>
      <thead>
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
    </table>
    <p><em>Principle:</em> Feeding mode determines which prey types can be captured/consumed.</p>
    <hr>

    <h5>3. Mobility (MB)</h5>
    <table class='table table-striped table-bordered'>
      <thead>
        <tr><th>Code</th><th>Type</th><th>Description</th><th>Examples</th></tr>
      </thead>
      <tbody>
        <tr><td><strong>MB1</strong></td><td>Sessile</td><td>Permanently attached, no movement</td><td>Barnacles, mussels, corals</td></tr>
        <tr><td><strong>MB2</strong></td><td>Limited</td><td>Passive floating, very slow creeping</td><td>Jellyfish, sea anemones</td></tr>
        <tr><td><strong>MB3</strong></td><td>Crawling-Burrowing</td><td>Benthic locomotion</td><td>Crabs, gastropods, worms</td></tr>
        <tr><td><strong>MB4</strong></td><td>Facultative Swimmer</td><td>Swimming + benthic resting</td><td>Flatfish, rays, shrimp</td></tr>
        <tr><td><strong>MB5</strong></td><td>Obligate Swimmer</td><td>Continuous pelagic swimming</td><td>Most fish, squid, mammals</td></tr>
      </tbody>
    </table>
    <p><em>Principle:</em> Consumers must be able to encounter and capture prey based on relative mobility.</p>
    <hr>

    <h5>4. Environmental Position (EP)</h5>
    <table class='table table-striped table-bordered'>
      <thead>
        <tr><th>Code</th><th>Position</th><th>Description</th><th>Examples</th></tr>
      </thead>
      <tbody>
        <tr><td><strong>EP1</strong></td><td>Infaunal</td><td>Buried in sediment</td><td>Burrowing bivalves, polychaetes</td></tr>
        <tr><td><strong>EP2</strong></td><td>Epibenthic</td><td>On sediment surface</td><td>Starfish, crabs, bottom fish</td></tr>
        <tr><td><strong>EP3</strong></td><td>Benthopelagic</td><td>Near bottom, some swimming</td><td>Adult cod, flatfish</td></tr>
        <tr><td><strong>EP4</strong></td><td>Pelagic</td><td>Water column, no substrate</td><td>Plankton, herring, jellyfish</td></tr>
      </tbody>
    </table>
    <p><em>Principle:</em> Trophic interactions require spatial co-occurrence (habitat overlap).</p>
    <hr>

    <h5>5. Protection (PR)</h5>
    <table class='table table-striped table-bordered'>
      <thead>
        <tr><th>Code</th><th>Type</th><th>Structure</th><th>Examples</th></tr>
      </thead>
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
}


#' Generate Probability Matrices HTML content
#' @export
generate_trait_help_matrices <- function() {
  HTML("
    <div class='well'>
      <h5><strong>How Interaction Probabilities are Calculated</strong></h5>
      <p>The probability that species i consumes species j is determined by <strong>five probability matrices</strong>,
      one for each trait dimension. The final probability uses the <strong>minimum rule:</strong></p>
      <pre><code>P(i eats j) = min(
  P_size(MSi, MSj),
  P_forage(FSi, MSj),
  P_mobility(MBi, MBj),
  P_position(EPi, MSj),
  P_protection(PRj, MSi)
)</code></pre>
    </div>

    <h5>Why the Minimum Rule?</h5>
    <ul>
      <li><strong>Ecological realism:</strong> All conditions must be satisfied for interaction to occur</li>
      <li><strong>Limiting factor:</strong> Interaction constrained by most restrictive trait</li>
      <li><strong>Conservative:</strong> Avoids overestimating interactions</li>
    </ul>

    <h5>Example Calculation</h5>
    <div class='well'>
      <p><strong>Consumer:</strong> Atlantic cod (MS6, FS1, MB5, EP4, PR0)</p>
      <p><strong>Resource:</strong> Herring (MS5, FS6, MB5, EP4, PR0)</p>
      <pre><code>P_size(MS6, MS5) = 0.8   # Cod can eat large prey
P_forage(FS1, MS5) = 0.9 # Predators eat large fish
P_mobility(MB5, MB5) = 0.6 # Can catch fast swimmers
P_position(EP4, MS5) = 0.9 # Same habitat (pelagic)
P_protection(PR0, MS6) = 1.0 # No protection

→ P(Cod eats Herring) = min(0.8, 0.9, 0.6, 0.9, 1.0) = 0.6</code></pre>
      <p><strong>Result:</strong> Strong interaction predicted (if threshold < 0.6)</p>
    </div>

    <h5>The Five Matrices</h5>
    <ol>
      <li><strong>MS × MS (Size):</strong> Can consumer size eat resource size?
        <ul><li>Optimal: Resource 0.1-0.5 × Consumer size</li></ul>
      </li>
      <li><strong>FS × MS (Foraging × Size):</strong> Does foraging mode access this prey size?
        <ul><li>Predators (FS1) access medium-large prey</li>
        <li>Filter feeders (FS6) access small prey</li></ul>
      </li>
      <li><strong>MB × MB (Mobility):</strong> Can consumer catch resource?
        <ul><li>Fast predators (MB5) catch all prey</li>
        <li>Sessile (MB1) limited to slow/passing prey</li></ul>
      </li>
      <li><strong>EP × MS (Position × Size):</strong> Does habitat allow encounter?
        <ul><li>Pelagic (EP4) access water column prey</li>
        <li>Infaunal (EP1) limited to sediment prey</li></ul>
      </li>
      <li><strong>PR × MS (Protection × Consumer Size):</strong> Can consumer overcome defense?
        <ul><li>Large predators (MS6-MS7) crush shells</li>
        <li>Small predators limited by hard defenses</li></ul>
      </li>
    </ol>

    <h5>Threshold Selection</h5>
    <p>After calculating probabilities, a <strong>threshold</strong> determines which interactions are included:</p>
    <ul>
      <li><strong>Low (0.01-0.1):</strong> Many interactions, high connectance</li>
      <li><strong>Medium (0.1-0.5):</strong> Moderate, realistic food webs</li>
      <li><strong>High (0.5-1.0):</strong> Only strongest interactions</li>
    </ul>
    <p><em>Recommended:</em> 0.05-0.15 for most marine ecosystems (target connectance 0.1-0.3)</p>

    <div class='alert alert-info'>
      <strong><i class='fa fa-info-circle'></i> Tip:</strong>
      View detailed matrices in the 'Probability Matrices' tab to understand why specific interactions occur or don't occur.
    </div>
  ")
}


#' Generate Automated Lookup HTML content
#' @export
generate_trait_help_lookup <- function() {
  HTML("
    <div class='well'>
      <h5><strong>Automated Trait Lookup System</strong></h5>
      <p>Automatically retrieve and harmonize traits from online databases using species scientific names.</p>
    </div>

    <h5>Database Hierarchy</h5>
    <p>Traits are retrieved following a hierarchical workflow (priority order):</p>
    <ol>
      <li><strong>WoRMS</strong> → Taxonomic backbone (phylum, class, order, family)</li>
      <li><strong>FishBase</strong> → Fish traits (size, feeding, habitat, trophic level)</li>
      <li><strong>BIOTIC</strong> → Marine invertebrate traits (size, feeding, mobility, skeleton)</li>
      <li><strong>MAREDAT</strong> → Zooplankton traits (size, taxonomic group)</li>
      <li><strong>PTDB</strong> → Phytoplankton traits (size, morphology)</li>
      <li><strong>Taxonomic Inference</strong> → Fallback rules based on phylum/class</li>
    </ol>

    <h5>Harmonization Process</h5>
    <p>Raw data is converted to categorical trait codes:</p>
    <ul>
      <li><strong>Size:</strong> Continuous (cm) → MS1-MS7</li>
      <li><strong>Foraging:</strong> Text pattern matching → FS0-FS6</li>
      <li><strong>Mobility:</strong> Behavior description + taxonomy → MB1-MB5</li>
      <li><strong>Position:</strong> Depth range + habitat → EP1-EP4</li>
      <li><strong>Protection:</strong> Skeleton type + taxonomy → PR0-PR8</li>
    </ul>

    <h5>Database Coverage</h5>
    <table class='table table-bordered'>
      <thead>
        <tr><th>Organism Type</th><th>Best Databases</th><th>Coverage</th></tr>
      </thead>
      <tbody>
        <tr><td>Fish</td><td>WoRMS + FishBase</td><td>Excellent (35,000+ species)</td></tr>
        <tr><td>Benthic invertebrates</td><td>WoRMS + BIOTIC</td><td>Good (3,000+ species)</td></tr>
        <tr><td>Zooplankton</td><td>WoRMS + MAREDAT</td><td>Good (200+ groups)</td></tr>
        <tr><td>Phytoplankton</td><td>WoRMS + PTDB</td><td>Moderate (200+ species)</td></tr>
        <tr><td>Marine mammals</td><td>WoRMS + FishBase</td><td>Fair (limited traits)</td></tr>
      </tbody>
    </table>

    <h5>Using Automated Lookup</h5>
    <ol>
      <li>Go to <strong>Data Input</strong> tab</li>
      <li>Select <strong>Automated Lookup</strong></li>
      <li>Enter scientific names (one per line):
        <pre>Gadus morhua\nClupea harengus\nMytilus edulis</pre>
      </li>
      <li>Select databases (always include WoRMS)</li>
      <li>Click <strong>Run Automated Lookup</strong></li>
      <li>Wait 10-30 seconds (depends on # species and internet speed)</li>
      <li>Review results in <strong>Trait Editor</strong> tab</li>
      <li>Edit any incorrect assignments</li>
    </ol>

    <h5>Confidence Levels</h5>
    <ul>
      <li><strong>High:</strong> Data from FishBase or BIOTIC (directly measured)</li>
      <li><strong>Medium:</strong> Harmonized from related databases or pattern matching</li>
      <li><strong>Low:</strong> Taxonomic inference (family/order-level rules)</li>
      <li><strong>None:</strong> No data available (manual entry needed)</li>
    </ul>

    <h5>Caching System</h5>
    <p>Results are cached for 30 days to improve performance:</p>
    <ul>
      <li>First lookup: 1-3 seconds per species</li>
      <li>Subsequent lookups: <1 ms (instant)</li>
      <li>Cache location: <code>cache/taxonomy/</code></li>
      <li>Clear cache if database updates or config changes</li>
    </ul>

    <div class='alert alert-warning'>
      <strong><i class='fa fa-exclamation-triangle'></i> Important:</strong>
      <ul>
        <li>Always use <strong>scientific names</strong> (not common names)</li>
        <li>Verify species names on <a href='https://www.marinespecies.org/' target='_blank'>WoRMS</a> first</li>
        <li>Internet connection required for first lookup</li>
        <li>BIOTIC, MAREDAT, PTDB require downloaded files (see Database Setup Guide)</li>
        <li>Review and edit automated assignments - databases may have gaps or errors</li>
      </ul>
    </div>

    <h5>Customizing Harmonization</h5>
    <p>Adjust harmonization thresholds and rules:</p>
    <ol>
      <li>Click <strong>Settings</strong> (gear icon) in top-right</li>
      <li>Go to <strong>Harmonization</strong> tab</li>
      <li>Adjust:
        <ul>
          <li>Size thresholds (MS boundaries)</li>
          <li>Foraging patterns (regex)</li>
          <li>Taxonomic rules (enable/disable)</li>
          <li>Ecosystem profile (Arctic, Temperate, etc.)</li>
        </ul>
      </li>
      <li>Save configuration</li>
      <li>Re-run automated lookup with custom settings</li>
    </ol>
  ")
}


#' Generate Best Practices HTML content
#' @export
generate_trait_help_best_practices <- function() {
  HTML("
    <h5>Data Quality</h5>
    <div class='well'>
      <strong>DO:</strong>
      <ul>
        <li>✅ Use <strong>maximum body size</strong> (not average)</li>
        <li>✅ Assign traits based on <strong>adult life stage</strong></li>
        <li>✅ Use <strong>scientific names</strong> (Gadus morhua, not 'cod')</li>
        <li>✅ Verify taxonomy on <a href='https://www.marinespecies.org/' target='_blank'>WoRMS</a></li>
        <li>✅ Document data sources and confidence</li>
        <li>✅ Include diverse trophic levels (producers, herbivores, predators)</li>
      </ul>

      <strong>DON'T:</strong>
      <ul>
        <li>❌ Mix juveniles and adults in same dataset</li>
        <li>❌ Use outdated taxonomy (synonyms)</li>
        <li>❌ Guess traits without justification</li>
        <li>❌ Ignore regional variation (use ecosystem profiles)</li>
        <li>❌ Skip validation step</li>
      </ul>
    </div>

    <h5>Threshold Selection</h5>
    <ol>
      <li><strong>Start with 0.05-0.10</strong> (moderate threshold)</li>
      <li><strong>Construct network</strong> and check connectance</li>
      <li><strong>Adjust if needed:</strong>
        <ul>
          <li>Connectance < 0.05 → Too sparse, lower threshold</li>
          <li>Connectance > 0.5 → Too dense, raise threshold</li>
          <li>Target: <strong>0.1-0.3</strong> (realistic for marine food webs)</li>
        </ul>
      </li>
      <li><strong>Ecosystem-specific recommendations:</strong>
        <ul>
          <li>Pelagic: 0.05 (high mobility, frequent encounters)</li>
          <li>Benthic: 0.10 (lower mobility)</li>
          <li>Complex/diverse: 0.15 (many specialists)</li>
        </ul>
      </li>
    </ol>

    <h5>Network Validation</h5>
    <p>Check for ecological realism:</p>
    <ul>
      <li><strong>Connectance:</strong> Should be 0.05-0.30</li>
      <li><strong>Trophic levels:</strong> Verify presence of herbivores, carnivores, top predators</li>
      <li><strong>Basal species:</strong> Primary producers (FS0) should be present</li>
      <li><strong>Top predators:</strong> Large fish/mammals (MS6-MS7) with no predators</li>
      <li><strong>No isolated nodes:</strong> All species should have ≥1 link</li>
      <li><strong>Diet breadth:</strong> Generalists (many prey) and specialists (few prey) both reasonable</li>
    </ul>

    <h5>Regional Customization</h5>
    <p>Adjust for your ecosystem:</p>
    <ul>
      <li><strong>Arctic/Polar:</strong> Select 'Arctic' profile (larger body sizes)</li>
      <li><strong>Tropical:</strong> Select 'Tropical' profile (smaller sizes, more grazers)</li>
      <li><strong>Deep Sea:</strong> Select 'Deep Sea' profile (gigantism, more scavengers)</li>
      <li><strong>Coastal/Estuarine:</strong> Select 'Coastal' profile (more sessile, filter feeders)</li>
      <li><strong>Open Ocean:</strong> Select 'Open Ocean' profile (more swimmers, predators)</li>
    </ul>
    <p>Access: Settings > Harmonization > Ecosystem Profiles</p>

    <h5>Handling Missing Data</h5>
    <p>When automated lookup fails:</p>
    <ol>
      <li><strong>Check species name:</strong> Verify on WoRMS, try synonyms</li>
      <li><strong>Use taxonomic inference:</strong> Enable relevant rules (Settings > Harmonization)</li>
      <li><strong>Manual entry:</strong> Consult field guides, literature, expert knowledge</li>
      <li><strong>Conservative approach:</strong> Use moderate/common traits if uncertain</li>
      <li><strong>Document:</strong> Note confidence level and source</li>
    </ol>

    <h5>Quality Control Checklist</h5>
    <table class='table table-bordered'>
      <thead>
        <tr><th>Check</th><th>Action</th><th>Expected Result</th></tr>
      </thead>
      <tbody>
        <tr>
          <td>Trait validity</td>
          <td>Click 'Validate Trait Data'</td>
          <td>No errors, all codes valid</td>
        </tr>
        <tr>
          <td>Size range</td>
          <td>Check statistics</td>
          <td>MS1-MS7 represented (or MS2-MS6 minimum)</td>
        </tr>
        <tr>
          <td>Feeding diversity</td>
          <td>Check trait distribution</td>
          <td>Mix of FS0 (producers), FS6 (herbivores), FS1 (predators)</td>
        </tr>
        <tr>
          <td>Network realism</td>
          <td>Check connectance</td>
          <td>0.1-0.3 (typical for marine food webs)</td>
        </tr>
        <tr>
          <td>Visual inspection</td>
          <td>View network plot</td>
          <td>No obviously wrong interactions</td>
        </tr>
      </tbody>
    </table>

    <h5>Performance Tips</h5>
    <ul>
      <li><strong>Dataset size:</strong> 5-50 species optimal for interactive analysis</li>
      <li><strong>Large networks (>100 species):</strong> May be slow to visualize, use batch mode</li>
      <li><strong>Automated lookup:</strong> Run once, cache results, then experiment with thresholds</li>
      <li><strong>Export results:</strong> Download adjacency matrix for external analysis (e.g., cheddar, igraph)</li>
    </ul>

    <h5>Citing This Methodology</h5>
    <p>If you use trait-based food webs in publications, cite:</p>
    <blockquote>
      <p><strong>Olivier, P., et al. (2019)</strong> Exploring the temporal variability of a food web using long-term biomonitoring data.
      <em>Ecography</em>, 42(11), 2107-2121. <a href='https://doi.org/10.1111/ecog.04461' target='_blank'>https://doi.org/10.1111/ecog.04461</a></p>
    </blockquote>
    <p>And databases used (WoRMS, FishBase, etc. - see Full Methodology for citations).</p>

    <div class='alert alert-success'>
      <strong><i class='fa fa-check-circle'></i> Ready to Build!</strong>
      <p>You now have the knowledge to construct ecologically realistic trait-based food webs. Start with an example dataset,
      then try your own species list. Good luck!</p>
    </div>
  ")
}
