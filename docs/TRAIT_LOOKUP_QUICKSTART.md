# Automated Trait Lookup - Quick Start Guide

Get started with automated trait lookup in 5 minutes!

## Prerequisites

Install required R packages:

```r
install.packages(c("worrms", "rfishbase"))
```

That's it! The online databases (WoRMS and FishBase) are all you need to get started.

## Using in the App

### 1. Launch EcoNeTool

```r
source("run_app.R")
```

### 2. Navigate to Trait Food Web Tab

Click "Trait Food Web" in the sidebar (DNA icon).

### 3. Select "Automated lookup"

Choose "Automated lookup" from the input method radio buttons.

### 4. Enter Species Names

In the text area, enter one species name per line:

```
Gadus morhua
Clupea harengus
Mytilus edulis
Cancer pagurus
Homarus gammarus
```

### 5. Select Databases

Check the databases you want to query:
- ☑ WoRMS (taxonomy) - **Always recommended**
- ☑ FishBase (fish) - **For fish species**
- ☐ BIOTIC (invertebrates) - Optional, requires download
- ☐ MAREDAT (zooplankton) - Optional, requires download
- ☐ PTDB (phytoplankton) - Optional, requires download

### 6. Run Lookup

Click "Run Automated Lookup" button.

Wait 10-30 seconds (depending on number of species).

### 7. Review Results

The trait data table will update with results:
- Green indicators: Complete (all 5 traits found)
- Yellow indicators: Partial (some traits missing)
- Red indicators: No data found

### 8. Edit if Needed

Click any cell in the table to manually edit traits.

### 9. Construct Food Web

Click "Construct Food Web" button.

Your trait-based network will appear!

## Using Programmatically

### Quick Example

```r
# 1. Load functions
source("R/functions/trait_lookup.R")
source("R/functions/trait_foodweb.R")

# 2. Define species
species <- c("Gadus morhua", "Clupea harengus", "Mytilus edulis")

# 3. Lookup traits
traits <- batch_lookup_traits(species, cache_dir = "cache/taxonomy")

# 4. View results
print(traits)

# 5. Construct food web
foodweb <- construct_trait_foodweb(traits, threshold = 0.05)

# 6. Visualize
library(igraph)
g <- trait_foodweb_to_igraph(traits, threshold = 0.05)
plot(g, vertex.size = 10, edge.arrow.size = 0.5)
```

## Example Species Lists

### North Atlantic Fish Community

```
Gadus morhua
Melanogrammus aeglefinus
Pleuronectes platessa
Clupea harengus
Sprattus sprattus
Scomber scombrus
Pollachius virens
```

### Baltic Sea Benthic Community

```
Mytilus edulis
Macoma balthica
Cerastoderma glaucum
Hediste diversicolor
Marenzelleria viridis
Gammarus oceanicus
Monoporeia affinis
```

### Plankton Community

```
Skeletonema costatum
Thalassiosira nordenskioeldii
Chaetoceros decipiens
Acartia tonsa
Centropages hamatus
Temora longicornis
Pseudocalanus elongatus
```

## Expected Results

For **Gadus morhua** (Atlantic cod):
- **MS:** MS6 (large, 50-150 cm)
- **FS:** FS1 (predator)
- **MB:** MB5 (obligate swimmer)
- **EP:** EP4 or EP3 (pelagic/benthopelagic)
- **PR:** PR0 (no hard protection)
- **Source:** WoRMS+FishBase
- **Confidence:** High

For **Mytilus edulis** (Blue mussel):
- **MS:** MS3 (small-medium, 1-5 cm)
- **FS:** FS6 (filter feeder)
- **MB:** MB1 (sessile)
- **EP:** EP2 (epibenthic)
- **PR:** PR6 (hard shell)
- **Source:** WoRMS
- **Confidence:** Medium

## Troubleshooting

### No traits found

**Check:**
1. Internet connection (WoRMS and FishBase are online)
2. Species name spelling (use scientific names)
3. Package installation: `library(worrms)`, `library(rfishbase)`

**Solution:**
- Verify species name at https://www.marinespecies.org/
- Try alternative spelling or synonym
- Manual entry as fallback

### Only some traits found (yellow indicators)

**This is normal!**

Databases don't have complete data for all species.

**Solution:**
- Edit missing traits manually in the table
- Use taxonomic inference (e.g., all bivalves have shells → PR6)
- Consult field guides or literature

### Error: Package 'worrms' not installed

**Solution:**
```r
install.packages("worrms")
install.packages("rfishbase")
```

Restart R and try again.

## Tips

1. **Always select WoRMS** - provides taxonomic context for all species

2. **Use cache** - second lookup is instant (cached for 30 days)

3. **Check scientific names** - visit WoRMS.org to verify spelling

4. **Start with fish** - FishBase has excellent coverage for fish species

5. **Review results** - spot-check a few species to verify harmonization

6. **Edit when needed** - it's okay to manually adjust traits

## Next Steps

- Read full guide: `docs/AUTOMATED_TRAIT_LOOKUP_GUIDE.md`
- Download optional databases: `docs/DATABASE_SETUP_GUIDE.md`
- Explore trait food web guide: `docs/TRAIT_FOODWEB_GUIDE.md`
- Try example datasets: `examples/TRAIT_FOODWEB_EXAMPLES_README.md`

## Complete Workflow Example

```r
# === NORTH ATLANTIC FISH FOOD WEB ===

# 1. Define community
species <- c(
  # Fish predators
  "Gadus morhua",           # Atlantic cod
  "Melanogrammus aeglefinus", # Haddock
  "Scomber scombrus",       # Mackerel

  # Small fish
  "Clupea harengus",        # Herring
  "Sprattus sprattus",      # Sprat

  # Invertebrates
  "Calanus finmarchicus",   # Copepod
  "Euphausia superba",      # Krill
  "Cancer pagurus",         # Crab

  # Primary producers
  "Skeletonema costatum",   # Diatom
  "Emiliania huxleyi"       # Coccolithophore
)

# 2. Automated lookup
source("R/functions/trait_lookup.R")
traits <- batch_lookup_traits(species, cache_dir = "cache/taxonomy")

# 3. Check results
cat("Complete traits:", sum(complete.cases(traits[,c("MS","FS","MB","EP","PR")])), "\n")
cat("Partial traits:", sum(!complete.cases(traits[,c("MS","FS","MB","EP","PR")])), "\n")

# 4. Review confidence
table(traits$confidence)

# 5. Fill any gaps manually
# (inspect traits data frame and edit as needed)

# 6. Construct food web
source("R/functions/trait_foodweb.R")
foodweb <- construct_trait_foodweb(traits, threshold = 0.05)

# 7. Create network
library(igraph)
g <- trait_foodweb_to_igraph(traits, threshold = 0.05)

# 8. Network properties
cat("Species:", vcount(g), "\n")
cat("Interactions:", ecount(g), "\n")
cat("Connectance:", edge_density(g), "\n")

# 9. Trophic levels
trophic <- igraph::trophic_levels(g)
print(trophic)

# 10. Plot
plot(g,
     vertex.size = 8,
     vertex.label.cex = 0.7,
     vertex.color = heat.colors(max(trophic))[trophic],
     edge.arrow.size = 0.3,
     layout = layout_with_fr(g),
     main = "North Atlantic Fish Food Web")

# 11. Export
write.csv(traits, "north_atlantic_traits.csv", row.names = FALSE)
write.csv(foodweb, "north_atlantic_foodweb.csv", row.names = TRUE)

# Done!
```

Expected output:
```
[1] Looking up traits for: Gadus morhua
  Found 5/5 traits from WoRMS+FishBase
[2] Looking up traits for: Melanogrammus aeglefinus
  Found 5/5 traits from WoRMS+FishBase
... (8 more species)

Complete traits: 8
Partial traits: 2

      high medium    low   none
         8      2      0      0

Species: 10
Interactions: 24
Connectance: 0.267
```

## That's It!

You're now using automated trait lookup to build food webs.

**Questions?**
- Full guide: `docs/AUTOMATED_TRAIT_LOOKUP_GUIDE.md`
- Database setup: `docs/DATABASE_SETUP_GUIDE.md`
- GitHub issues: Open an issue for bugs or questions

**Happy food web building!** 🦈🦀🐟
