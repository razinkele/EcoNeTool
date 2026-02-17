# Ontology-Based Trait Database

**File:** ontology_traits.csv
**Species:** 66 Baltic/North Sea species
**Trait Records:** 425
**Version:** 2.0 (2025-12-25)

## Quick Facts

- **Fuzzy scoring:** Species can have multiple feeding modes (0-3 scale)
- **Ontology terms:** ECO, FOODON, ENVO, PCO
- **Data provenance:** All traits cited (BIOTIC or Literature)
- **Ecologically realistic:** Reflects facultative and opportunistic behaviors

## Structure

```csv
taxon_name,aphia_id,trait_category,trait_name,trait_modality,trait_score,unit,ontology,ontology_id,source,notes
Macoma balthica,141579,feeding,feeding_mode,surface_deposit_feeder,3,score,ECO,ECO:0000205,BIOTIC,Primary feeding mode
Macoma balthica,141579,feeding,feeding_mode,suspension_feeder,2,score,ECO,ECO:0000212,BIOTIC,Facultative suspension feeding
```

## Trait Score System

- **3** = Primary/dominant (>60% of behavior)
- **2** = Secondary/common (20-60%)
- **1** = Occasional/rare (<20%)
- **0** = Absent (not recorded)

## Ontologies Used

**ECO** - Feeding processes (predator, grazer, suspension_feeder)  
**FOODON** - Diet items (detritus, phytoplankton, carrion)  
**ENVO** - Habitat (soft_sediment, benthic_zone, intertidal_zone)  
**PCO** - Life history (burrower, sessile, crawler, swimmer)

## Example Usage

```r
# Load traits
traits <- read.csv("data/ontology_traits.csv")

# Find deposit feeders (score >= 2)
deposit <- traits[traits$trait_modality == "surface_deposit_feeder" & 
                  traits$trait_score >= 2, ]

# Get fuzzy profile for one species
macoma <- traits[traits$taxon_name == "Macoma balthica", ]
```

## Species Coverage

**Fish (14):** Gadus morhua, Clupea harengus, Sprattus sprattus, Platichthys flesus, Limanda limanda, Pleuronectes platessa, Ammodytes marinus, Pomatoschistus microps, Zoarces viviparus, Gasterosteus aculeatus, Merlangius merlangus, Trisopterus esmarkii, Anguilla anguilla, Myoxocephalus scorpius

**Phytoplankton (10):** Skeletonema costatum, Chaetoceros spp., Thalassiosira spp., Dinophysis spp., Prorocentrum spp., Coscinodiscus spp., Ceratium spp., Rhizosolenia spp., Pseudo-nitzschia spp., Nodularia spumigena

**Zooplankton (10):** Acartia spp., Temora longicornis, Pseudocalanus spp., Centropages hamatus, Calanus finmarchicus, Evadne nordmanni, Pleopsis polyphemoides, Oithona spp., Bosmina spp., Fritillaria borealis

**Benthic Invertebrates (32):** Macoma balthica, Mya arenaria, Mytilus edulis, Mytilus trossulus, Cerastoderma glaucum, Hydrobia ulvae, Theodoxus fluviatilis, Amphibalanus improvisus, Gammarus oceanicus, Saduria entomon, Bathyporeia pilosa, Corophium volutator, Idotea balthica, Jaera albifrons, Hediste diversicolor, Marenzelleria spp., Pygospio elegans, Arenicola marina, Nereis diversicolor, Lanice conchilega, Crangon crangon, Neomysis integer, Praunus flexuosus, Carcinus maenas, Asterias rubens, Ophiura spp., Palaemon adspersus, Pagurus bernhardus, Cancer pagurus, Echinocardium cordatum, Ostracoda, Oligochaeta

## Integration Status

✅ Database expanded (66 species, 425 records)
✅ Lookup function implemented (lookup_ontology_traits)
✅ Cache integration complete
✅ Fuzzy harmonization implemented (harmonize_fuzzy_foraging, harmonize_fuzzy_mobility, harmonize_fuzzy_habitat)
⏳ GUI viewer pending

## Coverage Statistics

- **Average fuzzy coverage:** 94.5% (FS+MB+EP traits)
- **Full coverage species:** 10/12 tested (83%)
- **Fish:** 100% coverage
- **Zooplankton:** 100% coverage
- **Benthic invertebrates:** 100% coverage
- **Phytoplankton:** 78% coverage (some missing mobility data)

## Next Steps

1. Add protection traits (PR) to ontology database
2. Add mobility data for phytoplankton (floater modality)
3. Create GUI ontology trait viewer
4. Expand to 100+ species
