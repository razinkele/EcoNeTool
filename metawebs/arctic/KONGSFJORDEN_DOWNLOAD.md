# Kongsfjorden Food Web - Download Instructions

## Source
Farage et al. (2021) Methods in Ecology and Evolution 12:778-786
DOI: https://doi.org/10.1111/2041-210X.13569

## Original Data
Jacob et al. (2011) and Cirtwill & Eklöf (2018)
- 262 species
- 1,544 feeding interactions

## Download Options

### Option 1: GitHub Repository
Visit: https://github.com/Ecological-Complexity-Lab/infomap_ecology_package

Look for:
- Example data files
- Kongsfjorden food web data
- Files likely named: `kongsfjorden_nodes.csv`, `kongsfjorden_edges.csv` or similar

### Option 2: Zenodo Archive
Visit: https://zenodo.org/records/4494946
Download the R package and extract example data

### Option 3: Journal Supplementary Materials
Visit: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13569
Download supplementary materials from the article page

## Conversion
Once downloaded, use the metaweb conversion script:
```r
source('convert_kongsfjorden_metaweb.R')
```

