# EcoNeTool v2.0 - Quick Start Guide

## 🚀 Launch the Application

```r
library(shiny)
runApp()
```

**Or use the launcher script:**
```r
source("run_app.R")
```

---

## 📋 Navigation

### Sidebar Menu (Left)
- 🏠 **Dashboard** → Overview & key statistics
- 🔗 **Food Web Network** → Interactive visualization
- 📈 **Topological Metrics** → Structural properties
- ⚖️ **Biomass Analysis** → Node-weighted metrics
- ⚡ **Energy Fluxes** → Metabolic theory analysis

### Quick Info (Top Right)
- Click 💬 for dataset information

### Detailed Info (Right Sidebar)
- Click ⓘ to open controlbar
- View version, license, references

---

## 🎯 Key Features

### Interactive Network
- **Zoom**: Scroll wheel
- **Pan**: Click and drag
- **Select**: Click nodes
- **Hover**: View species details
- **Legend**: Toggle functional groups

### Box Controls
- **Collapse**: Click title bar
- **Maximize**: Click ⛶ icon (when available)
- **Scroll**: Within boxes if needed

### Value Boxes (Dashboard)
- Quick stats at a glance
- Color-coded by importance

---

## 🎨 Functional Groups

- 🟠 **Orange** = Benthos
- ⚫ **Dark Grey** = Detritus
- 🔵 **Blue** = Fish
- 🟢 **Green** = Phytoplankton
- 🔵 **Cyan** = Zooplankton

---

## 📊 Main Analysis Sections

### 1. Food Web Network
→ See species connections and identify key players

### 2. Topological Metrics
→ Understand network structure (Connectance, Generality, etc.)

### 3. Biomass Analysis
→ Explore biomass distribution and weighted metrics

### 4. Energy Fluxes
→ Analyze energy flow using metabolic theory

---

## 💡 Tips

1. **Start at Dashboard** for overview
2. **Use sidebar** for quick navigation
3. **Collapse boxes** to reduce clutter
4. **Maximize plots** for detail view
5. **Check controlbar** for references

---

## 🔧 Customization

### Change Theme
Edit in `app.R`:
```r
skin = "dark"  # or "light"
```

### Modify Colors
Edit configuration constants at top of `app.R`:
```r
COLOR_SCHEME <- c("orange", "darkgrey", "blue", "green", "cyan")
```

---

## 📚 Learn More

- **Full Documentation**: `BS4DASH_README.md`
- **Code Improvements**: `IMPROVEMENTS.md`
- **Tutorial**: `BalticFoodWeb.Rmd`

---

## 🐛 Troubleshooting

**App won't start?**
```r
# Check packages
install.packages("bs4Dash")

# Try backup version
source("app-before-bs4dash-backup.R")
```

**Layout broken?**
- Clear browser cache
- Try different browser
- Check window size (min 1024px width)

---

## 📖 Dataset Info

- **Species**: 34 taxa
- **Links**: 207 trophic interactions
- **Groups**: 5 functional categories
- **Period**: 1979-2016
- **Location**: Gulf of Riga, Baltic Sea
- **Source**: Frelat & Kortsch (2020)

---

**Version 2.0** | GPL-3.0 License | Powered by bs4Dash & Shiny
