# EcoNeTool v2.0 - Launch Guide

## 🚀 Quick Launch

Open R or RStudio and run:

```r
library(shiny)
runApp()
```

**That's it!** The application will open in your default web browser.

---

## 🎨 What You'll See

### 1. **Dashboard Header** (Top Bar)
```
┌─────────────────────────────────────────────────────────┐
│ EcoNeTool | Baltic Food Web Explorer        ℹ️ 💬 📊  │
└─────────────────────────────────────────────────────────┘
```
- **Left**: "EcoNeTool" brand logo
- **Center**: "Baltic Food Web Explorer" title
- **Right**: Info dropdown (click 💬 for dataset details)

---

### 2. **Sidebar Menu** (Left Side)
```
┌──────────────────────┐
│ Navigation           │
│ ──────────────────── │
│ 🏠 Dashboard         │
│ 🔗 Food Web Network  │
│ 📈 Topological Metrics│
│ ⚖️ Biomass Analysis  │
│ ⚡ Energy Fluxes     │
│ ──────────────────── │
│ Information          │
│                      │
│ EcoNeTool           │
│ Gulf of Riga food web│
│ 🐟 34 species        │
│ 🔗 207 links         │
│ 📊 5 groups          │
└──────────────────────┘
```

---

### 3. **Dashboard Home Page** (Default View)

When you first launch, you'll see:

#### Welcome Box
```
┌────────────────────────────────────────────┐
│ Welcome to EcoNeTool                       │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│                                            │
│ Baltic Food Web Explorer                   │
│                                            │
│ This interactive dashboard allows you to   │
│ explore the Gulf of Riga marine food web...│
└────────────────────────────────────────────┘
```

#### Value Boxes (Key Statistics)
```
┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
│ 🐟          │ │ 🔗          │ │ 📊          │ │ 📅          │
│    34       │ │    207      │ │     5       │ │ 1979-2016   │
│ Taxa/Species│ │Trophic Links│ │ Functional  │ │ Time Period │
│             │ │             │ │   Groups    │ │             │
└─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘
  (Blue)         (Green)         (Cyan)          (Orange)
```

#### Information Boxes
```
┌────────────────────────────┐ ┌────────────────────────────┐
│ Functional Groups          │ │ Quick Start                │
│ ━━━━━━━━━━━━━━━━━━━━━━━━  │ │ ━━━━━━━━━━━━━━━━━━━━━━━━  │
│                            │ │                            │
│ ● Benthos (orange)         │ │ 1. Food Web Network        │
│ ● Detritus (dark grey)     │ │ 2. Topological Metrics     │
│ ● Fish (blue)              │ │ 3. Biomass Analysis        │
│ ● Phytoplankton (green)    │ │ 4. Energy Fluxes           │
│ ● Zooplankton (cyan)       │ │                            │
└────────────────────────────┘ └────────────────────────────┘
```

---

### 4. **Food Web Network Page**

Click "🔗 Food Web Network" in sidebar to see:

```
┌────────────────────────────────────────────────────────┐
│ Interactive Food Web Network                      [−][□]│
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│                                                        │
│         [Interactive Network Visualization]            │
│         (34 nodes, 207 edges, color-coded)            │
│         Zoom, pan, click nodes, hover for info        │
│                                                        │
└────────────────────────────────────────────────────────┘

┌──────────────────────────┐ ┌──────────────────────────┐
│ 🌱 Basal Species    [−] │ │ 👑 Top Predators    [−] │
│ ━━━━━━━━━━━━━━━━━━━━━━  │ │ ━━━━━━━━━━━━━━━━━━━━━━  │
│ Phytoplankton species... │ │ Cod (Gadus morhua)...    │
└──────────────────────────┘ └──────────────────────────┘

┌────────────────────────────────────────────────────────┐
│ Adjacency Matrix Heatmap                          [−][□]│
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│         [Heatmap showing all connections]              │
└────────────────────────────────────────────────────────┘
```

**Legend:**
- `[−]` = Collapse button
- `[□]` = Maximize button (full screen)

---

### 5. **Topological Metrics Page**

Click "📈 Topological Metrics" to see:

```
┌────────────────────────────────────────────────────────┐
│ Topological Indicators (Qualitative Metrics)           │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│ • S: Species richness                                  │
│ • C: Connectance                                       │
│ • G: Generality                                        │
│ • V: Vulnerability                                     │
│ ...                                                    │
└────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────┐
│ Calculated Metrics                                     │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│ $S                                                     │
│ [1] 34                                                 │
│                                                        │
│ $C                                                     │
│ [1] 0.1846847                                          │
│ ...                                                    │
└────────────────────────────────────────────────────────┘
```

---

### 6. **Biomass Analysis Page**

Click "⚖️ Biomass Analysis" to see:

```
┌──────────────────────────┐ ┌──────────────────────────┐
│ Biomass Distribution [−] │ │ Biomass Percentage  [−] │
│ ━━━━━━━━━━━━━━━━━━━━━━  │ │ ━━━━━━━━━━━━━━━━━━━━━━  │
│  [Boxplot by group]      │ │  [Bar chart %]           │
└──────────────────────────┘ └──────────────────────────┘

┌────────────────────────────────────────────────────────┐
│ Food Web with Biomass-Scaled Nodes               [−][□]│
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│    [Network with node sizes = biomass]                 │
└────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────┐
│ Node-weighted Indicators                               │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│ Biomass-weighted metrics...                            │
└────────────────────────────────────────────────────────┘
```

---

### 7. **Energy Fluxes Page**

Click "⚡ Energy Fluxes" to see:

```
┌────────────────────────────────────────────────────────┐
│ Energy Flux Analysis                                   │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│ Metabolic theory calculations...                       │
│ Units: kJ/day/km²                                      │
└────────────────────────────────────────────────────────┘

┌──────────────────────────┐ ┌──────────────────────────┐
│ Flux Matrix Heatmap [−][□]│ │ Flux Network       [−][□]│
│ ━━━━━━━━━━━━━━━━━━━━━━  │ │ ━━━━━━━━━━━━━━━━━━━━━━  │
│ [Log-transformed heatmap]│ │ [Weighted network]       │
└──────────────────────────┘ └──────────────────────────┘

┌────────────────────────────────────────────────────────┐
│ Link-weighted Flux Indicators                          │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│ Shannon diversity indices...                           │
└────────────────────────────────────────────────────────┘
```

---

### 8. **Controlbar** (Right Sidebar)

Click the **ⓘ** icon in top-right to open:

```
                                ┌──────────────────┐
                                │ Information      │
                                │ ──────────────── │
                                │ EcoNeTool        │
                                │ Version: 2.0     │
                                │ License: GPL-3.0 │
                                │                  │
                                │ Data Source      │
                                │ Frelat & Kortsch │
                                │ (2020)           │
                                │                  │
                                │ Color Scheme     │
                                │ ● Benthos        │
                                │ ● Detritus       │
                                │ ● Fish           │
                                │ ● Phytoplankton  │
                                │ ● Zooplankton    │
                                │                  │
                                │ References       │
                                │ Williams (2004)  │
                                │ Olivier (2019)   │
                                │ Brown (2004)     │
                                └──────────────────┘
```

---

## 🎮 Interactive Features

### Network Visualization
- **Zoom**: Mouse scroll wheel
- **Pan**: Click and drag background
- **Select Node**: Click on any node
- **View Details**: Hover over nodes
- **Toggle Groups**: Use legend

### Box Controls
- **Collapse**: Click on box title bar
- **Expand**: Click title bar again
- **Maximize**: Click `[□]` icon (when available)
- **Close Fullscreen**: Click `[□]` again or press ESC

### Navigation
- **Switch Pages**: Click sidebar menu items
- **Current Page**: Highlighted in sidebar
- **Quick Info**: Click dropdowns in header

---

## 🎨 Color Scheme

### Functional Groups (Network)
- 🟠 **Orange** = Benthos (bottom-dwellers)
- ⚫ **Dark Grey** = Detritus (organic matter)
- 🔵 **Blue** = Fish
- 🟢 **Green** = Phytoplankton (producers)
- 🔵 **Cyan** = Zooplankton (drifters)

### Box Status Colors
- 🔵 **Blue (primary)** = Main content, introductions
- 🟢 **Green (success)** = Positive indicators, basal species
- 🔵 **Cyan (info)** = Information, neutral content
- 🟠 **Orange (warning)** = Metrics, quantitative data
- 🔴 **Red (danger)** = Top predators, important alerts

---

## 💡 Pro Tips

1. **Start with Dashboard** to get an overview of the dataset

2. **Use the sidebar** for quick navigation between sections

3. **Collapse boxes** you're not using to reduce clutter

4. **Maximize plots** when you need to see details

5. **Check the controlbar** (ⓘ) for references and info

6. **Hover over network nodes** to see species details

7. **Try different browsers** if rendering seems slow:
   - ✅ Best: Chrome, Firefox, Edge
   - ⚠️ OK: Safari
   - ❌ Avoid: Internet Explorer

---

## 🐛 Troubleshooting

### App won't start?
```r
# Check if bs4Dash is installed
install.packages("bs4Dash")

# Try the backup version
source("app-before-bs4dash-backup.R")
```

### Plots not showing?
- Wait a moment (calculations take a few seconds)
- Check browser console for errors (F12)
- Try refreshing the page

### Layout looks weird?
- Increase browser window size (minimum 1024px width)
- Clear browser cache (Ctrl+Shift+Delete)
- Try a different browser

### Network visualization blank?
- Wait for data to load (may take 5-10 seconds first time)
- Check that visNetwork package is installed
- Try clicking "Reset" in network controls

---

## 📖 Learn More

- **Full Documentation**: See `BS4DASH_README.md`
- **Code Improvements**: See `IMPROVEMENTS.md`
- **Fixes Applied**: See `FIXES_APPLIED.md`
- **Scientific Tutorial**: See `BalticFoodWeb.Rmd`

---

## 🎯 Quick Tasks

### View Network Structure
1. Launch app
2. Click "🔗 Food Web Network"
3. Interact with visualization

### Calculate Metrics
1. Click "📈 Topological Metrics"
2. View qualitative indicators
3. Go to "⚖️ Biomass Analysis" for quantitative metrics

### Analyze Energy Flow
1. Click "⚡ Energy Fluxes"
2. View flux heatmap
3. Examine weighted network

---

**Enjoy exploring the Baltic Food Web! 🐟📊🔬**

---

**Version**: 2.0.1
**Status**: ✅ Fully Operational
**Last Updated**: 2025-11-27
