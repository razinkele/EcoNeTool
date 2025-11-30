# Installing mdbtools on Windows

## Option A: Using WSL (Windows Subsystem for Linux)

### 1. Enable WSL
```bash
# In PowerShell (as Administrator)
wsl --install
```

### 2. Install Ubuntu from Microsoft Store
- Open Microsoft Store
- Search for "Ubuntu"
- Install Ubuntu 22.04 LTS

### 3. Install mdbtools in WSL
```bash
# Inside WSL Ubuntu terminal
sudo apt-get update
sudo apt-get install mdbtools
```

### 4. Extract data from .ewemdb file
```bash
# Navigate to your file (WSL can access Windows files)
cd "/mnt/c/Users/DELL/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"

# List all tables in the database
mdb-tables "coast 2011-04-10 10.00.ewemdb"

# Export specific tables to CSV
mdb-export "coast 2011-04-10 10.00.ewemdb" EcopathGroup > ecopath_basic.csv
mdb-export "coast 2011-04-10 10.00.ewemdb" EcopathDietComp > ecopath_diet.csv
```

## Option B: Use MSYS2

### 1. Install MSYS2
- Download from: https://www.msys2.org/
- Run the installer

### 2. Install mdbtools
```bash
# In MSYS2 terminal
pacman -S mingw-w64-x86_64-mdbtools
```

## Option C: Use Online MDB Converter

1. Upload your .ewemdb file to:
   - https://www.rebasedata.com/convert-mdb-to-csv-online
   - Or similar MDB to CSV converters

2. Download the converted CSV files

## Option D: Modify R Code to Use RODBC (Windows Native)

If you have Microsoft Access Database Engine installed:

```R
# Install RODBC
install.packages("RODBC")

# Example usage
library(RODBC)
con <- odbcConnectAccess2007("coast 2011-04-10 10.00.ewemdb")
tables <- sqlTables(con)
basic_data <- sqlFetch(con, "EcopathGroup")
diet_data <- sqlFetch(con, "EcopathDietComp")
odbcClose(con)
```

---

## Recommended Approach

**For immediate use:** Export from ECOPATH software (Option 1)

**For long-term:** Install WSL with mdbtools (Option A)
