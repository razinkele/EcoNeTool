# Version Number Correction - v1.0.20 → v1.1.1

**Date:** 2025-12-23
**Status:** Corrected

---

## Issue

The previous commit incorrectly versioned the release as **v1.0.20** when it should have been **v1.1.1**.

## Root Cause

Looking at the version history:
- **v1.1.0** (2025-12-18) - EMODnet Habitat Integration & Spatial Analysis
- **v1.0.20** (2025-12-23) - Color Scheme Fix & Lithuanian Coastal Dataset ❌ WRONG

According to [Semantic Versioning](https://semver.org/), version numbers follow the format:
```
MAJOR.MINOR.PATCH
```

Where:
- **MAJOR** version for incompatible API changes
- **MINOR** version for new functionality (backward-compatible)
- **PATCH** version for backward-compatible bug fixes

## Why v1.1.1 is Correct

The recent release (2025-12-23) contained:
1. **Bug Fixes** (Phytoplankton/Zooplankton colors, flux network)
2. **Minor Feature** (metaweb export)
3. **Dataset Change** (LTCoast.Rdata as default)

Since **v1.1.0** already exists (EMODnet integration), and our changes are primarily bug fixes after that release, the correct version is:

**v1.1.1** (PATCH increment from v1.1.0)

## Semantic Versioning Logic

```
v1.1.0  →  EMODnet Habitat Integration (MINOR feature)
  ↓
v1.1.1  →  Color Scheme Fix (PATCH fixes)
  ↓
v1.2.0  →  Next MINOR feature release
  ↓
v2.0.0  →  Breaking changes (MAJOR)
```

**Incorrect:**
```
v1.1.0  →  v1.0.20  ❌ (going backwards in MINOR version!)
```

**Correct:**
```
v1.1.0  →  v1.1.1  ✅ (PATCH increment)
```

---

## Correction Made

### Files Updated

1. **VERSION**
   - Changed: `VERSION=1.0.20` → `VERSION=1.1.1`
   - Changed: `MINOR=0` → `MINOR=1`
   - Changed: `PATCH=20` → `PATCH=1`

2. **CHANGELOG.md**
   - Changed: `## [1.0.20] - 2025-12-23` → `## [1.1.1] - 2025-12-23`
   - Updated documentation references from v1.0.20 to v1.1.1

3. **README.md**
   - Changed: `Recent Updates (v1.0.20 - 2025-12-23)` → `Recent Updates (v1.1.1 - 2025-12-23)`
   - Changed: `Current Version: 1.0.20` → `Current Version: 1.1.1`
   - Changed: `version = {1.0.20}` → `version = {1.1.1}` (citation)

---

## Version History (Corrected)

```
v1.1.1 (2025-12-23) - Color Scheme Fix & Lithuanian Coastal Dataset [PATCH]
  ├─ Fixed: Phytoplankton/Zooplankton colors
  ├─ Fixed: Flux network validation errors
  ├─ Changed: Default dataset to LTCoast.Rdata
  └─ Added: Metaweb export functionality

v1.1.0 (2025-12-18) - EMODnet Habitat Integration [MINOR]
  ├─ Added: EMODnet EUSeaMap habitat integration
  ├─ Added: Spatial analysis enhancements
  ├─ Added: BBT polygon selector
  └─ Added: Habitat layer visualization

v1.0.x - Initial releases and early features
```

---

## Lesson Learned

**Always check existing version history before incrementing version numbers.**

When the current version is **v1.1.0**:
- Bug fixes → **v1.1.1** (PATCH)
- New features → **v1.2.0** (MINOR)
- Breaking changes → **v2.0.0** (MAJOR)

---

## Commit Message for Correction

```
Fix version number: v1.0.20 → v1.1.1 (correct semantic versioning)

Corrected version number to follow semantic versioning properly.
Previous version v1.1.0 (EMODnet integration) already existed.
This release is a PATCH (bug fixes) after v1.1.0, so v1.1.1 is correct.

Files updated:
- VERSION (1.0.20 → 1.1.1)
- CHANGELOG.md ([1.0.20] → [1.1.1])
- README.md (all references updated)

No functional changes - version metadata only.
```

---

**Status:** ✅ Corrected
**Previous Version:** v1.0.20 (incorrect)
**Current Version:** v1.1.1 (correct)
**Semantic Versioning:** Properly followed
