# Apte Dictionary — AI Context Guide

This document provides a comprehensive context for AI coding assistants working on this project. It consolidates information about the architecture, codebase, conventions, and design decisions.

---

## 1. Project Overview

**Repository**: `apte-uncompress`
**Live URL**: <https://apte-dictionary.fly.dev/>
**GitHub**: <https://github.com/sumanthegde/apte-uncompress>

This project parses [V S Apte's Sanskrit-English Dictionary (1957 revised edition)](https://www.sanskrit-lexicon.uni-koeln.de/scans/AP90Scan/2020/web/webtc2/index.php), expands the terse compound-word (samāsa) entries into fully searchable terms, and serves them through a web interface. The core problem: Apte's dictionary nests compounds under their first component (pūrvapada), making direct lookup impossible with standard tools. This project generates ~60k expanded entries with full headwords.

The project has **two main components**:
1. **Haskell backend** — One-time data processing: parses the raw dictionary, expands compounds, applies sandhi rules, and outputs structured JSON + lookup tables.
2. **JavaScript frontend** — Web interface: Node.js server + vanilla HTML/CSS/JS client for searching and browsing the dictionary.

---

## 2. Directory Structure

```
apte-uncompress/
├── haskell/                    # Haskell data-generation code
│   ├── app/Main.hs             # Entry point — orchestrates the pipeline
│   ├── src/                    # Source modules (see §3)
│   ├── apteDir.nosync/         # Dictionary data (git-ignored)
│   │   ├── input/              # Raw dictionary source files (1957 edition)
│   │   └── output/             # Generated output
│   │       ├── sharded/*.json  # One JSON file per term (numeric IDs)
│   │       ├── table_new.txt   # Lookup table (Devanagari → term ID)
│   │       ├── pagemarks.json  # Line-number → page-number mapping
│   │       ├── apte_data.sqlite # SQLite DB (meaning-text search index)
│   │       └── mIndexesTable.txt # Meaning-text search index (flat file)
│   ├── package.yaml            # Stack package config (keep here for path resolution)
│   └── stack.yaml
├── web/                        # JavaScript web interface
│   ├── server/serve.js         # Node.js HTTP server (Express-free, raw http module)
│   ├── public/
│   │   ├── index.html          # Main SPA (~57KB, all client logic inline)
│   │   ├── css/                # Stylesheets
│   │   └── js/                 # Client-side JS (sanscript.js for transliteration)
│   └── package.json            # Node dependencies (sqlite3, sqlite)
├── data -> haskell/apteDir.nosync  # Symlink for local development
├── kosha-flat/                 # One JSON per expanded entry (CDN-friendly, public API)
├── kosha-nested/               # Nested JSON per headword (internal, may change)
├── Dockerfile                  # Node 18 slim, copies server + data
├── fly.toml                    # Fly.io deployment config
├── scripts/                    # Utility scripts (kosha generation, etc.)
└── archives/                   # Reference files (abbreviation maps, etc.)
```

### Important notes
- `data/` is a **symlink** to `haskell/apteDir.nosync` — keep this intact.
- `apteDir.nosync/` is **git-ignored** (large generated data).
- `kosha-flat/` is the **stable public API** for programmatic access via CDN:
  `https://cdn.jsdelivr.net/gh/sumanthegde/apte-uncompress@main/kosha-flat/{entry}.json`
- `kosha-nested/` is an internal format — may change for optimization.
- Generated files to ignore in git: `apte-uncompress.cabal`, `apte-uncompress.iml`.

---

## 3. Haskell Backend — Data Generation Pipeline

The Haskell code is a **one-time batch processor**. It runs locally to regenerate the dictionary data. It currently parses **Apte's 1957 revised edition** (not the original 1890 edition).

### Source Modules (`haskell/src/`)

| Module | Size | Purpose |
|--------|------|---------|
| `ApteParser.hs` | ~40KB | Parses raw dictionary text into structured `Term` objects using `ReadP` parser combinators |
| `ApteExpander.hs` | ~33KB | Expands compounds using sandhi rules; generates sharded JSON + lookup tables |
| `ApteRecords.hs` | ~7KB | Core data types: `Term`, `Record`, `Location` |
| `ApteUtils.hs` | ~5KB | Apte-specific utility functions |
| `Sandhi.hs` | ~15KB | Sanskrit sandhi (word-joining) rules |
| `SandhiTest.hs` | ~15KB | Sandhi test cases |
| `Pratyayas.hs` | ~5KB | Sanskrit suffix (pratyaya) handling |
| `SEncode.hs` | ~5KB | SLP1 ↔ Devanagari encoding conversion |
| `Utils.hs` | ~5KB | General utilities and path definitions |
| `SqliteUtil.hs` | ~4KB | SQLite database creation for meaning-text search |
| `ParserCombUtils.hs` | ~3KB | Parser combinator helpers |
| `JsonUtils.hs` | ~1KB | JSON serialization/deserialization |
| `Natva.hs` | <1KB | णत्व (retroflex nasal conversion) rules |

### Processing Pipeline

```
Raw dictionary text (apteDir.nosync/input/)
        │
        ▼
   1. parserMain (ApteParser.hs)
      Parse → structured Term objects → intermediate JSON
        │
        ▼
   2. expanderMain (ApteExpander.hs)
      Expand compounds using sandhi rules → hierarchical terms
        │
        ▼
   3. Data Generation
      ├── shardAndStore → sharded/*.json (one file per term)
      ├── tabulate → table_new.txt (Devanagari lookup table)
      └── pagemarks.json (line → page mapping)
```

### Build & Run

```bash
cd haskell
stack build
stack run          # Full pipeline
stack ghci         # Interactive: > parserMain  > expanderMain
```

Memory tip: use `+RTS -M2G` for large dictionary processing.

### Key Data Structures

**`Term`** — The fundamental dictionary entry:
```haskell
data Term = Term
  { __line      :: Maybe Int                      -- Line in original dictionary
  , _ancestry   :: Maybe [Location]               -- Hierarchical path
  , _banner     :: Maybe String                   -- Headword (often abbreviated)
  , _bannerExp  :: Maybe [Either String String]    -- Expanded headword
  , _gram       :: Maybe [String]                 -- Grammatical info
  , _meanings   :: Maybe [String]                 -- Definitions
  , _morphisms  :: Maybe [Term]                   -- Morphological variants
  , _samasas    :: Maybe [Term]                   -- Compound words
  , _spent      :: Maybe String                   -- Processed text
  , _unspent    :: Maybe String                   -- Unprocessed text
  }
```

**`Location`** — Position in the dictionary hierarchy (max depth 4):
```haskell
data Location = L_ {loc :: String}      -- Main entry
             | B_ {loc :: String}       -- Base word
             | S_ {loc :: String}       -- Compound
             | M_ {loc :: String}       -- Morphological variant
             | S_M_ {loc :: String}     -- Compound variant
             | S_S_ {loc :: String}     -- Sub-compound
             | S_M_S_ | S_M_S_M_ | S_S_M_  -- Deeper nesting
```

Common nesting patterns:
- `B_→ S_→ S_M_→ S_M_S_`
- `B_→ S_→ S_S_→ S_S_M_`

---

## 4. JavaScript Frontend — Web Interface

### Architecture

**Server** (`web/server/serve.js`):
- Raw Node.js `http` module (no Express)
- SQLite database for meaning-text search
- CLI args: `--public <dir> --data <dir>`
- Endpoints:
  | Endpoint | Purpose |
  |----------|---------|
  | `/{number}` or `/{number}.json` | Fetch term JSON by numeric ID |
  | `/search-meanings?q=<query>` | Search within meaning texts (SQLite) |
  | `/pagemarks-data.json` | Page-number mapping data |
  | `/` (index.html) | Main page — injects `table_new.txt` as `preloadedTableData` |

**Client** (`web/public/index.html`):
- Single-page application, all logic inline (~57KB)
- No framework — vanilla JS, vanilla CSS
- Uses `sanscript.js` (in `web/public/js/`) for SLP1 ↔ Devanagari transliteration

### Search & Navigation Flow

1. **Typing** → `onInput()` (debounced) → `getSuggestions()`:
   - Detects SLP1 vs Devanagari input
   - Converts to intermediate form
   - Binary search on preloaded `table_new.txt` (pattern: `while (left + 1 < right)`)
   - Filters: exact matches → prefix matches → alternative forms
   - Displays top 7 suggestions (excludes `M_`-ending labels, deduplicates)

2. **Selecting a suggestion** → `navigateToTerm()` → `loadTermWithContext()`:
   - Fetches term JSON via `loadSingleTerm(termId)`
   - Renders via `renderTermsInRange()` which recursively processes `_morphisms` and `_samasas`
   - Updates URL via `history.pushState` (enables back/forward, bookmarking, direct links)

3. **Scrolling/Paging** — infinite scroll with windowed loading:
   - Buffer: 4 × WINDOW_SIZE terms in DOM
   - Arrow keys at boundaries trigger `loadMoreTerms()`
   - `trimExcessTerms()` keeps DOM manageable

4. **Meaning search** — prefix query with `/`:
   - e.g., typing `/elephant` searches meaning texts
   - Uses `/search-meanings` endpoint (SQLite-backed)

### Lookup Table Format (`table_new.txt`)
Tab-separated: `देवनागरी_शब्द\tterm_id\tparent_term`
- Sorted lexicographically on first field (enables binary search)
- Fallback: if lookup fails for words ending in `अ` or `ौ`, tries appending `ः` or `ं`

---

## 5. Sanskrit Text Processing Conventions

- **SLP1 encoding**: Sanskrit text in the data is stored in [SLP1](https://en.wikipedia.org/wiki/SLP1) transliteration
- **Bracket notation**: SLP1 text within meaning strings is enclosed in `{# ... #}` — the frontend converts these to Devanagari using `sanscript.js`
- **Banner elements** (`_bannerExp`): SLP1 without `{# #}` brackets — converted directly
- **Independent vowels**: require special handling during SLP1 → Devanagari conversion
- **Anusvaras**: normalized via `anusvarafy()` function
- **Term display**: if `_bannerExp` is missing, display `_banner` with ⚠️ indicator

---

## 6. UI Design Principles

### Visual Theme
- **Aesthetic**: Classic wooden, old books/library feel
- **Devanagari text**: Teak-wood color palette
- **Background**: Warm tones matching the font colors
- **Nesting**: Alternating background colors that fade left-to-right

### Layout
- **Header** (fixed on scroll): three sections
  - Left: "Apte Dictionary" title + "Compounds expanded" subtitle + "1957 Edition" (animated golden shine effect)
  - Middle: Search input (2× Devanagari font size)
  - Right: GitHub icon
- **Content**: Fully expanded (no scrollable inner containers), minimalist
- **Navigation**: Pagination buttons at top and bottom; arrow-key support

### Term Rendering
- `div.term-container` with L-number prefix
- Nested structure with alternating highlights (per-element, not entire Term)
- Max depth: 4 levels
- Line numbers link to original scanned pages (via `pagemarks.json`)
- Banner headwords link to [sanskritkosha.com](https://sanskritkosha.com)
- Page links have class `page-link`
- External page URL pattern: `https://www.sanskrit-lexicon.uni-koeln.de/scans/csl-apidev/servepdf.php?dict=AP90&page=`

---

## 7. Deployment

- **Platform**: [Fly.io](https://fly.io)
- **App name**: `apte-dictionary`
- **Region**: `ams` (Amsterdam)
- **VM**: `shared-cpu-1x`
- **Docker**: Node 18 slim base image
- **Data**: SQLite DB + pagemarks.json + table_new.txt baked into image (at `/app/data2/`); mounted volume at `/app/data/`
- **Port**: 8080 (force HTTPS)
- **Deploy**: `fly deploy` from repo root

### Local Development
```bash
node web/server/serve.js --public web/public --data data/output
# Access at http://localhost:8080
```

#### Updating Reference Mappings
To auto-update `web/public/js/textprocessing.js` and auto-bump the package version whenever `web/resources/apte-ref-map.tsv` changes, run:
```bash
cd web && npm run update-refs
```

---

## 8. Development Guidelines

- **Accuracy > Coverage**: Prioritize precision in Sanskrit processing over breadth
- **Naming**: Keep the project name `apte-uncompress`
- **File moves**: Always use `git mv` to preserve history
- **HLS issues**: If Haskell Language Server doesn't recognize modules after a successful `stack build`, restart the language server or VS Code
- **Haskell component**: Used for one-time data generation — not a running service
- **Frontend self-containment**: Node.js server could potentially be eliminated; functionality could move client-side with data hosted directly on GitHub/CDN
- **Contact**: sumant.sanskrit@gmail.com
