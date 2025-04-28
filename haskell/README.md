# Apte Dictionary Haskell Code

This directory contains the Haskell code responsible for parsing, processing, and expanding the Apte Sanskrit-English Dictionary. The code transforms the raw dictionary data into a structured format with expanded Sanskrit compounds, making it more accessible and searchable.

## Overview

The Haskell component of this project performs several key functions:

1. **Parsing** - Converts the raw dictionary text into structured data
2. **Compound Expansion** - Expands Sanskrit compound words (samāsas) into their constituent parts
3. **Sandhi Processing** - Applies Sanskrit sandhi rules to correctly join word components
4. **Data Generation** - Creates JSON files and lookup tables for the web interface:
   - `apteDir.nosync/output/sharded/*.json` - Individual term files used by the frontend
   - `apteDir.nosync/output/table_new.txt` - Lookup table for Sanskrit word search
   - `apteDir.nosync/output/pagemarks.json` - Maps line numbers to page numbers in the original dictionary

## Code Structure

### Main Modules

- **`Main.hs`** - Entry point that orchestrates the entire processing pipeline
- **`ApteParser.hs`** - Parses the raw dictionary text into structured data
- **`ApteExpander.hs`** - Expands compound words and generates the final data files
- **`ApteRecords.hs`** - Defines the core data structures (Term, Record, Location)
- **`Sandhi.hs`** - Implements Sanskrit sandhi rules for word joining
- **`Natva.hs`** - Handles specific Sanskrit transformations like णत्व (retroflex conversion)
- **`Utils.hs`** - General utility functions and path definitions
- **`JsonUtils.hs`** - Functions for JSON serialization and deserialization
- **`SEncode.hs`** - Sanskrit encoding utilities (SLP1, Devanagari conversion)

### Key Data Structures

- **`Term`** - The basic unit of the dictionary, representing a headword with its meanings and related terms
  - Contains banner (headword), meanings, grammatical information, morphological variants, and compound words
  - Tracks ancestry to maintain the hierarchical structure of the dictionary

- **`Location`** - Represents the position of a term in the dictionary hierarchy
  - Various types (L_, B_, S_, M_, etc.) track different levels and types of entries
  - Used to build the "address" of each term in the dictionary

- **`Record`** - Wraps a Term with additional metadata (k1, k2 keys)

## Processing Pipeline

The main processing pipeline consists of these steps:

1. **Parsing** (`parserMain` in `ApteParser.hs`)
   - Reads the raw dictionary text
   - Parses it into structured `Term` objects
   - Stores intermediate results in JSON format

2. **Compound Expansion** (`expanderMain` in `ApteExpander.hs`)
   - Takes the parsed terms
   - Expands compound words using sandhi rules
   - Builds the hierarchical structure of terms

3. **Data Generation**
   - `shardAndStore` - Creates individual JSON files for each term
   - `tabulate` - Generates lookup tables for the web interface

## How to Use

### Prerequisites

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

### Building and Running

```bash
# Build the project
stack build

# Run the full processing pipeline
stack run

# Run specific functions in GHCi
stack ghci
> parserMain  -- Run just the parser
> expanderMain  -- Run just the expander
```

### Input and Output

- **Input**: Raw dictionary data in `apteDir.nosync/input/`
- **Output**:
  - Sharded JSON files in `apteDir.nosync/output/sharded/`
    - Each file is named with a numeric ID (e.g., `12345.json`)
    - Contains a single Term object with its hierarchy of meanings, morphological variants, and compounds
    - Used by the frontend to display dictionary entries
  - Lookup table in `apteDir.nosync/output/table_new.txt`
    - Tab-separated file with Sanskrit words in Devanagari and their corresponding term IDs
    - Used by the frontend for word search functionality
    - Format: `देवनागरी_शब्द\tterm_id\tparent_term`
  - Page mapping in `apteDir.nosync/output/pagemarks.json`
    - JSON mapping between line numbers and page numbers in the original dictionary
    - Used by the frontend to provide links to scanned dictionary pages
    - Format: `{"line_number": "page_number", ...}`

## Technical Details

### Term Structure for Frontend Integration

The `Term` structure is the primary data format consumed by the frontend. Understanding its structure is essential for frontend development:

```haskell
data Term = Term
  { __line :: Maybe Int                      -- Line number in original dictionary
  , _ancestry :: Maybe [Location]            -- Hierarchical path in dictionary
  , _banner :: Maybe String                  -- Headword (often in shorthand)
  , _bannerExp :: Maybe [Either String String] -- Expanded headword
  , _gram :: Maybe [String]                  -- Grammatical information
  , _meanings :: Maybe [String]              -- List of meanings
  , _morphisms :: Maybe [Term]               -- Morphological variants
  , _samasas :: Maybe [Term]                 -- Compound words
  , _spent :: Maybe String                   -- Processed text
  , _unspent :: Maybe String                 -- Unprocessed text
  }
```

Key fields for frontend use:
- `_banner`: The headword displayed at the top of an entry
- `_bannerExp`: The expanded form of the headword (resolves shorthand notations)
- `_meanings`: The definitions or translations of the term
- `_morphisms`: Grammatical variants (e.g., feminine forms)
- `_samasas`: Compound words that include this term
- `_ancestry`: Used to determine the term's position in the hierarchy

The frontend typically needs to recursively process `_morphisms` and `_samasas` to display the complete entry.

### Location Structure for Term Navigation

The `Location` structure defines the position of a term in the dictionary hierarchy:

```haskell
data Location = L_ {loc :: String}           -- Main entry (<L> value in ap90.txt)
             | B_ {loc :: String}            -- Base word (e.g., दुर्, त्रि, नृ)
             | M_ {loc :: String}            -- Morphological variant
             | S_ {loc :: String}            -- Compound (e.g., दुर्-गः, त्रि-पुरं, नृ-पः)
             | S_M_ {loc :: String}          -- Compound variant (e.g., दुर्-ग (--गः))
             | S_M_S_ {loc :: String}        -- Compound variant with subcompound
             | S_M_S_M_ {loc :: String}      -- Deeper nesting
             | S_S_ {loc :: String}          -- Compound with subcompound
             | S_S_M_ {loc :: String}        -- Compound with subcompound variant
```

The frontend uses this structure to:
- Determine the nesting level of terms (maximum depth is 4)
- Format the display based on term type
- Create proper navigation links
- Highlight the correct term when searched

Common patterns include:
- `B_ → S_ → S_M → S_M_S`
- `B_ → S_ → S_S_ → S_S_M`

Understanding these patterns helps in properly rendering the hierarchical structure in the UI.

### Sanskrit Processing

The code implements several Sanskrit-specific processing features:

1. **Sandhi Rules** - Implemented in `Sandhi.hs`, these rules govern how words combine in Sanskrit
2. **Compound Expansion** - Expands compounds like "राज-पुत्रः" into their components
3. **Encoding Conversion** - Converts between SLP1 (a Latin transliteration) and Devanagari

### Parser Combinators

The parsing is implemented using Haskell's `ReadP` parser combinators, which allow for:

- Declarative parsing of complex nested structures
- Handling of ambiguities in the dictionary format
- Graceful error recovery

### Performance Considerations

- The code uses lazy evaluation to process the large dictionary efficiently
- Intermediate results are stored to avoid recomputation
- Sharding splits the output into manageable chunks for the web interface

## Development

### Adding New Features

To extend the functionality:

1. For new Sanskrit processing rules, modify `Sandhi.hs` or `Natva.hs`
2. For changes to the parsing logic, modify `ApteParser.hs`
3. For changes to the expansion logic, modify `ApteExpander.hs`

### Testing

The code includes various helper functions for testing in GHCi:

```haskell
-- Get a term by its key
getTermByK "someKey" rs

-- View the ancestry of a term
briefAnc term

-- Get hints about a term's processing state
hint term
```

## Troubleshooting

Common issues:

- **Memory usage**: Processing the full dictionary requires significant memory. Use `+RTS -M2G` to increase the memory limit.
- **Parsing errors**: Check the raw input for formatting inconsistencies.
- **Missing output**: Ensure all input files are in the correct locations.

## References

- [Sanskrit Sandhi Rules](https://en.wikipedia.org/wiki/Sandhi)
- [SLP1 Encoding](https://en.wikipedia.org/wiki/SLP1)
- [Apte Dictionary](https://dsal.uchicago.edu/dictionaries/apte/)