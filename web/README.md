# Front-End Call Flow Overview

This document summarises how the autocomplete UI in `public/index.html` responds to three key user interactions:

1. Typing in the search box
2. Selecting an autocomplete suggestion
3. Paging upward with the Arrow-Up key

Each section lists the originating browser event followed by the primary function chain that is executed in `serve.js` (and helpers).  Nested bullets represent direct calls made by the preceding function.

---

## 1 Typing in the Search Box

**Event** `input` on `#devanagari-input`

```
document.getElementById('devanagari-input')
        .addEventListener('input', onInput)
```

Call sequence

- **`onInput()`** wraps the logic in a debounce helper for performance.
  - **`getSuggestions(inputText)`**
    - `isLikelySLP1(text)` – detect whether the text is already SLP-1.
    - `devanagariToIntermediate(text)` – transliterate Devanāgarī to SLP-1 (if necessary).
    - `findFirstGreaterOrEqual(input)` – binary search the term index.
    - Filter resulting list for
      - exact matches
      - prefix matches
      - alternative forms
  - **`renderSuggestions(suggestions)`**
    - create DOM elements for each suggestion
    - attach click handlers (→ `navigateToTerm`)
    - position and show the suggestions container

---

## 2 Selecting a Suggestion

**Event** `click` on a suggestion element (delegated listener on `document`)

Call sequence

- **`navigateToTerm(termId, word)`**
  - **`loadTermWithContext(termId)`**
    - `loadSingleTerm(termId)` – fetch and cache the requested term
    - **`renderTermsInRange(startId, endId)`**
      - `flattenTerm(term)` – normalise nested structures
      - `processTermContent(term)` – highlight headwords, accents, etc.
      - `processMorphisms(term)` / `processSamasas(term)` – annotate morphology & compounds
      - `renderTermRow(term)` – insert a `<tr>` into the results table
    - `scrollToHighlightedElement(termId)` – bring the selected term into view

---

## 3 Paging Upward (Infinite Scroll)

**Event** `keydown` ArrowUp when the first visible term is highlighted

Call sequence

- **`getLowestLoadedId()`** – determine the topmost loaded dictionary ID
- **`loadMoreTerms(termId, direction = 'up')`**
  - `getPageInfo(lineNumber)` – compute the range of IDs to load
  - **`renderTermsInRange(startId, endId)`** (see above for internal calls)
  - `trimExcessTerms()` – keep the DOM small by removing off-screen rows

---

### Notes

* All expensive operations (search, rendering) are either debounced or chunked to preserve frame rate.
* The code assumes that terms are stored in a dense numeric ID space, enabling rapid range queries.
* For further details, consult `public/index.html` and `server/serve.js`.