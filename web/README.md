# Function Call Hierarchies in index.html

## 1. Typing in Input Box
document.getElementById('devanagari-input').addEventListener('input')
  ↓
getSuggestions(inputText)
  ├─ isLikelySLP1(text) [checks input format]
  ├─ devanagariToIntermediate(text) [if needed]
  ├─ findFirstGreaterOrEqual(input) [binary search]
  └─ filter suggestions by:
      - Exact matches
      - Prefix matches
      - Alternative forms
  ↓
renderSuggestions(suggestions)
  ├─ create suggestion DOM elements
  ├─ attach click handlers (→ navigateToTerm)
  └─ position/show suggestions container

Note: Debouncing may be applied to optimize performance

## 2. Selecting an Autocomplete Suggestion
document.addEventListener('click') (on suggestion)
  ↓
navigateToTerm(termId, word)
  ↓
loadTermWithContext(termId)
  ├─ loadSingleTerm(termId)
  ├─ renderTermsInRange(startId, endId)
  │   ├─ flattenTerm(term)
  │   ├─ processTermContent(term)
  │   ├─ processMorphisms/Samasas(term)
  │   └─ renderTermRow(term)
  └─ scrollToHighlightedElement(termId)

## 3. Up Arrow Key Navigation
document.addEventListener('keydown') (ArrowUp)
  ↓
getLowestLoadedId()
  ↓
loadMoreTerms(termId, 'up')
  ├─ getPageInfo(lineNumber)
  ├─ renderTermsInRange(startId, endId)
  │   (same sub-calls as above)
  └─ trimExcessTerms()


These diagrams show the main function call chains for these interactions.