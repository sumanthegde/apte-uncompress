        // Configuration
        let pagemarks = {}; // Will store the pagemarks data
        let sortedPagemarkKeys = []; // Will store the sorted keys for binary search
        let currentTermId = null; // Current term ID being viewed
        let requestedTermId = null; // Track the term that was specifically requested
        let searchedWord = null; // Store the Devanagari word that was searched
        let textSearchMode = false; // Whether searching the meaning texts
        let isLoading = false; // Flag to prevent multiple simultaneous loads

        // Infinite scroll configuration`
        const WINDOW_SIZE = 16; // Current term + 15 after
        const WINDOW_BEFORE = 0; // 0 terms before
        const WINDOW_AFTER = 15; // 15 terms after
        let loadedTerms = {}; // Map of termId -> term data
        let loadedTermIds = new Set(); // Set of loaded term IDs

        // Predictive text configuration
        let parsedTableData = []; // Will store the parsed table data as arrays
        const INITIAL_SUGGESTIONS_COUNT = 20; // Initial number of suggestions to collect
        const MAX_SUGGESTIONS = 12; // Maximum number of suggestions to show after filtering
        let suggestionToEntryMap = new Map(); // Map from suggestion text to entry ID and label
        let selectedSuggestionIndex = -1; // Handle keyboard events in the Devanagari input field


        const BATCH_SIZE = 7; // Load half the WINDOW_AFTER size at once
        const SCROLL_TIMEOUT = 300; // Increased timeout for larger payloads
        let isLoadingBottom = false; // Flag to track loading state for bottom indicator
        let isLoadingTop = false; // Flag to track loading state for top indicator

        // Base URL for page links
        const PAGE_URL_BASE = "https://www.sanskrit-lexicon.uni-koeln.de/scans/csl-apidev/servepdf.php?dict=AP90&page=";
        const TERM_URL_BASE = "https://cdn.jsdelivr.net/gh/sumanthegde/apte-uncompress@latest/kosha-nested";

        const meaningSearchBeginner = '/';