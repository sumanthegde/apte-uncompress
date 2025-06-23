// Function to get page info for a line number using binary search
function getPageInfo(lineNumber) {
    // Convert lineNumber to a number if it's a string
    lineNumber = parseInt(lineNumber, 10);

    // If no sorted keys are available, return null
    if (!sortedPagemarkKeys || sortedPagemarkKeys.length === 0) {
        return null;
    }

    // Binary search to find the largest key less than or equal to lineNumber
    let left = 0;
    let right = sortedPagemarkKeys.length - 1;
    let result = -1;

    while (left <= right) {
        const mid = Math.floor((left + right) / 2);
        const midKey = sortedPagemarkKeys[mid];

        if (midKey <= lineNumber) {
            // This key is a candidate, but we need to find the largest such key
            result = mid;
            left = mid + 1;
        } else {
            // This key is too large
            right = mid - 1;
        }
    }

    // If we found a valid key, extract the page number and create the URL
    if (result !== -1) {
        const maxKey = sortedPagemarkKeys[result];
        const pageMarkValue = pagemarks[maxKey];

        // Extract the page number from the format [Page????-?]...
        const match = pageMarkValue.match(/\[Page([^\]]+)\]/);
        if (match && match[1]) {
            const pagePart = match[1].split('-')[0].trim();
            return {
                url: PAGE_URL_BASE + pagePart,
                pageNumber: pagePart
            };
        }
    }

    return null; // Return null if no valid page info could be determined
}


function findFirstGreaterOrEqual(input) {
    // parsedTableData comes from server-injected js
    if (!parsedTableData || parsedTableData.length === 0) {
        return -1;
    }

    // Binary search using the (left + 1 < right) pattern
    let left = 0;
    let right = parsedTableData.length - 1;

    // Handle edge cases first
    if (parsedTableData[left][0] >= input) {
        return left; // First element is already >= input
    }

    if (parsedTableData[right][0] < input) {
        return -1; // All elements are < input
    }

    // Main binary search loop
    while (left + 1 < right) {
        const mid = Math.floor(left + (right - left) / 2); // Avoid potential overflow
        const midValue = parsedTableData[mid][0]; // First field (Devanagari text)

        if (midValue >= input) {
            // This could be our answer or there might be a smaller index
            right = mid;
        } else {
            // This is definitely not our answer, look in the right half
            left = mid;
        }
    }

    // At this point, right is our candidate for the first element >= input
    return right;
}

// Function to add a clickable term ID link to a term container
function createPdfLink(termContainer, termId) {
    // Get the term data
    if (!loadedTerms[termId] || !loadedTerms[termId][0]) {
        console.error(`termId ${termId} no data`);
        return; // No term data available
    }

    const term = loadedTerms[termId][0];
    const lineNumber = term.__line;

    if (!lineNumber) {
        console.error(`termId ${termId} no line number`);

        return; // No line number available
    }

    // Create the link element
    const linkElement = document.createElement('a');
    linkElement.className = 'term-id-link';

    // Get page info for the line number
    const pageInfo = getPageInfo(lineNumber);

    if (pageInfo) {
        // Set the link text to the page number
        linkElement.textContent = termId;
        linkElement.href = pageInfo.url;
        linkElement.title = `See PDF page ${pageInfo.pageNumber}`;
        linkElement.target = '_blank'; // Open in new tab
    } else {
        // If no page info is available, just show the line number without a link
        linkElement.textContent = lineNumber;
        linkElement.style.cursor = 'default';
    }

    // Add the link to the term container
    return (linkElement);
}

// Function to get suggestions based on input text
function getSuggestions(input, filterLabel = null) {
    if (!input || input.length === 0) {
        return [];
    }

    // Convert to intermediate form first, then apply anusvarafy
    const intermediateInput = anusvarafy(devanagariToIntermediate(input));

    // Find the first entry that is lexicographically equal to or greater than the input
    const startIndex = findFirstGreaterOrEqual(intermediateInput);
    if (startIndex === -1) {
        return [];
    }

    // Collect initial suggestions (more than we'll eventually show)
    const initialSuggestions = [];
    const seenIntermediateTexts = []; // For tracking duplicates while preserving order

    // Collect up to INITIAL_SUGGESTIONS_COUNT entries
    for (let i = startIndex; i < parsedTableData.length && initialSuggestions.length < INITIAL_SUGGESTIONS_COUNT; i++) {
        const entry = parsedTableData[i];
        const intermediateText = entry[0];
        const entryId = entry[1];
        const entryLabel = entry[2];

        // Skip entries with labels ending with 'M_' unless it's an exact match
        if (entryLabel && entryLabel.endsWith('M_') && intermediateInput !== intermediateText) {
            continue;
        }

        // Skip duplicates (check intermediate text to maintain order)
        if (seenIntermediateTexts.includes(intermediateText)) {
            continue;
        }

        // Add to our collection and mark as seen
        initialSuggestions.push({
            intermediateText,
            entryId,
            entryLabel
        });
        seenIntermediateTexts.push(intermediateText);
    }

    // Limit to MAX_SUGGESTIONS
    const finalSuggestions = initialSuggestions.slice(0, MAX_SUGGESTIONS);

    // Now convert to Devanagari and store mappings
    return finalSuggestions.map(item => {
        // Convert the intermediate form back to regular Devanagari
        const suggestion = intermediateToDevanagari(item.intermediateText);

        // Store the mapping from suggestion to entry ID for lookup
        suggestionToEntryMap.set(suggestion, {
            id: item.entryId,
            label: item.entryLabel
        });

        return suggestion;
    });
}
