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

