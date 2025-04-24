/**
 * Example of integrating the Sanskrit lookup functionality with the term viewer
 * 
 * This shows how to:
 * 1. Load the lookup table
 * 2. Look up words in SLP1 format
 * 3. Navigate to the corresponding term
 */

// Wait for DOM and scripts to load
document.addEventListener('DOMContentLoaded', function() {
    // Path to the lookup table
    const TABLE_URL = '/apteDir.nosync/output/table_new.txt';
    
    // Load the lookup table
    SanskritLookup.loadLookupTable(TABLE_URL)
        .then(() => {
            console.log('Lookup table loaded successfully');
            setupWordLookup();
        })
        .catch(error => {
            console.error('Failed to load lookup table:', error);
        });
    
    // Set up word lookup functionality
    function setupWordLookup() {
        // Get or create the search input
        let searchInput = document.getElementById('word-search-input');
        if (!searchInput) {
            // Create the search input if it doesn't exist
            const searchContainer = document.createElement('div');
            searchContainer.className = 'search-container';
            searchContainer.style.marginBottom = '10px';
            
            searchInput = document.createElement('input');
            searchInput.id = 'word-search-input';
            searchInput.type = 'text';
            searchInput.placeholder = 'Enter Sanskrit word in SLP1...';
            searchInput.style.marginRight = '10px';
            
            const searchButton = document.createElement('button');
            searchButton.textContent = 'Look up';
            searchButton.onclick = performLookup;
            
            searchContainer.appendChild(searchInput);
            searchContainer.appendChild(searchButton);
            
            // Add to the page
            const container = document.querySelector('.container');
            if (container) {
                container.insertBefore(searchContainer, container.firstChild);
            } else {
                document.body.insertBefore(searchContainer, document.body.firstChild);
            }
            
            // Add event listener for Enter key
            searchInput.addEventListener('keypress', function(e) {
                if (e.key === 'Enter') {
                    performLookup();
                }
            });
        }
        
        // Function to perform the lookup
        function performLookup() {
            const word = searchInput.value.trim();
            if (!word) return;
            
            // Convert to intermediate form for display
            const intermediateForm = SanskritLookup.slp1ToIntermediate(word);
            console.log(`Looking up: ${word} (${intermediateForm})`);
            
            // Look up the word
            const termId = SanskritLookup.lookupSanskritWord(word);
            if (termId) {
                console.log(`Found term ID: ${termId}`);
                
                // Navigate to the term
                navigateToTerm(termId);
            } else {
                alert(`Word not found: ${intermediateForm}`);
            }
        }
        
        // Function to navigate to a term
        function navigateToTerm(termId) {
            // If you have a term-id-input field, set its value and trigger lookup
            const termIdInput = document.getElementById('term-id-input');
            if (termIdInput) {
                termIdInput.value = termId;
                
                // Trigger the lookup event
                const event = new Event('change');
                termIdInput.dispatchEvent(event);
                
                // Or if you have a specific function to load terms
                if (typeof loadTerm === 'function') {
                    loadTerm(termId);
                }
            } else {
                // Otherwise, navigate to the term directly
                window.location.href = `/${termId}`;
            }
        }
    }
});
