/**
 * Sanskrit word lookup for browser environments
 * This module provides functions to:
 * 1. Convert SLP1 to intermediate Devanagari form
 * 2. Look up the intermediate form in a pre-loaded lookup table
 * 3. Return the number from the second field if found
 */

(function(window) {
    'use strict';
    
    // Map from SLP1 to Devanagari characters
    const SLP1_TO_DEVANAGARI = {
        // Vowels
        'a': 'अ', 'A': 'आ', 'i': 'इ', 'I': 'ई', 
        'u': 'उ', 'U': 'ऊ', 'f': 'ऋ', 'F': 'ॠ',
        'x': 'ऌ', 'X': 'ॡ', 'e': 'ए', 'E': 'ऐ',
        'o': 'ओ', 'O': 'औ',
        
        // Consonants
        'k': 'क', 'K': 'ख', 'g': 'ग', 'G': 'घ', 'N': 'ङ',
        'c': 'च', 'C': 'छ', 'j': 'ज', 'J': 'झ', 'Y': 'ञ',
        'w': 'ट', 'W': 'ठ', 'q': 'ड', 'Q': 'ढ', 'R': 'ण',
        't': 'त', 'T': 'थ', 'd': 'द', 'D': 'ध', 'n': 'न',
        'p': 'प', 'P': 'फ', 'b': 'ब', 'B': 'भ', 'm': 'म',
        'y': 'य', 'r': 'र', 'l': 'ल', 'v': 'व', 
        'S': 'श', 'z': 'ष', 's': 'स', 'h': 'ह', 'L': 'ळ',
        
        // Other marks
        'M': 'ं', 'H': 'ः', '~': 'ँ',
        
        // Digits
        '0': '०', '1': '१', '2': '२', '3': '३', '4': '४',
        '5': '५', '6': '६', '7': '७', '8': '८', '9': '९'
    };
    
    // Lookup table
    let lookupTable = null;
    
    /**
     * Convert SLP1 to intermediate Devanagari form
     * @param {string} slp1Text - The SLP1 encoded text to convert
     * @return {string} - The intermediate Devanagari form
     */
    function slp1ToIntermediate(slp1Text) {
        let result = '';
        for (let i = 0; i < slp1Text.length; i++) {
            const char = slp1Text[i];
            if (SLP1_TO_DEVANAGARI[char]) {
                result += SLP1_TO_DEVANAGARI[char];
            } else {
                // Pass through any character not in our mapping
                result += char;
            }
        }
        return result;
    }
    
    /**
     * Load the lookup table from a URL
     * @param {string} url - URL to the lookup table file
     * @return {Promise<Object>} - Promise resolving to the lookup table
     */
    function loadLookupTable(url) {
        return new Promise((resolve, reject) => {
            if (lookupTable) {
                resolve(lookupTable);
                return;
            }
            
            fetch(url)
                .then(response => response.text())
                .then(text => {
                    lookupTable = {};
                    const lines = text.split('\n');
                    
                    for (const line of lines) {
                        if (!line.trim()) continue;
                        
                        const [intermediateForm, number, classification] = line.split(':').map(s => s.trim());
                        lookupTable[intermediateForm] = number;
                    }
                    
                    resolve(lookupTable);
                })
                .catch(error => {
                    console.error('Error loading lookup table:', error);
                    reject(error);
                });
        });
    }
    
    /**
     * Look up a Sanskrit word in SLP1 encoding
     * @param {string} slp1Word - The word in SLP1 encoding
     * @return {string|null} - The number corresponding to the word, or null if not found
     */
    function lookupSanskritWord(slp1Word) {
        if (!lookupTable) {
            throw new Error('Lookup table not loaded. Call loadLookupTable first.');
        }
        
        // Convert to intermediate form
        const intermediateForm = slp1ToIntermediate(slp1Word);
        
        // Look up the intermediate form
        return lookupTable[intermediateForm] || null;
    }
    
    // Create SanskritLookup object
    const SanskritLookup = {
        slp1ToIntermediate,
        loadLookupTable,
        lookupSanskritWord
    };
    
    // Add to Sanscript if it exists
    if (typeof window.Sanscript !== 'undefined') {
        window.Sanscript.toIntermediate = slp1ToIntermediate;
        window.Sanscript.lookupSanskritWord = function(word, from) {
            // Convert to SLP1 first if not already in SLP1
            const slp1Word = from && from !== 'slp1' ? 
                window.Sanscript.t(word, from, 'slp1') : word;
            
            return lookupSanskritWord(slp1Word);
        };
    }
    
    // Export to window
    window.SanskritLookup = SanskritLookup;
    
})(window);
