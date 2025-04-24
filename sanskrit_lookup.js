/**
 * Sanskrit word lookup using intermediate form
 * This module provides functions to:
 * 1. Convert SLP1 to intermediate Devanagari form
 * 2. Look up the intermediate form in table_new.txt
 * 3. Return the number from the second field if found
 */

// Import required modules
const fs = require('fs');
const path = require('path');

// Path to the lookup table
const TABLE_PATH = path.resolve(__dirname, '../apteDir.nosync/output/table_new.txt');

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

// Lookup table cache
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
 * Load the lookup table from file
 * @return {Object} - Map of intermediate forms to their corresponding numbers
 */
function loadLookupTable() {
    if (lookupTable) return lookupTable;
    
    lookupTable = {};
    
    try {
        const fileContent = fs.readFileSync(TABLE_PATH, 'utf8');
        const lines = fileContent.split('\n');
        
        for (const line of lines) {
            if (!line.trim()) continue;
            
            const [intermediateForm, number, classification] = line.split(':').map(s => s.trim());
            lookupTable[intermediateForm] = number;
        }
    } catch (error) {
        console.error('Error loading lookup table:', error);
        return {};
    }
    
    return lookupTable;
}

/**
 * Look up a Sanskrit word in SLP1 encoding
 * @param {string} slp1Word - The word in SLP1 encoding
 * @return {string|null} - The number corresponding to the word, or null if not found
 */
function lookupSanskritWord(slp1Word) {
    // Convert to intermediate form
    const intermediateForm = slp1ToIntermediate(slp1Word);
    
    // Load lookup table if not already loaded
    const table = loadLookupTable();
    
    // Look up the intermediate form
    return table[intermediateForm] || null;
}

// Export functions
module.exports = {
    slp1ToIntermediate,
    lookupSanskritWord
};
