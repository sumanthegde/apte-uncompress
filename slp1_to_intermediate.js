/**
 * Convert SLP1 to intermediate Devanagari form
 * This function converts SLP1 encoded text to an intermediate Devanagari form
 * where each character is mapped directly without combining vowels or applying viramas
 * 
 * Example: 'sikta' -> 'सइकतअ' instead of 'सिक्त'
 * 
 * @param {string} slp1Text - The SLP1 encoded text to convert
 * @return {string} - The intermediate Devanagari form
 */
function slp1ToIntermediate(slp1Text) {
    // Define character mappings from SLP1 to Devanagari
    const charMap = {
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
    
    // Process each character
    let result = '';
    for (let i = 0; i < slp1Text.length; i++) {
        const char = slp1Text[i];
        if (charMap[char]) {
            result += charMap[char];
        } else {
            // Pass through any character not in our mapping
            result += char;
        }
    }
    
    return result;
}

// Example usage:
// slp1ToIntermediate('sikta') -> 'सइकतअ'
// slp1ToIntermediate('rAma') -> 'रआमअ'

// If you're using this with sanscript.js, you can add this as a method:
if (typeof Sanscript !== 'undefined') {
    Sanscript.toIntermediate = function(data, from) {
        if (from === 'slp1') {
            return slp1ToIntermediate(data);
        } else {
            // First convert to SLP1, then to intermediate
            const slp1 = Sanscript.t(data, from, 'slp1');
            return slp1ToIntermediate(slp1);
        }
    };
}
