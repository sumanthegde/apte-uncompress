
        // No default path button anymore

        // Devanagari dependent vowel marks to independent vowels mapping
        const VOWEL_MARKS_TO_VOWELS = {
            '\u093e': '\u0906', // ा -> आ
            '\u093f': '\u0907', // ि -> इ
            '\u0940': '\u0908', // ी -> ई
            '\u0941': '\u0909', // ु -> उ
            '\u0942': '\u090a', // ू -> ऊ
            '\u0943': '\u090b', // ृ -> ऋ
            '\u0944': '\u0960', // ॄ -> ॠ
            '\u0962': '\u090c', // ॢ -> ऌ
            '\u0963': '\u0961', // ॣ -> ॡ
            '\u0945': '\u090f', // ॅ -> ए
            '\u0947': '\u090f', // े -> ए
            '\u0948': '\u0910', // ै -> ऐ
            '\u0949': '\u0913', // ॉ -> ओ
            '\u094b': '\u0913', // ो -> ओ
            '\u094c': '\u0914'  // ौ -> औ
        };

        // Function to convert nasal consonants to anusvara (ं) when followed by consonants from their group
        function anusvarafy(text) {
            if (!text) return '';

            // Define consonant groups
            const groups = {
                'ङ': ['क', 'ख', 'ग', 'घ'],
                'ञ': ['च', 'छ', 'ज', 'झ'],
                'ण': ['ट', 'ठ', 'ड', 'ढ'],
                'न': ['त', 'थ', 'द', 'ध'],
                'म': ['प', 'फ', 'ब', 'भ','म']
            };

            let result = '';

            for (let i = 0; i < text.length; i++) {
                const currentChar = text[i];
                const nextChar = i < text.length - 1 ? text[i + 1] : '';

                // Check if current character is a nasal consonant and next character is in its group
                let shouldConvert = false;

                if (groups[currentChar] && nextChar) {
                    shouldConvert = groups[currentChar].includes(nextChar);
                }

                if (shouldConvert) {
                    // Replace with anusvara
                    result += 'ं';
                } else {
                    // Keep the original character
                    result += currentChar;
                }
            }

            return result;
        }

        // Function to convert Devanagari to intermediate form
        function devanagariToIntermediate(text) {
            // Result buffer
            let result = '';

            // Process each character
            for (let i = 0; i < text.length; i++) {
                const char = text[i];

                // Check if it's a dependent vowel mark
                if (VOWEL_MARKS_TO_VOWELS[char]) {
                    // Convert to independent vowel
                    result += VOWEL_MARKS_TO_VOWELS[char];
                }
                // Check if it's a virama (्)
                else if (char === '\u094d') {
                    // Skip the virama - we don't include it in the intermediate form
                    continue;
                }
                // For all other characters, just add them as is
                else {
                    result += char;
                    const consonants = "कखगघङचछजझञटठडढणतथदधनपफबभमयरलवशषसहळ";
                    if (consonants.includes(char)) {
                        if(i+1 == text.length)
                            result += 'अ';
                        else if(!VOWEL_MARKS_TO_VOWELS[text[i+1]] && text[i+1] != '\u094d')
                            result += 'अ';
                    }
                }
            }

            return result;
        }

        // Mapping from Devanagari to SLP1
        const DEVANAGARI_TO_SLP1 = {
            // Vowels
            'अ': 'a', 'आ': 'A', 'इ': 'i', 'ई': 'I',
            'उ': 'u', 'ऊ': 'U', 'ऋ': 'f', 'ॠ': 'F',
            'ऌ': 'x', 'ॡ': 'X', 'ए': 'e', 'ऐ': 'E',
            'ओ': 'o', 'औ': 'O',

            // Consonants
            'क': 'k', 'ख': 'K', 'ग': 'g', 'घ': 'G', 'ङ': 'N',
            'च': 'c', 'छ': 'C', 'ज': 'j', 'झ': 'J', 'ञ': 'Y',
            'ट': 'w', 'ठ': 'W', 'ड': 'q', 'ढ': 'Q', 'ण': 'R',
            'त': 't', 'थ': 'T', 'द': 'd', 'ध': 'D', 'न': 'n',
            'प': 'p', 'फ': 'P', 'ब': 'b', 'भ': 'B', 'म': 'm',
            'य': 'y', 'र': 'r', 'ल': 'l', 'व': 'v',
            'श': 'S', 'ष': 'z', 'स': 's', 'ह': 'h', 'ळ': 'L',

            // Other marks
            'ं': 'M', 'ः': 'H', 'ँ': '~',

            // Digits
            '०': '0', '१': '1', '२': '2', '३': '3', '४': '4',
            '५': '5', '६': '6', '७': '7', '८': '8', '९': '9'
        };

        // Function to convert intermediate form back to regular Devanagari
        function intermediateToDevanagari(text) {
            if (!text) return '';

            // Convert intermediate Devanagari to SLP1
            let slp1 = '';
            for (let i = 0; i < text.length; i++) {
                const char = text[i];
                if (DEVANAGARI_TO_SLP1[char]) {
                    slp1 += DEVANAGARI_TO_SLP1[char];
                } else {
                    // Pass through any character not in our mapping
                    slp1 += char;
                }
            }

            // Use sanscript.js to convert SLP1 to Devanagari
            try {
                return Sanscript.t(slp1, 'slp1', 'devanagari');
            } catch (e) {

                return text; // Return original text if conversion fails
            }
        }


        // Function to check if input is likely in SLP1 format
        function isLikelySLP1(text) {
            const slp1Markers = /[aAiIuUfFxXeEoOMH~]|[kKgGNcCjJYwWqQRtTdDnpPbBmyrlvSzshL]/;
            return slp1Markers.test(text);
        }