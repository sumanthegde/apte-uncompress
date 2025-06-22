        // {# #} to Devanagari, remove {% %}, stylize <ab></ab>
        function processSpecialText(text) {
            if (!text) return '';

            // Convert SLP1 encoded Sanskrit text in {# #} brackets to Devanagari
            text = text.replace(/\{#(.*?)#\}/g, function(match, p1) {
                try {
                    // Convert SLP1 to Devanagari
                    const devanagari = Sanscript.t(p1, 'slp1', 'devanagari');
                    return `<span class="devanagari">${devanagari}</span>`;
                } catch (e) {
                    // If conversion fails, just return the original text with special styling
                    return `<span class="special-text">${p1}</span>`;
                }
            });

            // Remove {% %} brackets but style the content differently
            text = text.replace(/\{%(.*?)%\}/g, '<span class="special-text">$1</span>');

            // Remove <ab> </ab> tags but style the content differently
            text = text.replace(/<ab>(.*?)<\/ab>/g, '<span class="special-text">$1</span>');

            return text;
        }

        // {# #} to Devanagari, remove {% %}, <ab></ab>
        function processPlainText(text) {
            if (!text) return '';

            // Convert SLP1 encoded Sanskrit text in {# #} brackets to Devanagari
            text = text.replace(/\{#(.*?)#\}/g, function(match, p1) {
                try {
                    // Convert SLP1 to Devanagari
                    return Sanscript.t(p1, 'slp1', 'devanagari');
                } catch (e) {
                    // If conversion fails, just return the original text
                    return p1;
                }
            });

            // Remove {% %} brackets without special styling
            text = text.replace(/\{%(.*?)%\}/g, '$1');

            // Remove <ab> </ab> tags without special styling
            text = text.replace(/<ab>(.*?)<\/ab>/g, '$1');

            return text;
        }

        // squeeze whitespace, handle {@--n@}
        function processMeaning(text) {
            if (!text) return '';

            // Normalize all whitespace (including newlines) to a single space
            text = text.replace(/\s+/g, ' ');

            // Handle {@--n@} format at the beginning of meanings
            text = text.replace(/\{@--(\d+)@\}/g, '<span class="meaning-number">$1</span> ');

            // Handle {n} format at the beginning of meanings
            text = text.replace(/^\{(\d+)\}\s*/g, '<span class="meaning-number">$1</span> ');

            return text;
        }
