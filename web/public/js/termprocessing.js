// bannerExp or banner. Collect Rights
function processBannerExp(term) {
    // If no _bannerExp, return just the _banner
    if (!term._bannerExp || !term._bannerExp.length) {
        if (term._banner) {
            // Process special formatting but keep the text in black
            // No warning symbol, keeping it minimal
            return {
                text: processPlainText(term._banner),
                tooltip: 'Could not expand.',
                isExpanded: false,
                matchesSearch: false
            };
        }
        return { text: '', tooltip: '', isExpanded: false, matchesSearch: false };
    }

    // Extract all 'Right' values from _bannerExp
    const rightValues = term._bannerExp
        .filter(item => item.Right)
        .map(item => item.Right);

    // If there are Right values, use them
    if (rightValues.length > 0) {
        // Convert SLP1 to Devanagari for each Right value and check for matches
        let matchesSearch = false;
        const convertedValues = rightValues.map(value => {
            try {
                // Check if the value looks like Sanskrit (contains SLP1 characters)
                // This regex checks for common SLP1 characters that indicate Sanskrit
                if (/[aAiIuUfFxXeEoOMH~]|[kKgGNcCjJYwWqQRtTdDnpPbBmyrlvSzshL]/.test(value)) {
                    const devanagari = Sanscript.t(value, 'slp1', 'devanagari');

                    // Check if this matches the searched word
                    if (searchedWord && devanagari === searchedWord) {
                        matchesSearch = true;
                        // console.log(`Found match for '${searchedWord}' in term:`, term);
                    }

                    return `<span class="devanagari">${devanagari}</span>`;
                } else {
                    // Check if this matches the searched word (for non-Sanskrit)
                    if (searchedWord && value === searchedWord) {
                        matchesSearch = true;
                        // console.log(`Found match for '${searchedWord}' in term:`, term);
                    }
                    return value; // Not Sanskrit, return as is
                }
            } catch (e) {
                return value; // If conversion fails, return original
            }
        });

        return {
            text: convertedValues.sort().join(', '),
            tooltip: term._banner ? `${processPlainText(term._banner)}` : '',
            isExpanded: true,
            matchesSearch: matchesSearch
        };
    }

    // If all are Left values, use just the _banner
    if (term._banner) {
        // Process special formatting but keep the text in black
        // No warning symbol, keeping it minimal
        return {
            text: processPlainText(term._banner),
            tooltip: 'Could not expand',
            isExpanded: false,
            matchesSearch: false
        };
    }

    // Fallback
    return { text: '', tooltip: '', isExpanded: false, matchesSearch: false };
}

// Function to process a term's basic content (banner, grammar, meanings)
function processBannerGramMeans(term) {
    let content = '';
    let matchesSearch = false;
    let meaningTextMatchesSeach = false;

    // Process banner/bannerExp
    const bannerInfo = processBannerExp(term);
    if (bannerInfo.text) {
        // Extract plain text from banner for the search URL
        const plainBannerText = bannerInfo.text.replace(/<[^>]*>/g, '');
        // Create the sanskritkosha.com search URL
        const searchUrl = `https://sanskritkosha.com/?search=${encodeURIComponent(plainBannerText)}`;

        // Add the external link icon before the banner
        content += `<a href="${searchUrl}" target="_blank" title="Search on sanskritkosha.com"><svg class="external-link-icon" viewBox="0 0 768 1024" xmlns="http://www.w3.org/2000/svg"><path d="M640 768H128V257.90599999999995L256 256V128H0v768h768V576H640V768zM384 128l128 128L320 448l128 128 192-192 128 128V128H384z"/></svg></a>`;

        // Add the banner element
        content += `<span class="banner" data-tooltip="${bannerInfo.tooltip}">${bannerInfo.text}</span>`;
        matchesSearch = bannerInfo.matchesSearch;
    }

    // Grammar info
    if (term._gram && term._gram.length) {
        content += `<span class="gram">${term._gram.map(processSpecialText).join(' ')}</span>`;
    }

    // Meanings
    term._meanings.forEach(meaning => {
        const processedMeaning = processMeaning(processSpecialText(meaning));
        // Check if the processed meaning contains the searched word (case-insensitive)
        let hasMatch = false;
        if (typeof searchedWord === 'string' && searchedWord.length) {
            hasMatch = processedMeaning.toLowerCase().includes(searchedWord.toLowerCase());
            meaningTextMatchesSeach = true;
        }
        const spanClass = textSearchMode && hasMatch ? 'meaning meaning-match' : 'meaning';
        content += `<span class="${spanClass}">${processedMeaning}</span>`;
    });

    return { content, matchesSearch, meaningTextMatchesSeach };
}

// Recursive function to process morphisms at any nesting level
function processMorphisms(morphisms, nestingLevel = 0) {
    if (!morphisms || !morphisms.length) return { html: '', hasMatch: false };

    let result = '';
    let hasMatch = false;

    morphisms.forEach(morphism => {
        const { content: morphismContent, matchesSearch } = processBannerGramMeans(morphism);
        let nestedContent = '';
        let nestedHasMatch = false;

        // Process nested morphisms recursively
        if (morphism._morphisms && morphism._morphisms.length) {
            const nestedMorphisms = processMorphisms(morphism._morphisms, nestingLevel + 1);
            nestedContent += nestedMorphisms.html;
            if (nestedMorphisms.hasMatch) nestedHasMatch = true;
        }

        // Process nested samasas if any
        if (morphism._samasas && morphism._samasas.length) {
            const nestedSamasas = processSamasas(morphism._samasas, nestingLevel + 1);
            nestedContent += nestedSamasas.html;
            if (nestedSamasas.hasMatch) nestedHasMatch = true;
        }

        // Add the morphism content to the result if not empty
        if (morphismContent || nestedContent) {
            // Get ancestry info if available
            let ancestryInfo = '';
            if (morphism._ancestry && morphism._ancestry.length) {
                ancestryInfo = morphism._ancestry.join(' / ');
            }

            // Determine if this morphism or any of its children match the search
            const thisMatchesSearch = matchesSearch || nestedHasMatch;
            if (thisMatchesSearch) hasMatch = true;

            // Add appropriate classes based on search match
            const searchMatchClass = thisMatchesSearch ? ' search-match' : '';

            // Add nesting level as a data attribute for potential styling
            result += `<span class="nested-term nested-morphism${searchMatchClass}" data-type="Morphism" data-nesting-level="${nestingLevel}" data-ancestry="${ancestryInfo}">${morphismContent}${nestedContent}</span>`;
        }
    });

    return { html: result, hasMatch };
}

// Recursive function to process samasas at any nesting level
function processSamasas(samasas, nestingLevel = 0) {
    if (!samasas || !samasas.length) return { html: '', hasMatch: false };

    let result = '';
    let hasMatch = false;

    samasas.forEach(samasa => {
        const { content: samasaContent, matchesSearch } = processBannerGramMeans(samasa);
        let nestedContent = '';
        let nestedHasMatch = false;

        // Process nested samasas recursively
        if (samasa._samasas && samasa._samasas.length) {
            const nestedSamasas = processSamasas(samasa._samasas, nestingLevel + 1);
            nestedContent += nestedSamasas.html;
            if (nestedSamasas.hasMatch) nestedHasMatch = true;
        }

        // Process nested morphisms if any
        if (samasa._morphisms && samasa._morphisms.length) {
            const nestedMorphisms = processMorphisms(samasa._morphisms, nestingLevel + 1);
            nestedContent += nestedMorphisms.html;
            if (nestedMorphisms.hasMatch) nestedHasMatch = true;
        }

        // Add the samasa content to the result if not empty
        if (samasaContent || nestedContent) {
            // Get ancestry info if available
            let ancestryInfo = '';
            if (samasa._ancestry && samasa._ancestry.length) {
                ancestryInfo = samasa._ancestry.join(' / ');
            }

            // Determine if this samasa or any of its children match the search
            const thisMatchesSearch = matchesSearch || nestedHasMatch;
            if (thisMatchesSearch) hasMatch = true;

            // Add appropriate classes based on search match
            const searchMatchClass = thisMatchesSearch ? ' search-match' : '';

            // Add nesting level as a data attribute for potential styling
            result += `<span class="nested-term nested-samasa${searchMatchClass}" data-type="Samasa" data-nesting-level="${nestingLevel}" data-ancestry="${ancestryInfo}">${samasaContent}${nestedContent}</span>`;
        }
    });

    return { html: result, hasMatch };
}

// Function to flatten a term and all its nested terms into a single row
function flattenTerm(term) {
    let result = '';
    let hasMatch = false;

    // Line number and page link removed

    // Process the term's own content
    const { content: termContent, matchesSearch } = processBannerGramMeans(term);
    result += termContent;
    if (matchesSearch) hasMatch = true;

    // Process morphisms with recursive handling for any nesting level
    if (term._morphisms && term._morphisms.length) {
        const morphismsResult = processMorphisms(term._morphisms);
        result += morphismsResult.html;
        if (morphismsResult.hasMatch) hasMatch = true;
    }

    // Process samasas with recursive handling for any nesting level
    if (term._samasas && term._samasas.length) {
        const samasasResult = processSamasas(term._samasas);
        result += samasasResult.html;
        if (samasasResult.hasMatch) hasMatch = true;
    }

    return { content: result, hasMatch };
}

// Function to render a term row
function processTerm(term) {
    const { content: rowContent, hasMatch } = flattenTerm(term);
    const matchClass = hasMatch ? ' has-match' : '';
    return `<div class="term-row${matchClass}">${rowContent}</div>`;
}
