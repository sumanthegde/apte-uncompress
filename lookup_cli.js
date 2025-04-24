#!/usr/bin/env node

/**
 * Command-line tool for Sanskrit word lookup
 * Usage: node lookup_cli.js <slp1_word>
 */

const { slp1ToIntermediate, lookupSanskritWord } = require('./sanskrit_lookup');

// Get the word from command line arguments
const word = process.argv[2];

if (!word) {
    console.error('Please provide a Sanskrit word in SLP1 encoding');
    console.error('Usage: node lookup_cli.js <slp1_word>');
    process.exit(1);
}

// Convert to intermediate form
const intermediateForm = slp1ToIntermediate(word);
console.log(`SLP1: ${word}`);
console.log(`Intermediate form: ${intermediateForm}`);

// Look up the word
const result = lookupSanskritWord(word);
if (result) {
    console.log(`Found: ${result}`);
} else {
    console.log('Not found in lookup table');
}
