const fs = require('fs');
const path = require('path');

const tsvPath = path.join(__dirname, '../web/resources/apte-ref-map.tsv');
const jsPath = path.join(__dirname, '../web/public/js/textprocessing.js');
const pkgPath = path.join(__dirname, '../web/package.json');

// 1. Parse the TSV file
const lines = fs.readFileSync(tsvPath, 'utf-8').split('\n');
lines.shift(); // remove header

const mapping = {};
for (const line of lines) {
    if (!line.trim()) continue;
    const parts = line.split('\t');
    const abbr = parts[0].trim();
    if (!abbr) continue;

    const title = (parts[1] || '').trim();
    const confidence = (parts[2] || '').trim();

    mapping[abbr] = { title, confidence };
}

// 2. Inject into textprocessing.js
let code = fs.readFileSync(jsPath, 'utf-8');

const newLsReferencesStr = 'const lsReferences = ' + JSON.stringify(mapping, null, 2) + ';\n\n';

// Replace the lsReferences object completely if it exists
const matchObj = code.match(/const lsReferences = \{([\s\S]*?)\};\n\n/);
if (matchObj) {
    code = code.replace(matchObj[0], newLsReferencesStr);
} else {
    // Inject at the very top if it doesn't exist
    code = newLsReferencesStr + code;
}

fs.writeFileSync(jsPath, code);
console.log('✅ Successfully updated lsReferences in textprocessing.js');

// 3. Auto-bump the version in package.json to clear browser cache
try {
    let pkg = JSON.parse(fs.readFileSync(pkgPath, 'utf-8'));
    let versionParts = pkg.version.split('.');
    if (versionParts.length === 4) {
        versionParts[3] = parseInt(versionParts[3]) + 1;
        pkg.version = versionParts.join('.');
    } else {
        pkg.version += '.1';
    }
    fs.writeFileSync(pkgPath, JSON.stringify(pkg, null, 2) + '\n');
    console.log(`✅ Bumped version in web/package.json to ${pkg.version}`);
} catch (e) {
    console.error('❌ Failed to bump version in package.json:', e.message);
}
