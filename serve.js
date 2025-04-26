const http = require('http');
const fs = require('fs');
const path = require('path');

// Parse command line arguments
const args = process.argv.slice(2);
const argMap = {};
for (let i = 0; i < args.length; i += 2) {
  if (args[i].startsWith('--') && i + 1 < args.length) {
    argMap[args[i].slice(2)] = args[i + 1];
  }
}

const PORT = parseInt(argMap.port || '8080', 10);

// Define paths based on command line arguments or defaults
const PUBLIC_DIR = argMap.public ? path.resolve(argMap.public) : path.join(__dirname);
const DATA_DIR = argMap.data ? path.resolve(argMap.data) : path.join(__dirname, 'apteDir.nosync/output');

console.log(`Using PUBLIC_DIR: ${PUBLIC_DIR}`);
console.log(`Using DATA_DIR: ${DATA_DIR}`);

const MIME_TYPES = {
  '.html': 'text/html',
  '.js': 'text/javascript',
  '.css': 'text/css',
  '.json': 'application/json',
  '.png': 'image/png',
  '.jpg': 'image/jpg',
  '.gif': 'image/gif',
  '.svg': 'image/svg+xml',
  '.ico': 'image/x-icon',
  '.txt': 'text/plain',
};

// Regular expression to match numeric paths like /12345
const numericPathRegex = /^\/([0-9]+)$/;

// Load pagemarks data at server startup
let pagemarks = {};
let sortedPagemarkKeys = [];

// Load table_new.txt data at server startup
let tableNewLines = [];

try {
  const pagemarksPath = path.join(DATA_DIR, 'pagemarks.json');
  const pagemarksData = fs.readFileSync(pagemarksPath, 'utf8');
  pagemarks = JSON.parse(pagemarksData);

  // Extract and sort keys numerically
  sortedPagemarkKeys = Object.keys(pagemarks).map(key => parseInt(key, 10)).sort((a, b) => a - b);

  console.log(`Loaded pagemarks data with ${sortedPagemarkKeys.length} entries`);
} catch (error) {
  console.error('Error loading pagemarks data:', error.message);
}

// Load table_new.txt
try {
  const tableNewPath = path.join(DATA_DIR, 'table_new.txt');
  const tableNewData = fs.readFileSync(tableNewPath, 'utf8');
  tableNewLines = tableNewData.split('\n').filter(line => line.trim());

  console.log(`Loaded table_new.txt with ${tableNewLines.length} entries`);
} catch (error) {
  console.error('Error loading table_new.txt:', error.message);
}

const server = http.createServer((req, res) => {
  console.log(`Request: ${req.url}`);

  // Handle numeric paths like /12345
  const numericMatch = req.url.match(numericPathRegex);
  if (numericMatch) {
    const number = numericMatch[1];
    const shardedFilePath = path.join(DATA_DIR, `sharded/${number}.json`);

    console.log(`Handling numeric path: ${req.url}, checking file: ${shardedFilePath}`);

    // Check if file exists
    if (!fs.existsSync(shardedFilePath)) {
      console.error(`File not found: ${shardedFilePath}`);
      res.writeHead(404);
      res.end(`Term with ID ${number} not found`);
      return;
    }

    try {
      // Read the sharded file
      const content = fs.readFileSync(shardedFilePath, 'utf8');
      const termObject = JSON.parse(content);

      console.log(`Successfully read term ${number}: ${termObject._banner}`);

      // Redirect to the term-viewer-simple.html with a path parameter
      res.writeHead(302, { 'Location': `/?path=/${number}.json` });
      res.end();
      return;
    } catch (error) {
      console.error(`Error processing file: ${error.message}`);
      res.writeHead(500);
      res.end(`Server Error: ${error.message}`);
      return;
    }
  }

  // Handle numeric JSON paths (for the redirected requests)
  if (req.url.startsWith('/?path=/') && req.url.endsWith('.json')) {
    // This is the term-viewer-simple.html page with a path parameter
    // Serve the HTML page with injected table data
    const htmlPath = path.join(PUBLIC_DIR, 'term-viewer-simple.html');
    fs.readFile(htmlPath, 'utf8', (error, content) => {
      if (error) {
        console.error(`Error reading HTML file: ${error.message}`);
        res.writeHead(500);
        res.end(`Server Error: ${error.message}`);
        return;
      }

      // Create a script tag with the table data
      const tableDataScript = `<script>const preloadedTableData = ${JSON.stringify(tableNewLines)};</script>`;

      // Insert the script tag before the closing </head> tag
      const modifiedContent = content.replace('</head>', `${tableDataScript}\n</head>`);

      console.log(`Serving term-viewer-simple.html with path parameter and injected table data: ${req.url}`);
      res.writeHead(200, { 'Content-Type': 'text/html' });
      res.end(modifiedContent);
    });
    return;
  }

  // Handle direct requests for numeric JSON files
  const numericJsonMatch = req.url.match(/^\/([0-9]+)\.json$/);
  if (numericJsonMatch) {
    const number = numericJsonMatch[1];
    const shardedFilePath = path.join(DATA_DIR, `sharded/${number}.json`);

    console.log(`Handling numeric JSON path: ${req.url}, checking file: ${shardedFilePath}`);

    // Check if file exists
    if (!fs.existsSync(shardedFilePath)) {
      console.error(`File not found: ${shardedFilePath}`);
      res.writeHead(404);
      res.end(`Term with ID ${number} not found`);
      return;
    }

    try {
      // Read the sharded file
      const content = fs.readFileSync(shardedFilePath, 'utf8');
      const termObject = JSON.parse(content);

      console.log(`Serving JSON for term ${number}`);
      const wrappedContent = JSON.stringify([termObject]); // Wrap in array to match es.json format
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(wrappedContent);
      return;
    } catch (error) {
      console.error(`Error processing file: ${error.message}`);
      res.writeHead(500);
      res.end(`Server Error: ${error.message}`);
      return;
    }
  }

  // Handle root path and other paths
  let filePath = req.url === '/'
    ? path.join(PUBLIC_DIR, 'term-viewer-simple.html')
    : path.join(PUBLIC_DIR, req.url);

  // Special case for term-viewer-simple.html - inject table_new.txt data
  if (req.url === '/' || req.url === '/term-viewer-simple.html') {
    console.log('Serving term-viewer-simple.html with injected table_new.txt data');

    // Read the HTML file
    fs.readFile(path.join(PUBLIC_DIR, 'term-viewer-simple.html'), 'utf8', (error, content) => {
      if (error) {
        console.error(`Error reading HTML file: ${error.message}`);
        res.writeHead(500);
        res.end(`Server Error: ${error.message}`);
        return;
      }

      // Create a script tag with the table data
      const tableDataScript = `
      <script>
      // Pre-loaded table data from table_new.txt
      const preloadedTableData = ${JSON.stringify(tableNewLines)};
      console.log("Pre-loaded table data with", preloadedTableData.length, "entries");
      </script>
      `;

      // Insert the script tag before the closing </head> tag
      const modifiedContent = content.replace('</head>', `${tableDataScript}\n</head>`);

      // Send the modified HTML
      res.writeHead(200, { 'Content-Type': 'text/html' });
      res.end(modifiedContent);
    });
    return;
  }

  // Special case for es.json
  if (req.url === '/es.json') {
    console.log(`Serving es.json from: ${path.join(DATA_DIR, 'es.json')}`);
    filePath = path.join(DATA_DIR, 'es.json');
  }

  // Special case for pagemarks.json
  if (req.url === '/pagemarks.json') {
    console.log(`Serving pagemarks.json from: ${path.join(DATA_DIR, 'pagemarks.json')}`);
    filePath = path.join(DATA_DIR, 'pagemarks.json');
  }

  // Special case for pagemarks-data.json - serve the pre-loaded and processed pagemarks data
  if (req.url === '/pagemarks-data.json') {
    console.log('Serving pre-loaded pagemarks data');
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      pagemarks: pagemarks,
      sortedKeys: sortedPagemarkKeys
    }));
    return;
  }

  // Special case for table_new.txt
  if (req.url === '/table_new.txt') {
    console.log(`Serving table_new.txt from: ${path.join(DATA_DIR, 'table_new.txt')}`);
    filePath = path.join(DATA_DIR, 'table_new.txt');
  }

  // Special case for table2.txt (for backward compatibility)
  if (req.url === '/table2.txt') {
    console.log(`Serving table2.txt from: ${path.join(DATA_DIR, 'table2.txt')}`);
    filePath = path.join(DATA_DIR, 'table2.txt');
  }

  // Special case for sanscript.js
  if (req.url === '/sanscript.js') {
    console.log(`Serving sanscript.js from: ${path.join(PUBLIC_DIR, 'sanscript.js')}`);
    filePath = path.join(PUBLIC_DIR, 'sanscript.js');
  }

  const extname = String(path.extname(filePath)).toLowerCase();
  const contentType = MIME_TYPES[extname] || 'application/octet-stream';

  fs.readFile(filePath, (error, content) => {
    if (error) {
      if (error.code === 'ENOENT') {
        console.error(`File not found: ${filePath}`);
        res.writeHead(404);
        res.end('File not found');
      } else {
        console.error(`Server error: ${error.code}`);
        res.writeHead(500);
        res.end(`Server Error: ${error.code}`);
      }
    } else {
      console.log(`Serving file: ${filePath}`);
      res.writeHead(200, { 'Content-Type': contentType });
      res.end(content, 'utf-8');
    }
  });
});

server.listen(PORT, '0.0.0.0', () => {
  console.log(`Server running at http://0.0.0.0:${PORT}/`);
  console.log(`Access the Term Viewer at http://0.0.0.0:${PORT}/`);
  console.log(`Access individual terms at http://0.0.0.0:${PORT}/<number>`);
  console.log(`For example: http://0.0.0.0:${PORT}/9091`);
});

server.on('error', (error) => {
  console.error('Server error:', error);
});

console.log('Starting server...');
