const http = require('http');
const fs = require('fs');
const path = require('path');
const sqlite3 = require('sqlite3').verbose();
const { open } = require('sqlite');

// Database connection
let db;
async function initDb() {
  const dbPath = path.join(DATA_DIR, 'apte_data.sqlite');
  try {
    db = await open({
      filename: dbPath,
      driver: sqlite3.Database
    });
    console.log('Connected to SQLite database');
  } catch (error) {
    console.error('Error connecting to SQLite database:', error);
    process.exit(1);
  }
}

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
const PUBLIC_DIR = argMap.public ? path.resolve(argMap.public) : path.join(__dirname, '../public');
const DATA_DIR = argMap.data ? path.resolve(argMap.data) : path.join(__dirname, '../../data/output');

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
  console.log(`Req: ${req.url}`);

  let filePath = req.url === '/'
    ? path.join(PUBLIC_DIR, 'index.html')
    : path.join(PUBLIC_DIR, req.url);

  // Security check to prevent directory traversal.
  // We resolve the path and ensure it's within the PUBLIC_DIR.
  const resolvedPath = path.resolve(filePath);
  if (!resolvedPath.startsWith(PUBLIC_DIR)) {
    console.error(`Directory traversal attempt blocked for: ${req.url}`);
    res.writeHead(403, { 'Content-Type': 'text/plain' });
    res.end('Forbidden');
    return;
  }

  if (req.url === '/' || req.url === '/index.html') {
    console.log('Serving index.html with injected table_new.txt data');

    // Read the HTML file
    fs.readFile(path.join(PUBLIC_DIR, 'index.html'), 'utf8', (error, content) => {
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
  } else if(req.url === '/pagemarks-data.json') {
    console.log('Serving pre-loaded pagemarks data');
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      pagemarks: pagemarks,
      sortedKeys: sortedPagemarkKeys
    }));
  } else if(req.url.endsWith('.js') || req.url.endsWith('.css')){ 
    // scripts themselves, or any resource (in public directory)
    
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
  } else if (req.url.startsWith('/text-query')) {
    handleTextQuery(req, res);
  } else {
    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end('Not found');
  }

});

server.on('error', (error) => {
  console.error('Server error:', error);
});

// Initialize database and start server
initDb().then(() => {
  server.listen(PORT, '0.0.0.0', () => {
    console.log(`Server running at http://0.0.0.0:${PORT}/`);
    console.log(`Access the Term Viewer at http://0.0.0.0:${PORT}/`);
    console.log(`Access individual terms at http://0.0.0.0:${PORT}/<number>`);
    console.log(`For example: http://0.0.0.0:${PORT}/9091`);
  });
}).catch(err => {
  console.error('Failed to start server:', err);
  process.exit(1);
});

// Handle text search queries
async function handleTextQuery(req, res) {
  const url = new URL(req.url, `http://${req.headers.host}`);
  const searchTerm = url.searchParams.get('q');
  // Optional pagination offset (?from=NN)
  let offset = parseInt(url.searchParams.get('from') || '0', 10);
  if (isNaN(offset) || offset < 0) offset = 0;

  if (!searchTerm) {
    res.writeHead(400, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ error: 'Search term is required' }));
    return;
  }

  try {
    const query = `
      SELECT metadata.id, expanded_banner, meaning as snippet, ancestry
      FROM meanings 
      JOIN metadata ON meta_id = metadata.id 
      WHERE meaning LIKE ? COLLATE NOCASE
      ORDER BY (instr(lower(meaning), lower(?)) > 0) DESC,
                instr(lower(meaning), lower(?))
      LIMIT 50 OFFSET ?`;
    
    const searchPattern = `%${searchTerm}%`;
    const results = await db.all(query, [searchPattern, searchTerm, searchTerm, offset]);
    console.log(query, searchTerm, offset);
    
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      query: searchTerm,
      results: results,
      offset
    }));
  } catch (error) {
    console.error('Error executing search query:', error);
    res.writeHead(500, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ 
      error: 'Error executing search',
      details: error.message 
    }));
  }
}

console.log('Starting server...');
