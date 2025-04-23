const http = require('http');
const fs = require('fs');
const path = require('path');

const PORT = 8081;

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

const server = http.createServer((req, res) => {
  console.log(`Request: ${req.url}`);

  // Handle numeric paths like /12345
  const numericMatch = req.url.match(numericPathRegex);
  if (numericMatch) {
    const number = numericMatch[1];
    const shardedFilePath = path.join(__dirname, `apteDir.nosync/output/sharded/${number}.json`);

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
    // Just serve the HTML page, it will request the JSON file
    const htmlPath = path.join(__dirname, 'term-viewer-simple.html');
    fs.readFile(htmlPath, (error, content) => {
      if (error) {
        console.error(`Error reading HTML file: ${error.message}`);
        res.writeHead(500);
        res.end(`Server Error: ${error.message}`);
        return;
      }

      console.log(`Serving term-viewer-simple.html with path parameter: ${req.url}`);
      res.writeHead(200, { 'Content-Type': 'text/html' });
      res.end(content);
    });
    return;
  }

  // Handle direct requests for numeric JSON files
  const numericJsonMatch = req.url.match(/^\/([0-9]+)\.json$/);
  if (numericJsonMatch) {
    const number = numericJsonMatch[1];
    const shardedFilePath = path.join(__dirname, `apteDir.nosync/output/sharded/${number}.json`);

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
    ? path.join(__dirname, 'term-viewer-simple.html')
    : path.join(__dirname, req.url);

  // Special case for es.json
  if (req.url === '/es.json') {
    console.log(`Serving es.json from: ${path.join(__dirname, 'apteDir.nosync/output/es.json')}`);
    filePath = path.join(__dirname, 'apteDir.nosync/output/es.json');
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

server.listen(PORT, () => {
  console.log(`Server running at http://localhost:${PORT}/`);
  console.log(`Access the Term Viewer at http://localhost:${PORT}/`);
  console.log(`Access individual terms at http://localhost:${PORT}/<number>`);
  console.log(`For example: http://localhost:${PORT}/9091`);
});
