FROM node:18-slim

WORKDIR /app

# Copy package files and install dependencies first
COPY ./web/package.json ./
RUN npm install --production

# Copy server files explicitly
COPY ./web/server/serve.js /app/server/serve.js
COPY ./web/public/ /app/public/

# Create data directory
RUN mkdir -p /app/data2

# Populate the data directory (TODO: move to /app/data/ which is a mounted volume. Needs start.sh)
COPY ./haskell/apteDir.nosync/output/apte_data.sqlite /app/data2/
COPY ./haskell/apteDir.nosync/output/pagemarks.json /app/data2/
COPY ./haskell/apteDir.nosync/output/table_new.txt /app/data2/

EXPOSE 8080

# Debug: List the contents to verify structure (remove these lines once working)
RUN ls -la /app/
RUN ls -la /app/server/

# Start the application with the correct paths
CMD ["node", "server/serve.js", "--public", "/app/public", "--data", "/app/data2"]