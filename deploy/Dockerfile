FROM node:18-slim

WORKDIR /app

# Install unzip
RUN apt-get update && apt-get install -y unzip && apt-get clean

# Copy package files and install dependencies
COPY package.json ./
RUN npm install --production

# Copy application code
COPY server/ ./server/
COPY public/ ./public/

# Create data directory
RUN mkdir -p /app/data/sharded

# Copy data files (if they exist locally)
COPY data.zip /app/

# Extract data files
RUN if [ -f /app/data.zip ]; then unzip -o /app/data.zip -d /app/ && \
    cp -r /app/apteDir.nosync/output/* /app/data/ && \
    rm -rf /app/apteDir.nosync /app/data.zip; fi

# Expose the port
EXPOSE 8080

# Start the application
CMD ["node", "server/serve.js"]
