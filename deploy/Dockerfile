FROM node:18-slim

WORKDIR /app

# Install unzip and zip
RUN apt-get update && apt-get install -y unzip zip && apt-get clean

# Copy package files and install dependencies
COPY package.json ./
RUN npm install --production

# Copy application code
COPY server/ ./server/
COPY public/ ./public/

# Create data directory
RUN mkdir -p /app/data/

COPY data.zip /app/data
WORKDIR /app/data
RUN unzip data.zip


WORKDIR /app
# Expose the port
EXPOSE 8080

# Start the application with the correct paths
CMD ["node", "server/serve.js", "--public", "/app/public", "--data", "/app/data"]
