# Apte Dictionary Deployment

This directory contains files for deploying the Apte Dictionary server to Fly.io.

## Deployment Steps

### First-time Setup

1. Install the Fly.io CLI:
   ```bash
   curl -L https://fly.io/install.sh | sh
   ```

2. Login to Fly.io:
   ```bash
   fly auth login
   ```

3. Launch your app (first time only):
   ```bash
   fly launch
   ```
   - This will guide you through creating an app
   - When asked about database, choose "No"
   - When asked about deployment, choose "Yes"

4. Create a volume for your data:
   ```bash
   fly volumes create apte_data --size 3
   ```
   This volume is mounted at `/app/data` and stores your dictionary data persistently across deployments.

### Deployment Options

There are two main deployment scenarios:

#### 1. Deploying New Code WITH New Data

Use this when you want to update both the application code and the dictionary data:

```bash
cd deploy
./prepare-deploy-and-data.sh  # This runs cleanup first
fly deploy
```

This will:
- Clean up any files from previous deployments
- Copy the necessary application files to the deploy directory
- Create a data.zip file with your latest dictionary data
- Deploy the application with the new code and data

#### 2. Deploying New Code WITHOUT New Data

Use this when you want to update only the application code, keeping the existing dictionary data:

```bash
cd deploy
./prepare-deploy-without-data.sh  # This runs cleanup first
fly deploy
```

This will:
- Clean up any files from previous deployments
- Copy the necessary application files to the deploy directory
- Create a dummy data.zip file (which won't affect the existing data)
- Deploy the application with the new code only

### How It Works

The deployment process uses a clever approach to handle data:

1. The prepare scripts create either a real or dummy data.zip file:
   - `prepare-deploy-and-data.sh` creates a zip with actual dictionary data at the root level
   - `prepare-deploy-without-data.sh` creates a zip with just an empty dummy folder

2. The Dockerfile copies this zip file into the container's data directory and extracts it:
   ```dockerfile
   COPY data.zip /app/data
   WORKDIR /app/data
   RUN unzip data.zip
   ```

3. When the container starts:
   - If real data was included: The extracted files (pagemarks.json, table_new.txt, sharded/) are directly in /app/data/
   - If dummy data was included: Only an empty folder is extracted, and the existing data in the volume is used

4. The volume mount at /app/data takes precedence:
   - When deploying with new data: The new data replaces what's in the volume
   - When deploying without new data: The existing data in the volume is preserved

This ensures that:
- You can deploy with or without new data
- The existing data in the volume is preserved when needed
- The deployment process is simple and reliable

### About the Cleanup Process

The deployment process includes automatic cleanup:

1. **Before preparation**: The prepare scripts run `cleanup.sh` to ensure a clean slate
2. **After deployment**: No manual cleanup is needed

The cleanup script:
- Removes copied application files (serve.js, term-viewer-simple.html, sanscript.js)
- Removes the data.zip file (if it exists)
- Removes dictionary data files (pagemarks.json, table_new.txt, and all files in the sharded/ directory)
- Preserves the directory structure (data/, data/sharded/) for future deployments

You can also run the cleanup script manually if needed:
```bash
./cleanup.sh
```

These temporary files are only needed during the deployment process and can be safely removed, as the deployed application uses the data stored in the persistent volume on the server.

## Monitoring

- View logs: `fly logs`
- SSH into app: `fly ssh console`
- Check app status: `fly status`

## Troubleshooting

If you encounter issues:

1. Check the logs: `fly logs`
2. Verify data files exist on the volume:
   ```bash
   fly ssh console
   ls -la /app/data
   ls -la /app/data/sharded
   ```
3. Restart the app: `fly apps restart`
