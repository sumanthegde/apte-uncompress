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

### Uploading Data

After deploying, you need to upload your dictionary data:

1. Create a tar archive of your data:
   ```bash
   tar -czf data.tar.gz ../apteDir.nosync/output/pagemarks.json ../apteDir.nosync/output/table_new.txt ../apteDir.nosync/output/sharded/
   ```

2. Copy to the Fly VM:
   ```bash
   fly ssh sftp shell
   # In the SFTP shell:
   put data.tar.gz /tmp/
   exit
   ```

3. SSH into the VM and extract:
   ```bash
   fly ssh console
   # In the SSH shell:
   cd /app
   mkdir -p /app/data
   tar -xzf /tmp/data.tar.gz -C /app/data
   # Verify files were extracted
   ls -la /app/data
   exit
   ```

4. Restart your application:
   ```bash
   fly apps restart
   ```

### Subsequent Deployments

For code changes (without data changes):

```bash
fly deploy
```

For data changes:
1. Generate new data with the Haskell application
2. Follow the "Uploading Data" steps above

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
