#!/bin/bash
# ==============================================================================
# Server Setup Script
# ==============================================================================
# Run this ONCE on the server to prepare for deployment
# Usage: ssh razinka@laguna.ku.lt 'bash -s' < setup_server.sh
# ==============================================================================

echo "=========================================="
echo "Setting up EcoNeTool on Shiny Server"
echo "=========================================="
echo ""

# Create directories
echo "Creating directories..."
sudo mkdir -p /srv/shiny-server/EcoNeTool
sudo mkdir -p /srv/shiny-server/backups/EcoNeTool

# Set ownership
echo "Setting ownership..."
sudo chown -R razinka:razinka /srv/shiny-server/EcoNeTool
sudo chown -R razinka:razinka /srv/shiny-server/backups/EcoNeTool

# Set permissions
echo "Setting permissions..."
sudo chmod 755 /srv/shiny-server/EcoNeTool
sudo chmod 755 /srv/shiny-server/backups/EcoNeTool

echo ""
echo "✓ Server setup complete!"
echo ""
echo "You can now deploy using:"
echo "  deploy_to_server()"
echo ""
