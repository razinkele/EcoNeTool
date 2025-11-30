#!/bin/bash
# ==============================================================================
# SSH Key Setup Script for Windows
# ==============================================================================
# This script sets up SSH key authentication for laguna.ku.lt
# Run this in Git Bash
# ==============================================================================

SERVER="razinka@laguna.ku.lt"
KEY_FILE="$HOME/.ssh/id_ed25519"

echo "=========================================="
echo "SSH Key Setup for laguna.ku.lt"
echo "=========================================="
echo ""

# Step 1: Check if key exists
if [ -f "$KEY_FILE" ]; then
    echo "✓ SSH key already exists: $KEY_FILE"
else
    echo "Generating new SSH key..."
    ssh-keygen -t ed25519 -C "$SERVER" -f "$KEY_FILE" -N ""
    echo "✓ SSH key generated"
fi

echo ""

# Step 2: Display public key
echo "Your public key:"
echo "=========================================="
cat "${KEY_FILE}.pub"
echo "=========================================="
echo ""

# Step 3: Instructions for copying to server
echo "To complete setup, run this command:"
echo ""
echo "type \$HOME/.ssh/id_ed25519.pub | ssh $SERVER \"mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys\""
echo ""
echo "You will be asked for your password ONCE."
echo ""
echo "After that, run this to test:"
echo "ssh $SERVER \"echo 'Success!'\""
echo ""
echo "If it works without asking for password, you're done!"
echo "=========================================="
