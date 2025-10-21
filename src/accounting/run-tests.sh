#!/bin/bash
# Test Runner Script for Account Management System

echo "Running tests for Account Management System..."
echo ""

# Navigate to the accounting directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Run Jest
./node_modules/.bin/jest "$@"
