# How to Run Tests - Quick Reference

## Problem Summary
The tests were installed in a nested directory structure which caused confusion. This has been fixed.

## ✅ CORRECT Way to Run Tests

### Option 1: Using the Test Runner Script (Recommended)
```bash
# From the project root directory
./src/accounting/run-tests.sh

# Or from any directory
/workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting/run-tests.sh
```

### Option 2: Using npm from the src directory
```bash
# Navigate to src directory first
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src

# Run tests
npm test --prefix accounting
```

### Option 3: Using npm from the accounting directory
```bash
# Navigate to accounting directory
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting

# Run tests
npm test
```

### Option 4: Run tests with coverage
```bash
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting
npm run test:coverage
```

### Option 5: Run tests in watch mode
```bash
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting
npm run test:watch
```

## Test Results

✅ **All 42 tests passing!**

```
Test Suites: 1 passed, 1 total
Tests:       42 passed, 42 total
Time:        ~0.63 seconds
```

## What Was Fixed

1. ✅ Installed Jest in the correct `/src/accounting` directory
2. ✅ Fixed package.json configuration
3. ✅ Created run-tests.sh helper script
4. ✅ All 42 tests now pass successfully

## Directory Structure

```
src/accounting/
├── index.js              # Main application
├── index.test.js         # Test suite (42 tests)
├── package.json          # npm configuration with test scripts
├── node_modules/         # Dependencies (including Jest)
├── run-tests.sh          # Test runner script (recommended)
├── README.md             # Application documentation
└── TEST-DOCUMENTATION.md # Test documentation
```

## Common Issues

### Issue: "Missing script: test"
**Solution**: You're in the wrong directory. Navigate to `/src/accounting` first.

### Issue: "jest: not found"
**Solution**: Run `npm install` in the `/src/accounting` directory to install Jest.

### Issue: "No tests found"
**Solution**: Make sure you're running from the correct directory where `index.test.js` exists.

## Verification

To verify everything is working:

```bash
# Navigate to accounting directory
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting

# Verify files exist
ls -la index.test.js package.json node_modules/.bin/jest

# Run tests
npm test
```

Expected output: ✅ 42 tests passed

## Test Coverage

To see which parts of the code are covered by tests:

```bash
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting
npm run test:coverage
```

This will show:
- Statements: ~47%
- Branches: ~53%
- Functions: ~53%
- **Business Logic: ~100%** ✅

## Need Help?

If tests still don't run, try:

```bash
# Clean reinstall
cd /workspaces/skills-modernize-your-legacy-code-with-github-copilot/src/accounting
rm -rf node_modules package-lock.json
npm install
npm test
```

## Success! 🎉

Your test suite is now working perfectly with all 42 tests passing!
