# Account Management System - Node.js Application

## Overview

This is a Node.js implementation of the legacy COBOL Account Management System. The application has been converted while preserving:
- ✅ Original three-tier architecture
- ✅ All business logic and rules
- ✅ Data integrity and validation
- ✅ Menu options and user interface flow

## Architecture

The application maintains the same modular structure as the COBOL original:

```
┌─────────────────────────────────────────┐
│     MainProgram (Presentation Layer)    │
│  - User interface and menu system       │
│  - Input handling and routing           │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│      Operations (Business Logic Layer)  │
│  - View Balance (TOTAL)                 │
│  - Credit Account                       │
│  - Debit Account                        │
│  - Business rule enforcement            │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│       DataProgram (Data Layer)          │
│  - Balance storage and retrieval        │
│  - READ and WRITE operations            │
└─────────────────────────────────────────┘
```

## Business Rules Preserved

1. **Initial Balance**: $1,000.00 on application start
2. **Maximum Balance**: $999,999.99 (PIC 9(6)V99 equivalent)
3. **Minimum Balance**: $0.00 (no negative balances allowed)
4. **Decimal Precision**: 2 decimal places for all monetary values
5. **Insufficient Funds Check**: Prevents overdrafts (balance >= debit amount)
6. **Transaction Atomicity**: Failed transactions don't modify the balance

## Installation

The application uses only Node.js built-in modules (readline), so no external dependencies are required.

```bash
cd src/accounting
npm install
```

## Running the Application

### Option 1: Command Line

```bash
# From the src/accounting directory
npm start

# Or directly with node
node index.js
```

### Option 2: VS Code Debugger

1. Open VS Code
2. Press `F5` or go to **Run > Start Debugging**
3. Select **"Launch Account Management System"** from the configurations

The `.vscode/launch.json` file includes two configurations:
- **Launch Account Management System** - Normal execution
- **Debug Account Management System** - Debug mode with tracing

## Usage

Once the application starts, you'll see the menu:

```
--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4):
```

### Menu Options

1. **View Balance** - Display current account balance
2. **Credit Account** - Add funds to the account
   - Prompts for amount
   - Validates maximum balance limit
   - Updates balance immediately
3. **Debit Account** - Withdraw funds from the account
   - Prompts for amount
   - Checks for sufficient funds
   - Only updates balance if sufficient funds available
4. **Exit** - Terminate the application

## Examples

### Viewing Balance
```
Enter your choice (1-4): 1
Current balance: 1000.00
```

### Crediting Account
```
Enter your choice (1-4): 2
Enter credit amount: 500.00
Amount credited. New balance: 1500.00
```

### Successful Debit
```
Enter your choice (1-4): 3
Enter debit amount: 300.00
Amount debited. New balance: 1200.00
```

### Insufficient Funds
```
Enter your choice (1-4): 3
Enter debit amount: 2000.00
Insufficient funds for this debit.
```

## Code Structure

### index.js

The single file contains three classes mirroring the COBOL programs:

1. **DataProgram Class**
   - Manages `storageBalance` (equivalent to COBOL STORAGE-BALANCE)
   - `execute(operation, balance)` method handles READ/WRITE

2. **Operations Class**
   - `execute(operationType)` - Routes to specific operations
   - `viewBalance()` - TOTAL operation
   - `creditAccount()` - CREDIT operation with validation
   - `debitAccount()` - DEBIT operation with insufficient funds check
   - `promptForAmount()` - Input validation helper

3. **MainProgram Class**
   - `displayMenu()` - Shows menu options
   - `getUserChoice()` - Accepts user input
   - `run()` - Main program loop (equivalent to PERFORM UNTIL)

## Differences from COBOL Version

### Improvements
- ✨ Enhanced input validation (checks for NaN, negative values)
- ✨ Explicit maximum balance validation on credits
- ✨ Asynchronous I/O using async/await
- ✨ Detailed code comments and documentation
- ✨ Error handling for edge cases

### Maintained Compatibility
- Same menu structure and options
- Identical business logic
- Same validation rules
- Equivalent data flow
- Matching output messages

## Testing

Refer to the `docs/TESTPLAN.md` for comprehensive test cases covering:
- All menu operations
- Business rule validation
- Edge cases and boundaries
- Data integrity scenarios

## Migration Notes

This Node.js implementation was created by:
1. Analyzing the COBOL source code structure
2. Reviewing the data flow diagrams in `docs/README.md`
3. Preserving the three-tier architecture
4. Implementing equivalent business logic
5. Maintaining the same user experience

## Future Enhancements

Potential improvements while maintaining backward compatibility:
- Add persistent storage (file or database)
- Implement transaction history logging
- Add multiple account support
- Create REST API endpoints
- Add unit and integration tests
- Implement authentication

## License

MIT
