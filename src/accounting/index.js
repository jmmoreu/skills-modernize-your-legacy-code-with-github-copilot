/**
 * Account Management System - Node.js Implementation
 * Converted from COBOL legacy application
 * 
 * Architecture:
 * - DataProgram: Data layer (storage and retrieval)
 * - Operations: Business logic layer (account operations)
 * - MainProgram: Presentation layer (user interface)
 */

const readline = require('readline');

// ============================================================================
// DATA LAYER - DataProgram (data.cob)
// ============================================================================

/**
 * DataProgram - Manages persistent storage of account balance
 * Equivalent to COBOL DataProgram (data.cob)
 */
class DataProgram {
    constructor() {
        // STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
        this.storageBalance = 1000.00;
    }

    /**
     * Executes READ or WRITE operation
     * @param {string} operation - 'READ' or 'WRITE'
     * @param {number} balance - Balance value for WRITE, or container for READ
     * @returns {number} Current balance for READ operation
     */
    execute(operation, balance = null) {
        if (operation === 'READ') {
            // Return current stored balance
            return this.storageBalance;
        } else if (operation === 'WRITE') {
            // Update stored balance
            this.storageBalance = balance;
            return balance;
        }
        return this.storageBalance;
    }
}

// ============================================================================
// BUSINESS LOGIC LAYER - Operations (operations.cob)
// ============================================================================

/**
 * Operations - Implements core business logic for account operations
 * Equivalent to COBOL Operations program (operations.cob)
 */
class Operations {
    constructor(dataProgram, rl) {
        this.dataProgram = dataProgram;
        this.rl = rl;
    }

    /**
     * Execute operation based on type
     * @param {string} operationType - 'TOTAL ', 'CREDIT', or 'DEBIT '
     * @returns {Promise<void>}
     */
    async execute(operationType) {
        if (operationType === 'TOTAL ') {
            await this.viewBalance();
        } else if (operationType === 'CREDIT') {
            await this.creditAccount();
        } else if (operationType === 'DEBIT ') {
            await this.debitAccount();
        }
    }

    /**
     * TOTAL operation - View current balance
     */
    async viewBalance() {
        const finalBalance = this.dataProgram.execute('READ');
        console.log(`Current balance: ${finalBalance.toFixed(2)}`);
    }

    /**
     * CREDIT operation - Add funds to account
     */
    async creditAccount() {
        const amount = await this.promptForAmount('Enter credit amount: ');
        
        if (amount === null) return; // User cancelled
        
        // Read current balance
        let finalBalance = this.dataProgram.execute('READ');
        
        // Add amount to balance
        finalBalance += amount;
        
        // Validate maximum balance (PIC 9(6)V99 max: 999,999.99)
        if (finalBalance > 999999.99) {
            console.log('Error: Transaction would exceed maximum balance of $999,999.99');
            return;
        }
        
        // Write updated balance
        this.dataProgram.execute('WRITE', finalBalance);
        
        console.log(`Amount credited. New balance: ${finalBalance.toFixed(2)}`);
    }

    /**
     * DEBIT operation - Withdraw funds from account
     * Business Rule: Prevents overdrafts (balance >= debit amount)
     */
    async debitAccount() {
        const amount = await this.promptForAmount('Enter debit amount: ');
        
        if (amount === null) return; // User cancelled
        
        // Read current balance
        let finalBalance = this.dataProgram.execute('READ');
        
        // Check for sufficient funds (critical business rule)
        if (finalBalance >= amount) {
            // Subtract amount from balance
            finalBalance -= amount;
            
            // Write updated balance
            this.dataProgram.execute('WRITE', finalBalance);
            
            console.log(`Amount debited. New balance: ${finalBalance.toFixed(2)}`);
        } else {
            // Insufficient funds - no balance modification
            console.log('Insufficient funds for this debit.');
        }
    }

    /**
     * Prompt user for numeric amount
     * @param {string} prompt - Prompt message
     * @returns {Promise<number|null>} Amount entered or null if invalid
     */
    promptForAmount(prompt) {
        return new Promise((resolve) => {
            this.rl.question(prompt, (input) => {
                const amount = parseFloat(input);
                
                // Validate input
                if (isNaN(amount) || amount < 0) {
                    console.log('Invalid amount. Please enter a positive number.');
                    resolve(null);
                    return;
                }
                
                // Round to 2 decimal places (PIC 9(6)V99 precision)
                const roundedAmount = Math.round(amount * 100) / 100;
                
                // Validate maximum amount
                if (roundedAmount > 999999.99) {
                    console.log('Amount exceeds maximum allowed value of $999,999.99');
                    resolve(null);
                    return;
                }
                
                resolve(roundedAmount);
            });
        });
    }
}

// ============================================================================
// PRESENTATION LAYER - MainProgram (main.cob)
// ============================================================================

/**
 * MainProgram - User interface and menu system
 * Equivalent to COBOL MainProgram (main.cob)
 */
class MainProgram {
    constructor() {
        this.rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout
        });
        
        this.dataProgram = new DataProgram();
        this.operations = new Operations(this.dataProgram, this.rl);
        this.continueFlag = 'YES';
    }

    /**
     * Display the main menu
     */
    displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
    }

    /**
     * Get user's menu choice
     * @returns {Promise<number>} User's choice (1-4)
     */
    getUserChoice() {
        return new Promise((resolve) => {
            this.rl.question('Enter your choice (1-4): ', (input) => {
                const choice = parseInt(input, 10);
                resolve(choice);
            });
        });
    }

    /**
     * Main program logic loop
     * Equivalent to COBOL PERFORM UNTIL CONTINUE-FLAG = 'NO'
     */
    async run() {
        while (this.continueFlag === 'YES') {
            this.displayMenu();
            const userChoice = await this.getUserChoice();

            // Evaluate user choice (COBOL EVALUATE statement)
            switch (userChoice) {
                case 1:
                    // View Balance
                    await this.operations.execute('TOTAL ');
                    break;
                    
                case 2:
                    // Credit Account
                    await this.operations.execute('CREDIT');
                    break;
                    
                case 3:
                    // Debit Account
                    await this.operations.execute('DEBIT ');
                    break;
                    
                case 4:
                    // Exit
                    this.continueFlag = 'NO';
                    break;
                    
                default:
                    // Invalid choice
                    console.log('Invalid choice, please select 1-4.');
                    break;
            }
        }

        // Program termination
        console.log('Exiting the program. Goodbye!');
        this.rl.close();
    }
}

// ============================================================================
// APPLICATION ENTRY POINT
// ============================================================================

// Export classes for testing
module.exports = {
    DataProgram,
    Operations,
    MainProgram
};

// Start the application only if this file is run directly (not imported for testing)
if (require.main === module) {
    const app = new MainProgram();
    app.run().catch((error) => {
        console.error('Application error:', error);
        process.exit(1);
    });
}
