/**
 * Unit Tests for Account Management System
 * Based on TESTPLAN.md - Student Account Management System
 * 
 * Test Coverage:
 * - Section 1: Application Initialization
 * - Section 2: View Balance Operations
 * - Section 3: Credit Account Operations
 * - Section 4: Debit Account Operations (Sufficient Funds)
 * - Section 5: Debit Account Operations (Insufficient Funds)
 * - Section 6: Mixed Transaction Scenarios
 * - Section 8: Data Persistence and Integrity
 * - Section 9: Boundary and Edge Cases
 */

const { DataProgram, Operations } = require('./index');

// Mock readline interface for testing
const createMockReadline = () => {
    return {
        question: jest.fn(),
        close: jest.fn()
    };
};

describe('Account Management System - Unit Tests', () => {
    
    // ========================================================================
    // SECTION 1 & 2: Application Initialization and View Balance Operations
    // ========================================================================
    
    describe('Section 1-2: Initialization and View Balance (TC-002, TC-004)', () => {
        
        test('TC-002: Verify initial account balance is $1,000.00', () => {
            const dataProgram = new DataProgram();
            const balance = dataProgram.execute('READ');
            
            expect(balance).toBe(1000.00);
        });
        
        test('TC-004: View initial balance with no transactions', () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            operations.viewBalance();
            
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1000.00');
            
            consoleSpy.mockRestore();
        });
        
        test('TC-005: View balance after credit', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('500.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            await operations.viewBalance();
            
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1500.00');
            
            consoleSpy.mockRestore();
        });
        
        test('TC-006: View balance after debit', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('300.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            await operations.viewBalance();
            
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 700.00');
            
            consoleSpy.mockRestore();
        });
    });
    
    // ========================================================================
    // SECTION 3: Credit Account Operations
    // ========================================================================
    
    describe('Section 3: Credit Account Operations (TC-008 to TC-015)', () => {
        
        test('TC-008: Credit account with valid amount ($500)', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('500.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1500.00');
            expect(dataProgram.execute('READ')).toBe(1500.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-009: Credit account with small amount ($0.01)', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('0.01'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1000.01');
            expect(dataProgram.execute('READ')).toBe(1000.01);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-010: Credit account with large amount ($50,000)', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('50000.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 51000.00');
            expect(dataProgram.execute('READ')).toBe(51000.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-011: Credit account with decimal precision ($123.45)', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('123.45'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1123.45');
            expect(dataProgram.execute('READ')).toBe(1123.45);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-012: Credit account multiple times', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Credit $100
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('100'));
            await operations.creditAccount();
            
            // Credit $200
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.creditAccount();
            
            // Credit $300
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('300'));
            await operations.creditAccount();
            
            expect(dataProgram.execute('READ')).toBe(1600.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-013: Credit account near maximum limit', async () => {
            const dataProgram = new DataProgram();
            // Set balance to $999,000.00
            dataProgram.execute('WRITE', 999000.00);
            
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('999.99'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 999999.99');
            expect(dataProgram.execute('READ')).toBe(999999.99);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-014: Credit account exceeding maximum limit', async () => {
            const dataProgram = new DataProgram();
            // Set balance to $999,000.00
            dataProgram.execute('WRITE', 999000.00);
            
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('1000.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Error: Transaction would exceed maximum balance of $999,999.99');
            expect(dataProgram.execute('READ')).toBe(999000.00); // Balance unchanged
            
            consoleSpy.mockRestore();
        });
        
        test('TC-015: Credit with zero amount', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('0.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1000.00');
            expect(dataProgram.execute('READ')).toBe(1000.00);
            
            consoleSpy.mockRestore();
        });
    });
    
    // ========================================================================
    // SECTION 4: Debit Account Operations (Sufficient Funds)
    // ========================================================================
    
    describe('Section 4: Debit Operations - Sufficient Funds (TC-016 to TC-022)', () => {
        
        test('TC-016: Debit account with valid amount ($300)', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('300.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 700.00');
            expect(dataProgram.execute('READ')).toBe(700.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-017: Debit account with small amount ($0.01)', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('0.01'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 999.99');
            expect(dataProgram.execute('READ')).toBe(999.99);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-018: Debit entire balance', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('1000.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 0.00');
            expect(dataProgram.execute('READ')).toBe(0.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-019: Debit with decimal precision ($234.56)', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('234.56'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 765.44');
            expect(dataProgram.execute('READ')).toBe(765.44);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-020: Debit account multiple times', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Debit $100
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('100'));
            await operations.debitAccount();
            
            // Debit $200
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.debitAccount();
            
            // Debit $150
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('150'));
            await operations.debitAccount();
            
            expect(dataProgram.execute('READ')).toBe(550.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-021: Debit from low balance (sufficient)', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 100.00);
            
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('100.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 0.00');
            expect(dataProgram.execute('READ')).toBe(0.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-022: Debit with zero amount', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('0.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 1000.00');
            expect(dataProgram.execute('READ')).toBe(1000.00);
            
            consoleSpy.mockRestore();
        });
    });
    
    // ========================================================================
    // SECTION 5: Debit Account Operations (Insufficient Funds)
    // ========================================================================
    
    describe('Section 5: Debit Operations - Insufficient Funds (TC-023 to TC-027)', () => {
        
        test('TC-023: Debit exceeding current balance', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('1500.00'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            
            consoleSpy.mockRestore();
        });
        
        test('TC-024: Debit slightly exceeding balance', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('1000.01'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            
            consoleSpy.mockRestore();
        });
        
        test('TC-025: Debit from zero balance', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 0.00);
            
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('0.01'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(0.00); // Balance unchanged
            
            consoleSpy.mockRestore();
        });
        
        test('TC-026: Verify balance unchanged after failed debit', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 500.00);
            
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('600'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(500.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-027: Multiple failed debit attempts', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 100.00);
            
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Attempt 1: Debit $150
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('150'));
            await operations.debitAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            
            // Attempt 2: Debit $200
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.debitAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            
            // Balance should remain unchanged
            expect(dataProgram.execute('READ')).toBe(100.00);
            
            consoleSpy.mockRestore();
        });
    });
    
    // ========================================================================
    // SECTION 6: Mixed Transaction Scenarios
    // ========================================================================
    
    describe('Section 6: Mixed Transaction Scenarios (TC-028 to TC-032)', () => {
        
        test('TC-028: Credit then debit scenario', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Credit $500
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('500'));
            await operations.creditAccount();
            expect(dataProgram.execute('READ')).toBe(1500.00);
            
            // Debit $300
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('300'));
            await operations.debitAccount();
            expect(dataProgram.execute('READ')).toBe(1200.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-029: Debit then credit scenario', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Debit $400
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('400'));
            await operations.debitAccount();
            expect(dataProgram.execute('READ')).toBe(600.00);
            
            // Credit $200
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.creditAccount();
            expect(dataProgram.execute('READ')).toBe(800.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-030: Complex transaction sequence', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Starting balance: $1,000.00
            // Credit $500
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('500'));
            await operations.creditAccount();
            
            // Debit $300
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('300'));
            await operations.debitAccount();
            
            // Credit $100
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('100'));
            await operations.creditAccount();
            
            // Debit $200
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.debitAccount();
            
            // Expected: 1000 + 500 - 300 + 100 - 200 = 1100
            expect(dataProgram.execute('READ')).toBe(1100.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-031: Failed debit followed by successful debit', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 500.00);
            
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Attempt debit $600 (fails)
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('600'));
            await operations.debitAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(500.00);
            
            // Debit $300 (succeeds)
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('300'));
            await operations.debitAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 200.00');
            expect(dataProgram.execute('READ')).toBe(200.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-032: Credit after failed debit', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 100.00);
            
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Attempt debit $200 (fails)
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.debitAccount();
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            
            // Credit $500
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('500'));
            await operations.creditAccount();
            expect(dataProgram.execute('READ')).toBe(600.00);
            
            // Debit $200 (succeeds)
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.debitAccount();
            expect(dataProgram.execute('READ')).toBe(400.00);
            
            consoleSpy.mockRestore();
        });
    });
    
    // ========================================================================
    // SECTION 8: Data Persistence and Integrity
    // ========================================================================
    
    describe('Section 8: Data Persistence and Integrity (TC-039 to TC-042)', () => {
        
        test('TC-039: Balance persists across operations', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Credit $200
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('200'));
            await operations.creditAccount();
            expect(dataProgram.execute('READ')).toBe(1200.00);
            
            // Debit $100
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('100'));
            await operations.debitAccount();
            expect(dataProgram.execute('READ')).toBe(1100.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-040: Balance accuracy with many operations', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            let expectedBalance = 1000.00;
            
            // Perform 10 random operations
            const transactions = [
                { type: 'credit', amount: 100 },
                { type: 'debit', amount: 50 },
                { type: 'credit', amount: 200 },
                { type: 'debit', amount: 75 },
                { type: 'credit', amount: 150 },
                { type: 'debit', amount: 100 },
                { type: 'credit', amount: 300 },
                { type: 'debit', amount: 125 },
                { type: 'credit', amount: 250 },
                { type: 'debit', amount: 200 }
            ];
            
            for (const tx of transactions) {
                mockRl.question.mockImplementationOnce((prompt, callback) => callback(tx.amount.toString()));
                
                if (tx.type === 'credit') {
                    await operations.creditAccount();
                    expectedBalance += tx.amount;
                } else {
                    await operations.debitAccount();
                    expectedBalance -= tx.amount;
                }
            }
            
            // Manual calculation: 1000 + 100 - 50 + 200 - 75 + 150 - 100 + 300 - 125 + 250 - 200 = 1450
            expect(dataProgram.execute('READ')).toBe(1450.00);
            expect(dataProgram.execute('READ')).toBe(expectedBalance);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-041: Application restart resets balance', () => {
            // First instance
            const dataProgram1 = new DataProgram();
            dataProgram1.execute('WRITE', 500.00);
            expect(dataProgram1.execute('READ')).toBe(500.00);
            
            // New instance (simulates restart)
            const dataProgram2 = new DataProgram();
            expect(dataProgram2.execute('READ')).toBe(1000.00); // Reset to initial
        });
        
        test('TC-042: Decimal precision maintained', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Credit $0.99
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('0.99'));
            await operations.creditAccount();
            expect(dataProgram.execute('READ')).toBe(1000.99);
            
            // Debit $0.50
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('0.50'));
            await operations.debitAccount();
            expect(dataProgram.execute('READ')).toBe(1000.49);
            
            consoleSpy.mockRestore();
        });
    });
    
    // ========================================================================
    // SECTION 9: Boundary and Edge Cases
    // ========================================================================
    
    describe('Section 9: Boundary and Edge Cases (TC-043 to TC-048)', () => {
        
        test('TC-043: Maximum balance boundary', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 999999.98);
            
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('0.01'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(dataProgram.execute('READ')).toBe(999999.99);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-044: Minimum balance boundary', async () => {
            const dataProgram = new DataProgram();
            dataProgram.execute('WRITE', 0.01);
            
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('0.01'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.debitAccount();
            
            expect(dataProgram.execute('READ')).toBe(0.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-045: Maximum single transaction', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('998999.99'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(dataProgram.execute('READ')).toBe(999999.99);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-046: Very small transaction', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            // Credit $0.01
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('0.01'));
            await operations.creditAccount();
            
            // Debit $0.01
            mockRl.question.mockImplementationOnce((prompt, callback) => callback('0.01'));
            await operations.debitAccount();
            
            expect(dataProgram.execute('READ')).toBe(1000.00);
            
            consoleSpy.mockRestore();
        });
        
        test('TC-047: Negative amount input', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('-100'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a positive number.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            
            consoleSpy.mockRestore();
        });
        
        test('TC-048: Non-numeric amount input', async () => {
            const dataProgram = new DataProgram();
            const mockRl = createMockReadline();
            mockRl.question.mockImplementation((prompt, callback) => callback('ABC'));
            
            const operations = new Operations(dataProgram, mockRl);
            const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
            
            await operations.creditAccount();
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a positive number.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            
            consoleSpy.mockRestore();
        });
    });
    
    // ========================================================================
    // ADDITIONAL TESTS: DataProgram Class
    // ========================================================================
    
    describe('DataProgram: READ and WRITE Operations', () => {
        
        test('DataProgram: READ operation returns current balance', () => {
            const dataProgram = new DataProgram();
            const balance = dataProgram.execute('READ');
            
            expect(balance).toBe(1000.00);
        });
        
        test('DataProgram: WRITE operation updates balance', () => {
            const dataProgram = new DataProgram();
            
            dataProgram.execute('WRITE', 5000.00);
            const balance = dataProgram.execute('READ');
            
            expect(balance).toBe(5000.00);
        });
        
        test('DataProgram: Multiple WRITE operations', () => {
            const dataProgram = new DataProgram();
            
            dataProgram.execute('WRITE', 1500.00);
            expect(dataProgram.execute('READ')).toBe(1500.00);
            
            dataProgram.execute('WRITE', 2500.00);
            expect(dataProgram.execute('READ')).toBe(2500.00);
            
            dataProgram.execute('WRITE', 500.00);
            expect(dataProgram.execute('READ')).toBe(500.00);
        });
    });
});
