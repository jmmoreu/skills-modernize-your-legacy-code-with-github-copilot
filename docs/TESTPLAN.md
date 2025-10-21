# Test Plan - Student Account Management System

## Document Information

**Application**: Student Account Management System (COBOL Legacy Application)  
**Version**: 1.0  
**Date**: October 21, 2025  
**Purpose**: Validate business logic for migration from COBOL to Node.js  
**Stakeholder Review**: Required before Node.js implementation

---

## Test Objectives

- Validate all account balance operations (View, Credit, Debit)
- Verify business rules enforcement (insufficient funds, balance limits)
- Confirm data persistence during application lifecycle
- Validate user interface and navigation
- Test error handling and edge cases

---

## Test Environment

- **Initial Balance**: $1,000.00
- **Maximum Balance**: $999,999.99
- **Minimum Balance**: $0.00
- **Decimal Precision**: 2 decimal places

---

## Test Cases

### Section 1: Application Initialization and Menu

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-001 | Verify application starts successfully | Application not running | 1. Start application<br>2. Observe initial screen | Application displays menu with 4 options:<br>1. View Balance<br>2. Credit Account<br>3. Debit Account<br>4. Exit | | | |
| TC-002 | Verify initial account balance | Application just started | 1. Start application<br>2. Select option 1 (View Balance) | System displays: "Current balance: 1000.00" | | | Default balance should be $1,000.00 |
| TC-003 | Verify menu displays after each operation | Application running | 1. Perform any operation<br>2. Observe screen | Menu is redisplayed after operation completes | | | Ensures continuous operation |

### Section 2: View Balance Operations

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-004 | View initial balance | Application started, no transactions performed | 1. Select option 1<br>2. Observe displayed balance | System displays: "Current balance: 1000.00" | | | Tests READ operation from DataProgram |
| TC-005 | View balance after credit | Balance credited with $500 | 1. Credit $500<br>2. Select option 1<br>3. Observe balance | System displays: "Current balance: 1500.00" | | | Verifies balance persistence |
| TC-006 | View balance after debit | Balance debited by $300 | 1. Debit $300 from initial balance<br>2. Select option 1<br>3. Observe balance | System displays: "Current balance: 700.00" | | | Verifies debit is persisted |
| TC-007 | View balance after multiple transactions | Multiple credits and debits performed | 1. Perform multiple transactions<br>2. Select option 1<br>3. Verify calculation | Balance reflects all transactions correctly | | | Tests cumulative accuracy |

### Section 3: Credit Account Operations

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-008 | Credit account with valid amount | Initial balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 500.00<br>3. Observe result | System displays: "Amount credited. New balance: 1500.00" | | | Basic credit functionality |
| TC-009 | Credit account with small amount | Initial balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 0.01<br>3. Observe result | System displays: "Amount credited. New balance: 1000.01" | | | Tests minimum credit amount |
| TC-010 | Credit account with large amount | Initial balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 50000.00<br>3. Observe result | System displays: "Amount credited. New balance: 51000.00" | | | Tests large credit amounts |
| TC-011 | Credit account with decimal precision | Initial balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 123.45<br>3. Observe result | System displays: "Amount credited. New balance: 1123.45" | | | Validates decimal handling |
| TC-012 | Credit account multiple times | Initial balance: $1,000.00 | 1. Select option 2, credit $100<br>2. Select option 2, credit $200<br>3. Select option 2, credit $300<br>4. View balance | System displays: "Current balance: 1600.00" | | | Tests cumulative credits |
| TC-013 | Credit account near maximum limit | Balance: $999,000.00 | 1. Select option 2<br>2. Enter amount: 999.99<br>3. Observe result | System displays: "Amount credited. New balance: 999999.99" | | | Tests maximum balance limit |
| TC-014 | Credit account exceeding maximum limit | Balance: $999,000.00 | 1. Select option 2<br>2. Enter amount: 1000.00<br>3. Observe result | System should handle overflow or reject | | | Edge case: exceeds PIC 9(6)V99 |
| TC-015 | Credit with zero amount | Initial balance: $1,000.00 | 1. Select option 2<br>2. Enter amount: 0.00<br>3. Observe result | System displays: "Amount credited. New balance: 1000.00" | | | Tests zero credit |

### Section 4: Debit Account Operations (Sufficient Funds)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-016 | Debit account with valid amount | Initial balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 300.00<br>3. Observe result | System displays: "Amount debited. New balance: 700.00" | | | Basic debit functionality |
| TC-017 | Debit account with small amount | Initial balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 0.01<br>3. Observe result | System displays: "Amount debited. New balance: 999.99" | | | Tests minimum debit amount |
| TC-018 | Debit entire balance | Balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 1000.00<br>3. Observe result | System displays: "Amount debited. New balance: 0.00" | | | Tests balance reaching zero |
| TC-019 | Debit with decimal precision | Balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 234.56<br>3. Observe result | System displays: "Amount debited. New balance: 765.44" | | | Validates decimal handling |
| TC-020 | Debit account multiple times | Initial balance: $1,000.00 | 1. Select option 3, debit $100<br>2. Select option 3, debit $200<br>3. Select option 3, debit $150<br>4. View balance | System displays: "Current balance: 550.00" | | | Tests cumulative debits |
| TC-021 | Debit from low balance (sufficient) | Balance: $100.00 | 1. Select option 3<br>2. Enter amount: 100.00<br>3. Observe result | System displays: "Amount debited. New balance: 0.00" | | | Tests exact balance debit |
| TC-022 | Debit with zero amount | Balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 0.00<br>3. Observe result | System displays: "Amount debited. New balance: 1000.00" | | | Tests zero debit |

### Section 5: Debit Account Operations (Insufficient Funds)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-023 | Debit exceeding current balance | Balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 1500.00<br>3. Observe result | System displays: "Insufficient funds for this debit."<br>Balance remains: $1,000.00 | | | Critical business rule |
| TC-024 | Debit slightly exceeding balance | Balance: $1,000.00 | 1. Select option 3<br>2. Enter amount: 1000.01<br>3. Observe result | System displays: "Insufficient funds for this debit."<br>Balance remains: $1,000.00 | | | Tests precise validation |
| TC-025 | Debit from zero balance | Balance: $0.00 | 1. Select option 3<br>2. Enter amount: 0.01<br>3. Observe result | System displays: "Insufficient funds for this debit."<br>Balance remains: $0.00 | | | Tests minimum balance rule |
| TC-026 | Verify balance unchanged after failed debit | Balance: $500.00 | 1. Attempt to debit $600<br>2. View balance<br>3. Observe result | Failed debit message shown<br>Balance still shows: $500.00 | | | Ensures no partial debit |
| TC-027 | Multiple failed debit attempts | Balance: $100.00 | 1. Attempt to debit $150<br>2. Attempt to debit $200<br>3. View balance | Each attempt shows error message<br>Balance remains: $100.00 | | | Tests repeated failures |

### Section 6: Mixed Transaction Scenarios

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-028 | Credit then debit scenario | Initial balance: $1,000.00 | 1. Credit $500 (balance: $1,500)<br>2. Debit $300 (balance: $1,200)<br>3. View balance | System displays: "Current balance: 1200.00" | | | Common transaction pattern |
| TC-029 | Debit then credit scenario | Initial balance: $1,000.00 | 1. Debit $400 (balance: $600)<br>2. Credit $200 (balance: $800)<br>3. View balance | System displays: "Current balance: 800.00" | | | Reverse transaction order |
| TC-030 | Complex transaction sequence | Initial balance: $1,000.00 | 1. Credit $500<br>2. Debit $300<br>3. Credit $100<br>4. Debit $200<br>5. View balance | System displays: "Current balance: 1100.00"<br>(1000 + 500 - 300 + 100 - 200) | | | Tests calculation accuracy |
| TC-031 | Failed debit followed by successful debit | Balance: $500.00 | 1. Attempt debit $600 (fails)<br>2. Debit $300 (succeeds)<br>3. View balance | First attempt: error message<br>Second attempt: success<br>Final balance: $200.00 | | | Tests recovery from failure |
| TC-032 | Credit after failed debit | Balance: $100.00 | 1. Attempt debit $200 (fails)<br>2. Credit $500<br>3. Debit $200 (succeeds)<br>4. View balance | Failed debit shows error<br>Credit succeeds (balance: $600)<br>Debit succeeds<br>Final balance: $400.00 | | | Tests fund availability logic |

### Section 7: User Interface and Navigation

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-033 | Invalid menu choice - alphabetic | Application running | 1. Enter "A" at menu prompt<br>2. Observe result | System displays: "Invalid choice, please select 1-4."<br>Menu redisplays | | | Tests input validation |
| TC-034 | Invalid menu choice - out of range high | Application running | 1. Enter "5" at menu prompt<br>2. Observe result | System displays: "Invalid choice, please select 1-4."<br>Menu redisplays | | | Tests range validation |
| TC-035 | Invalid menu choice - out of range low | Application running | 1. Enter "0" at menu prompt<br>2. Observe result | System displays: "Invalid choice, please select 1-4."<br>Menu redisplays | | | Tests lower bound |
| TC-036 | Invalid menu choice - special characters | Application running | 1. Enter "#" at menu prompt<br>2. Observe result | System displays: "Invalid choice, please select 1-4."<br>Menu redisplays | | | Tests special character handling |
| TC-037 | Exit application | Application running | 1. Select option 4<br>2. Observe result | System displays: "Exiting the program. Goodbye!"<br>Application terminates | | | Tests clean exit |
| TC-038 | Menu displays correct options | Application running | 1. Observe menu display | Menu shows:<br>- Account Management System title<br>- Options 1-4 clearly labeled<br>- Proper formatting | | | UI validation |

### Section 8: Data Persistence and Integrity

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-039 | Balance persists across operations | Initial balance: $1,000.00 | 1. Credit $200<br>2. View balance (should be $1,200)<br>3. Debit $100<br>4. View balance | Each view balance shows correct cumulative amount<br>Final balance: $1,100.00 | | | Tests STORAGE-BALANCE persistence |
| TC-040 | Balance accuracy with many operations | Initial balance: $1,000.00 | 1. Perform 10 random credits/debits<br>2. Manually calculate expected balance<br>3. View balance | Displayed balance matches manual calculation | | | Tests cumulative accuracy |
| TC-041 | Application restart resets balance | Application has modified balance | 1. Modify balance (e.g., to $500)<br>2. Exit application (option 4)<br>3. Restart application<br>4. View balance | Balance resets to initial: $1,000.00 | | | Confirms no file persistence |
| TC-042 | Decimal precision maintained | Initial balance: $1,000.00 | 1. Credit $0.99<br>2. Debit $0.50<br>3. View balance | Balance displays: $1,000.49<br>No rounding errors | | | Tests precision handling |

### Section 9: Boundary and Edge Cases

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-043 | Maximum balance boundary | Balance: $999,999.98 | 1. Credit $0.01<br>2. View balance | Balance displays: $999,999.99 | | | Tests maximum allowed balance |
| TC-044 | Minimum balance boundary | Balance: $0.01 | 1. Debit $0.01<br>2. View balance | Balance displays: $0.00 | | | Tests minimum allowed balance |
| TC-045 | Maximum single transaction | Initial balance: $1,000.00 | 1. Credit $998,999.99<br>2. View balance | Balance displays: $999,999.99 | | | Tests maximum single credit |
| TC-046 | Very small transaction | Balance: $1,000.00 | 1. Credit $0.01<br>2. Debit $0.01<br>3. View balance | Balance displays: $1,000.00 | | | Tests smallest unit (penny) |
| TC-047 | Negative amount input (if possible) | Balance: $1,000.00 | 1. Attempt to credit -$100<br>2. Observe behavior | System should reject or handle appropriately | | | Tests input validation |
| TC-048 | Non-numeric amount input | Application prompts for amount | 1. Enter "ABC" when prompted for amount<br>2. Observe behavior | System should reject or handle appropriately | | | Tests data type validation |

### Section 10: Program Flow and Control

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-049 | Continuous operation loop | Application running | 1. Perform 5 different operations<br>2. Verify menu redisplays each time | Menu redisplays after each operation<br>Application remains running | | | Tests CONTINUE-FLAG loop |
| TC-050 | Exit terminates program | Application running | 1. Select option 4<br>2. Verify program stops | Program displays goodbye message<br>Execution stops cleanly | | | Tests STOP RUN |
| TC-051 | Operations call correct programs | Application running | 1. Test each menu option<br>2. Verify correct behavior | Each option invokes correct operation:<br>1→TOTAL, 2→CREDIT, 3→DEBIT | | | Tests EVALUATE logic |
| TC-052 | Program handles rapid inputs | Application running | 1. Quickly enter multiple menu choices<br>2. Perform rapid transactions | All operations complete correctly<br>No data corruption | | | Tests system responsiveness |

---

## Test Execution Guidelines

### For Manual Testing

1. **Start Fresh**: Begin each test section with a fresh application start to ensure initial balance of $1,000.00
2. **Record Results**: Document actual results in the "Actual Result" column
3. **Mark Status**: Update "Status" column with Pass/Fail after each test
4. **Add Comments**: Note any deviations, unexpected behavior, or observations
5. **Calculate Expected Values**: Manually verify all balance calculations before marking as Pass

### For Automated Testing (Node.js Implementation)

1. **Unit Tests**: Create unit tests for each business logic function (credit, debit, balance inquiry)
2. **Integration Tests**: Test the interaction between modules (equivalent to CALL statements)
3. **Test Data**: Use the same test amounts and scenarios from this plan
4. **Assertions**: Verify both displayed messages and actual balance values
5. **Mock Data Layer**: Create mocks for the data persistence layer
6. **Edge Cases**: Implement all boundary tests (TC-043 through TC-048)

---

## Success Criteria

- **All test cases pass**: 100% pass rate required for migration approval
- **Business rules validated**: All stakeholders confirm expected behavior
- **Edge cases handled**: No unexpected errors or data corruption
- **Decimal precision**: All calculations accurate to 2 decimal places
- **User experience**: Menu navigation and prompts are clear and functional

---

## Test Execution Summary

**Total Test Cases**: 52  
**Tests Passed**: ___  
**Tests Failed**: ___  
**Pass Rate**: ___%  

**Tested By**: ________________  
**Date**: ________________  
**Stakeholder Approval**: ________________  
**Ready for Node.js Migration**: ☐ Yes  ☐ No  

---

## Notes for Node.js Implementation

### Key Business Rules to Preserve

1. **Initial Balance**: $1,000.00 on application start
2. **Insufficient Funds Check**: Balance >= debit amount (no overdrafts)
3. **Decimal Precision**: Two decimal places for all amounts
4. **Maximum Balance**: $999,999.99
5. **Minimum Balance**: $0.00 (no negative balances)
6. **Transaction Atomicity**: Failed transactions don't modify balance
7. **Menu Loop**: Application continues until explicit exit

### Recommended Test Framework

- **Unit Tests**: Jest or Mocha
- **Integration Tests**: Supertest (if implementing REST API)
- **Test Coverage**: Aim for >90% code coverage
- **Continuous Integration**: Run tests on every commit

### Additional Tests for Node.js

Consider adding these tests that aren't relevant to COBOL but important for Node.js:

- Concurrent transaction handling
- API endpoint security
- Database connection handling
- Error logging and monitoring
- Performance/load testing
- Input sanitization and validation
- Session management (if multi-user)

---

## Change Log

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | October 21, 2025 | System Analyst | Initial test plan created from COBOL analysis |

---

## Appendix: Test Data Reference

### Standard Test Amounts

- **Small Amount**: $0.01
- **Medium Amount**: $500.00
- **Large Amount**: $50,000.00
- **Maximum Safe Credit**: $998,999.99 (to reach max balance from initial)
- **Boundary Credit**: Amounts that reach exactly $999,999.99
- **Insufficient Debit**: Any amount > current balance

### Expected Balance Calculations

| Starting Balance | Transaction | Expected Result |
|------------------|-------------|-----------------|
| $1,000.00 | Credit $500.00 | $1,500.00 |
| $1,000.00 | Debit $300.00 | $700.00 |
| $1,000.00 | Debit $1,500.00 | $1,000.00 (no change - insufficient) |
| $1,500.00 | Credit $100 → Debit $200 | $1,400.00 |
| $0.00 | Any debit | $0.00 (no change - insufficient) |
