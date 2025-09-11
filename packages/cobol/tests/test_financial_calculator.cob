       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-FINANCIAL-CALCULATOR.
       AUTHOR. CodeUChain Test Suite.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEST-RESULTS.
           05  TESTS-RUN             PIC 9(3) VALUE 0.
           05  TESTS-PASSED          PIC 9(3) VALUE 0.
           05  TESTS-FAILED          PIC 9(3) VALUE 0.

       01  WS-LINK-NAME.
           05  WS-LINK-NAME-LEN     PIC S9(4) COMP.
           05  WS-LINK-NAME-DATA    PIC X(30).
       01  WS-INPUT-CONTEXT         PIC X(10000).
       01  WS-OUTPUT-CONTEXT        PIC X(10000).
       01  WS-LINK-RESULT           PIC X(10).

       PROCEDURE DIVISION.

           DISPLAY "CodeUChain COBOL - Financial Calculator Tests"
           DISPLAY "============================================="

           PERFORM TEST-FINANCIAL-BASIC

           PERFORM DISPLAY-TEST-RESULTS

           STOP RUN.

       TEST-FINANCIAL-BASIC.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Financial Calculator Basic Functionality"

           MOVE 21 TO WS-LINK-NAME-LEN
           MOVE "COMPOUND-INTEREST-CALC" TO WS-LINK-NAME-DATA
           MOVE "Principal: $1000, Rate: 5%, Time: 2" 
               TO WS-INPUT-CONTEXT
           MOVE SPACES TO WS-OUTPUT-CONTEXT

           CALL "FINANCIAL-CALCULATOR" USING
               WS-LINK-NAME
               WS-INPUT-CONTEXT
               WS-OUTPUT-CONTEXT
               WS-LINK-RESULT

           IF WS-LINK-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "PASS: Financial calculator basic functionality"
               DISPLAY "Result: " WS-OUTPUT-CONTEXT
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "FAIL: Financial calculator basic functionality"
           END-IF.

       DISPLAY-TEST-RESULTS.
           DISPLAY "============================================="
           DISPLAY "Test Results:"
           DISPLAY "Total Tests Run: " TESTS-RUN
           DISPLAY "Tests Passed: " TESTS-PASSED
           DISPLAY "Tests Failed: " TESTS-FAILED
           DISPLAY "============================================="

           IF TESTS-FAILED = 0
               DISPLAY "All tests passed!"
           ELSE
               DISPLAY "Some tests failed."
           END-IF.

       END PROGRAM TEST-FINANCIAL-CALCULATOR.