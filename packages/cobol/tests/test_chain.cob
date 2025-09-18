       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CHAIN.
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
       01  WS-INITIAL-CONTEXT       PIC X(10000).
       01  WS-FINAL-CONTEXT         PIC X(10000).
       01  WS-RESULT                PIC X(10).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "CodeUChain COBOL - Chain Module Tests"
           DISPLAY "=========================================="

           PERFORM TEST-CHAIN-INITIALIZATION
           PERFORM TEST-CHAIN-EXECUTION-SINGLE-LINK
           PERFORM TEST-CHAIN-EXECUTION-MULTIPLE-LINKS
           PERFORM TEST-CHAIN-ERROR-PROPAGATION
           PERFORM TEST-CHAIN-STATUS-TRACKING

           PERFORM DISPLAY-TEST-RESULTS

           STOP RUN.

       TEST-CHAIN-INITIALIZATION.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Chain Initialization"

           MOVE 12 TO WS-LINK-NAME-LEN
           MOVE "TEST-CHAIN" TO WS-LINK-NAME-DATA
           MOVE "Initial context" TO WS-INITIAL-CONTEXT
           MOVE SPACES TO WS-FINAL-CONTEXT

           CALL "CHAIN-ORCHESTRATOR" USING
               WS-LINK-NAME
               WS-INITIAL-CONTEXT
               WS-FINAL-CONTEXT
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "PASS: Chain initialization successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Chain initialization failed"
           END-IF.

       TEST-CHAIN-EXECUTION-SINGLE-LINK.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Single Link Chain Execution"

           MOVE 15 TO WS-LINK-NAME-LEN
           MOVE "SINGLE-LINK" TO WS-LINK-NAME-DATA
           MOVE "Test input data" TO WS-INITIAL-CONTEXT
           MOVE SPACES TO WS-FINAL-CONTEXT

           CALL "CHAIN-ORCHESTRATOR" USING
               WS-LINK-NAME
               WS-INITIAL-CONTEXT
               WS-FINAL-CONTEXT
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Single link chain execution successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Single link chain execution failed"
           END-IF.

       TEST-CHAIN-EXECUTION-MULTIPLE-LINKS.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Multiple Links Chain Execution"

           MOVE 14 TO WS-LINK-NAME-LEN
           MOVE "MULTI-LINK" TO WS-LINK-NAME-DATA
           MOVE "Multiple link test data" TO WS-INITIAL-CONTEXT
           MOVE SPACES TO WS-FINAL-CONTEXT

           CALL "CHAIN-ORCHESTRATOR" USING
               WS-LINK-NAME
               WS-INITIAL-CONTEXT
               WS-FINAL-CONTEXT
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Multiple links chain execution successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Multiple links chain execution failed"
           END-IF.

       TEST-CHAIN-ERROR-PROPAGATION.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Chain Error Propagation"

           MOVE 11 TO WS-LINK-NAME-LEN
           MOVE "ERROR-CHAIN" TO WS-LINK-NAME-DATA
           MOVE "Error test data" TO WS-INITIAL-CONTEXT
           MOVE SPACES TO WS-FINAL-CONTEXT

           CALL "CHAIN-ORCHESTRATOR" USING
               WS-LINK-NAME
               WS-INITIAL-CONTEXT
               WS-FINAL-CONTEXT
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "PASS: Chain error propagation test"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "FAIL: Chain error propagation"
           END-IF.

       TEST-CHAIN-STATUS-TRACKING.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Chain Status Tracking"

           MOVE 16 TO WS-LINK-NAME-LEN
           MOVE "STATUS-TEST" TO WS-LINK-NAME-DATA
           MOVE "Status tracking test" TO WS-INITIAL-CONTEXT
           MOVE SPACES TO WS-FINAL-CONTEXT

           CALL "CHAIN-ORCHESTRATOR" USING
               WS-LINK-NAME
               WS-INITIAL-CONTEXT
               WS-FINAL-CONTEXT
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Chain status tracking successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Chain status tracking failed"
           END-IF.

       DISPLAY-TEST-RESULTS.
           DISPLAY "=========================================="
           DISPLAY "Test Results Summary:"
           DISPLAY "Total Tests Run: " TESTS-RUN
           DISPLAY "Tests Passed: " TESTS-PASSED
           DISPLAY "Tests Failed: " TESTS-FAILED
           DISPLAY "=========================================="

           IF TESTS-FAILED = 0
               DISPLAY "🎉 All chain tests passed!"
           ELSE
               DISPLAY "❌ Some chain tests failed. Please review."
           END-IF.

       END PROGRAM TEST-CHAIN.