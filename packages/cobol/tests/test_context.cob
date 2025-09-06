       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CONTEXT.
       AUTHOR. CodeUChain Test Suite.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEST-RESULTS.
           05  TESTS-RUN             PIC 9(3) VALUE 0.
           05  TESTS-PASSED          PIC 9(3) VALUE 0.
           05  TESTS-FAILED          PIC 9(3) VALUE 0.

       01  WS-KEY                    PIC X(50).
       01  WS-VALUE                  PIC X(1000).
       01  WS-RESULT                 PIC X(10).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "CodeUChain COBOL - Context Module Tests"
           DISPLAY "=========================================="

           PERFORM TEST-INITIALIZE-CONTEXT
           PERFORM TEST-SET-CONTEXT-VALUE
           PERFORM TEST-GET-CONTEXT-VALUE
           PERFORM TEST-CONTEXT-PERSISTENCE

           PERFORM DISPLAY-TEST-RESULTS

           STOP RUN.

       TEST-INITIALIZE-CONTEXT.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Initialize Context"

           MOVE "INSERT" TO WS-KEY
           MOVE "init-value" TO WS-VALUE

           CALL "CONTEXT" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Context init successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Context init failed"
               DISPLAY "Result: " WS-RESULT
           END-IF.

       TEST-SET-CONTEXT-VALUE.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Set Context Value"

           MOVE "INSERT test-key" TO WS-KEY
           MOVE "test-value" TO WS-VALUE

           CALL "CONTEXT" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Set context value successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Set context value failed"
               DISPLAY "Result: " WS-RESULT
           END-IF.

       TEST-GET-CONTEXT-VALUE.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Get Context Value"

           MOVE "GET test-key" TO WS-KEY
           MOVE SPACES TO WS-VALUE

           CALL "CONTEXT" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS" AND WS-VALUE = "test-value"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Get context value successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Get context value failed"
           END-IF.

       TEST-CONTEXT-PERSISTENCE.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Context Persistence"

           MOVE "INSERT persistent-key" TO WS-KEY
           MOVE "persistent-value" TO WS-VALUE
           CALL "CONTEXT" USING WS-KEY WS-VALUE WS-RESULT

           MOVE "GET persistent-key" TO WS-KEY
           MOVE SPACES TO WS-VALUE
           CALL "CONTEXT" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS" AND WS-VALUE = "persistent-value"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Context persistence successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Context persistence failed"
           END-IF.

       DISPLAY-TEST-RESULTS.
           DISPLAY "=========================================="
           DISPLAY "Test Results Summary:"
           DISPLAY "Total Tests Run: " TESTS-RUN
           DISPLAY "Tests Passed: " TESTS-PASSED
           DISPLAY "Tests Failed: " TESTS-FAILED
           DISPLAY "=========================================="

           IF TESTS-FAILED = 0
               DISPLAY "🎉 All tests passed!"
           ELSE
               DISPLAY "❌ Some tests failed. Please review."
           END-IF.

       END PROGRAM TEST-CONTEXT.