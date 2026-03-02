       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-STATE.
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
           DISPLAY "CodeUChain COBOL - State Module Tests"
           DISPLAY "=========================================="

           PERFORM TEST-INITIALIZE-STATE
           PERFORM TEST-SET-STATE-VALUE
           PERFORM TEST-GET-STATE-VALUE
           PERFORM TEST-STATE-PERSISTENCE

           PERFORM DISPLAY-TEST-RESULTS

           STOP RUN.

       TEST-INITIALIZE-STATE.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Initialize State"

           MOVE "INSERT" TO WS-KEY
           MOVE "init-value" TO WS-VALUE

           CALL "STATE" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ State init successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ State init failed"
               DISPLAY "Result: " WS-RESULT
           END-IF.

       TEST-SET-STATE-VALUE.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Set State Value"

           MOVE "INSERT test-key" TO WS-KEY
           MOVE "test-value" TO WS-VALUE

           CALL "STATE" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Set state value successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Set state value failed"
               DISPLAY "Result: " WS-RESULT
           END-IF.

       TEST-GET-STATE-VALUE.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: Get State Value"

           MOVE "GET test-key" TO WS-KEY
           MOVE SPACES TO WS-VALUE

           CALL "STATE" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS" AND WS-VALUE = "test-value"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ Get state value successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ Get state value failed"
           END-IF.

       TEST-STATE-PERSISTENCE.
           ADD 1 TO TESTS-RUN
           DISPLAY "Test: State Persistence"

           MOVE "INSERT persistent-key" TO WS-KEY
           MOVE "persistent-value" TO WS-VALUE
           CALL "STATE" USING WS-KEY WS-VALUE WS-RESULT

           MOVE "GET persistent-key" TO WS-KEY
           MOVE SPACES TO WS-VALUE
           CALL "STATE" USING WS-KEY WS-VALUE WS-RESULT

           IF WS-RESULT = "SUCCESS" AND WS-VALUE = "persistent-value"
               ADD 1 TO TESTS-PASSED
               DISPLAY "✓ State persistence successful"
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY "✗ State persistence failed"
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

       END PROGRAM TEST-STATE.