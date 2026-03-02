      *================================================================*
      * CodeUChain COBOL Example - Hook Demonstration          *
      *                                                                *
      * Demonstrates hook functionality with logging.           *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOOK-EXAMPLE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-STATE-DATA          PIC X(10000).
       01  WS-HOOK-NAME.
           05  WS-HOOK-NAME-LEN    PIC S9(4) COMP.
           05  WS-HOOK-NAME-DATA   PIC X(30).
       01  WS-OPERATION.
           05  WS-OPERATION-LEN          PIC S9(4) COMP.
           05  WS-OPERATION-DATA         PIC X(20).
       01  WS-RESULT                PIC X(10).
       01  WS-DISPLAY-NAME          PIC X(30).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "CodeUChain COBOL - Hook Example"
           DISPLAY "=========================================="

           MOVE "Sample data" TO WS-STATE-DATA
           DISPLAY "State data: " WS-STATE-DATA

           DISPLAY "Getting hook name..."
           MOVE 8 TO WS-OPERATION-LEN
           MOVE "GET-NAME" TO WS-OPERATION-DATA

           CALL "LOGGING-HOOK" USING
               WS-HOOK-NAME,
               WS-STATE-DATA,
               WS-OPERATION,
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               DISPLAY "Hook name: " WS-HOOK-NAME-DATA
           ELSE
               DISPLAY "Failed to get hook name"
           END-IF

           DISPLAY "Executing 'before' hook operation..."
           MOVE 6 TO WS-OPERATION-LEN
           MOVE "BEFORE" TO WS-OPERATION-DATA

           CALL "LOGGING-HOOK" USING
               WS-HOOK-NAME,
               WS-STATE-DATA,
               WS-OPERATION,
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               DISPLAY "Before operation completed"
               DISPLAY "Check codeuchain.log for details"
           ELSE
               DISPLAY "Before operation failed"
           END-IF

           DISPLAY "Simulating processing operations..."
           DISPLAY "Processing step 1..."
           DISPLAY "Processing step 2..."
           DISPLAY "Processing step 3..."

           DISPLAY "Executing 'after' hook operation..."
           MOVE 5 TO WS-OPERATION-LEN
           MOVE "AFTER" TO WS-OPERATION-DATA

           CALL "LOGGING-HOOK" USING
               WS-HOOK-NAME,
               WS-STATE-DATA,
               WS-OPERATION,
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               DISPLAY "After operation completed"
               DISPLAY "Check codeuchain.log for details"
           ELSE
               DISPLAY "After operation failed"
           END-IF

           DISPLAY "=========================================="
           DISPLAY "Hook example completed!"
           DISPLAY "Note: Check codeuchain.log for audit trail"
           DISPLAY "=========================================="

           STOP RUN.

       END PROGRAM HOOK-EXAMPLE.