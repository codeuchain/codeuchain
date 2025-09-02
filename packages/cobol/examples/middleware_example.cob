      *================================================================*
      * CodeUChain COBOL Example - Middleware Demonstration          *
      *                                                                *
      * Demonstrates middleware functionality with logging.           *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDDLEWARE-EXAMPLE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CONTEXT-DATA          PIC X(10000).
       01  WS-MIDDLEWARE-NAME.
           05  WS-MIDDLEWARE-NAME-LEN    PIC S9(4) COMP.
           05  WS-MIDDLEWARE-NAME-DATA   PIC X(30).
       01  WS-OPERATION.
           05  WS-OPERATION-LEN          PIC S9(4) COMP.
           05  WS-OPERATION-DATA         PIC X(20).
       01  WS-RESULT                PIC X(10).
       01  WS-DISPLAY-NAME          PIC X(30).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "CodeUChain COBOL - Middleware Example"
           DISPLAY "=========================================="

           MOVE "Sample data" TO WS-CONTEXT-DATA
           DISPLAY "Context data: " WS-CONTEXT-DATA

           DISPLAY "Getting middleware name..."
           MOVE 8 TO WS-OPERATION-LEN
           MOVE "GET-NAME" TO WS-OPERATION-DATA

           CALL "LOGGING-MIDDLEWARE" USING
               WS-MIDDLEWARE-NAME,
               WS-CONTEXT-DATA,
               WS-OPERATION,
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               DISPLAY "Middleware name: " WS-MIDDLEWARE-NAME-DATA
           ELSE
               DISPLAY "Failed to get middleware name"
           END-IF

           DISPLAY "Executing 'before' middleware operation..."
           MOVE 6 TO WS-OPERATION-LEN
           MOVE "BEFORE" TO WS-OPERATION-DATA

           CALL "LOGGING-MIDDLEWARE" USING
               WS-MIDDLEWARE-NAME,
               WS-CONTEXT-DATA,
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

           DISPLAY "Executing 'after' middleware operation..."
           MOVE 5 TO WS-OPERATION-LEN
           MOVE "AFTER" TO WS-OPERATION-DATA

           CALL "LOGGING-MIDDLEWARE" USING
               WS-MIDDLEWARE-NAME,
               WS-CONTEXT-DATA,
               WS-OPERATION,
               WS-RESULT

           IF WS-RESULT = "SUCCESS"
               DISPLAY "After operation completed"
               DISPLAY "Check codeuchain.log for details"
           ELSE
               DISPLAY "After operation failed"
           END-IF

           DISPLAY "=========================================="
           DISPLAY "Middleware example completed!"
           DISPLAY "Note: Check codeuchain.log for audit trail"
           DISPLAY "=========================================="

           STOP RUN.

       END PROGRAM MIDDLEWARE-EXAMPLE.