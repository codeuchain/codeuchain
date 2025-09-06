      *================================================================*
      * CodeUChain COBOL Example - Simple Chain                       *
      *                                                                *
      * Demonstrates basic chain execution with context passing.      *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CHAIN-EXAMPLE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CONTEXT-DATA          PIC X(10000).
       01  WS-RESULT                PIC X(10000).
       01  WS-LINK-RESULT           PIC X(10).
       01  WS-CHAIN-RESULT          PIC X(10).
       01  WS-LINK-NAME.
           05  WS-LINK-NAME-LEN     PIC S9(4) COMP.
           05  WS-LINK-NAME-DATA    PIC X(30).
       01  WS-CHAIN-NAME.
           05  WS-CHAIN-NAME-LEN    PIC S9(4) COMP.
           05  WS-CHAIN-NAME-DATA   PIC X(30).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "CodeUChain COBOL - Simple Chain Example"
           DISPLAY "=========================================="

           MOVE "Hello from COBOL Chain Example!" TO WS-CONTEXT-DATA
           DISPLAY "Initial context: " WS-CONTEXT-DATA

      *     Set up link name structure
           MOVE 11 TO WS-LINK-NAME-LEN
           MOVE "SIMPLE-LINK" TO WS-LINK-NAME-DATA

      *     Set up chain name structure
           MOVE 12 TO WS-CHAIN-NAME-LEN
           MOVE "SIMPLE-CHAIN" TO WS-CHAIN-NAME-DATA

           DISPLAY "Executing link processing..."
           CALL "LINK-INTERFACE" USING
               WS-LINK-NAME,
               WS-CONTEXT-DATA,
               WS-RESULT,
               WS-LINK-RESULT

           IF WS-LINK-RESULT = "SUCCESS"
               DISPLAY "Link execution successful"
               DISPLAY "Result: " WS-RESULT
           ELSE
               DISPLAY "Link execution failed"
           END-IF

           DISPLAY "Executing chain orchestration..."
           CALL "CHAIN-ORCHESTRATOR" USING
               WS-CHAIN-NAME,
               WS-CONTEXT-DATA,
               WS-RESULT,
               WS-CHAIN-RESULT

           IF WS-CHAIN-RESULT = "SUCCESS"
               DISPLAY "Chain execution successful"
               DISPLAY "Final result: " WS-RESULT
           ELSE
               DISPLAY "Chain execution failed"
           END-IF

           DISPLAY "=========================================="
           DISPLAY "Example completed!"
           DISPLAY "=========================================="

           STOP RUN.

       END PROGRAM SIMPLE-CHAIN-EXAMPLE.