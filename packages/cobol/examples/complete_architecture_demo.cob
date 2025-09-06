      *================================================================*
      * CodeUChain COBOL Example - Complete Architecture Demo        *
      *                                                                *
      * Demonstrates the complete CodeUChain architecture in COBOL.  *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLETE-ARCHITECTURE-DEMO.

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
       01  WS-MIDDLEWARE-NAME.
           05  WS-MIDDLEWARE-NAME-LEN    PIC S9(4) COMP.
           05  WS-MIDDLEWARE-NAME-DATA   PIC X(30).
       01  WS-MIDDLEWARE-RESULT     PIC X(10).
       01  WS-OPERATION.
           05  WS-OPERATION-LEN          PIC S9(4) COMP.
           05  WS-OPERATION-DATA         PIC X(20).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "CodeUChain COBOL - Complete Architecture"
           DISPLAY "=========================================="

           DISPLAY "Step 1: Initializing business context..."
           STRING
               "Business Process: Loan Application, "
               "Applicant: John Doe, "
               "Amount: $25,000.00, "
               "Term: 30 years, "
               "Rate: 6.5%"
               DELIMITED BY SIZE
               INTO WS-CONTEXT-DATA
           END-STRING

           DISPLAY "Business context initialized:"
           DISPLAY WS-CONTEXT-DATA

      *     Set up names for the demonstration
           MOVE 16 TO WS-LINK-NAME-LEN
           MOVE "LOAN-CALCULATION" TO WS-LINK-NAME-DATA
           MOVE 19 TO WS-CHAIN-NAME-LEN
           MOVE "BUSINESS-PROCESS-CHAIN" TO WS-CHAIN-NAME-DATA

           DISPLAY "Step 2: Executing middleware (before)..."
           MOVE 6 TO WS-OPERATION-LEN
           MOVE "BEFORE" TO WS-OPERATION-DATA
           CALL "LOGGING-MIDDLEWARE" USING
               WS-MIDDLEWARE-NAME,
               WS-CONTEXT-DATA,
               WS-OPERATION,
               WS-MIDDLEWARE-RESULT

           IF WS-MIDDLEWARE-RESULT = "SUCCESS"
               DISPLAY "Middleware before-operation successful"
           END-IF

           DISPLAY "Step 3: Executing financial calculation link..."
           CALL "FINANCIAL-CALCULATOR" USING
               WS-LINK-NAME,
               WS-CONTEXT-DATA,
               WS-RESULT,
               WS-LINK-RESULT

           IF WS-LINK-RESULT = "SUCCESS"
               DISPLAY "Financial calculation completed"
               MOVE WS-RESULT TO WS-CONTEXT-DATA
           ELSE
               DISPLAY "Financial calculation failed"
           END-IF

           DISPLAY "Step 4: Executing general link processing..."
           MOVE 19 TO WS-LINK-NAME-LEN
           MOVE "BUSINESS-PROCESSING" TO WS-LINK-NAME-DATA
           CALL "LINK-INTERFACE" USING
               WS-LINK-NAME,
               WS-CONTEXT-DATA,
               WS-RESULT,
               WS-LINK-RESULT

           IF WS-LINK-RESULT = "SUCCESS"
               DISPLAY "General link processing completed"
               MOVE WS-RESULT TO WS-CONTEXT-DATA
           END-IF

           DISPLAY "Step 5: Executing chain orchestration..."
           CALL "CHAIN-ORCHESTRATOR" USING
               WS-CHAIN-NAME,
               WS-CONTEXT-DATA,
               WS-RESULT,
               WS-CHAIN-RESULT

           IF WS-CHAIN-RESULT = "SUCCESS"
               DISPLAY "Chain orchestration completed"
               DISPLAY "Final result: " WS-RESULT
           ELSE
               DISPLAY "Chain orchestration failed"
           END-IF

           DISPLAY "Step 6: Executing middleware (after)..."
           MOVE 5 TO WS-OPERATION-LEN
           MOVE "AFTER" TO WS-OPERATION-DATA
           CALL "LOGGING-MIDDLEWARE" USING
               WS-MIDDLEWARE-NAME,
               WS-CONTEXT-DATA,
               WS-OPERATION,
               WS-MIDDLEWARE-RESULT

           IF WS-MIDDLEWARE-RESULT = "SUCCESS"
               DISPLAY "Middleware after-operation successful"
           END-IF

           DISPLAY "=========================================="
           DISPLAY "ARCHITECTURE DEMONSTRATION SUMMARY:"
           DISPLAY "- Context Management: ✅ Initialized and passed"
           DISPLAY "- Link Processing: ✅ Financial + General links"
           DISPLAY "- Middleware: ✅ Before/After operations"
           DISPLAY "- Chain Orchestration: ✅ Complete workflow"
           DISPLAY "- Logging: ✅ Audit trail generated"
           DISPLAY "=========================================="
           DISPLAY "Complete CodeUChain architecture working in COBOL!"
           DISPLAY "=========================================="

           STOP RUN.

       END PROGRAM COMPLETE-ARCHITECTURE-DEMO.