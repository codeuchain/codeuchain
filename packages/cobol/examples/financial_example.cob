      *================================================================*
      * CodeUChain COBOL Example - Financial Calculator               *
      *                                                                *
      * Demonstrates COBOL's strength in financial calculations.      *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINANCIAL-EXAMPLE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-FINANCIAL-DATA        PIC X(10000).
       01  WS-RESULT                PIC X(10000).
       01  WS-LINK-RESULT           PIC X(10).
       01  WS-LINK-NAME.
           05  WS-LINK-NAME-LEN     PIC S9(4) COMP.
           05  WS-LINK-NAME-DATA    PIC X(30).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "CodeUChain COBOL - Financial Calculator"
           DISPLAY "=========================================="

           STRING
               "Principal: $10,000.00, "
               "Rate: 5.0%, "
               "Time: 5 years, "
               "Compounding: Monthly"
               DELIMITED BY SIZE
               INTO WS-FINANCIAL-DATA
           END-STRING

           DISPLAY "Financial parameters: " WS-FINANCIAL-DATA

      *     Set up link name for financial calculation
           MOVE 23 TO WS-LINK-NAME-LEN
           MOVE "COMPOUND-INTEREST-CALC" TO WS-LINK-NAME-DATA

           DISPLAY "Calculating compound interest..."
           CALL "FINANCIAL-CALCULATOR" USING
               WS-LINK-NAME,
               WS-FINANCIAL-DATA,
               WS-RESULT,
               WS-LINK-RESULT

           IF WS-LINK-RESULT = "SUCCESS"
               DISPLAY "Financial calculation completed"
               DISPLAY "Calculation result: " WS-RESULT

               DISPLAY "=========================================="
               DISPLAY "Calculation Summary:"
               DISPLAY "- Principal Amount: $10,000.00"
               DISPLAY "- Annual Interest Rate: 5.0%"
               DISPLAY "- Time Period: 5 years"
               DISPLAY "- Compounding Frequency: Monthly"
               DISPLAY "- Future Value: Calculated"
               DISPLAY "=========================================="
           ELSE
               DISPLAY "Financial calculation failed"
           END-IF

           DISPLAY "=========================================="
           DISPLAY "Financial example completed!"
           DISPLAY "=========================================="

           STOP RUN.

       END PROGRAM FINANCIAL-EXAMPLE.