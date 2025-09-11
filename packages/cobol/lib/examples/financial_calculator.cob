      *================================================================*
      * CodeUChain COBOL Implementation - Financial Calculator Link   *
      *                                                                *
      * Demonstrates COBOL's strength in financial calculations.      *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINANCIAL-CALCULATOR.
       AUTHOR. CodeUChain Team.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PRINCIPAL-AMOUNT       PIC S9(15)V9(4) COMP-3.
       01  WS-INTEREST-RATE          PIC S9(3)V9(4) COMP-3.
       01  WS-TIME-PERIOD            PIC S9(5)V9(2) COMP-3.
       01  WS-COMPOUND-FREQUENCY     PIC S9(2) COMP-3.
       01  WS-CALCULATED-RESULT      PIC S9(15)V9(4) COMP-3.

       LINKAGE SECTION.
       01  LS-LINK-NAME.
           05  LS-LINK-NAME-LEN     PIC S9(4) COMP.
           05  LS-LINK-NAME-DATA    PIC X(30).
       01  LS-INPUT-CONTEXT         PIC X(10000).
       01  LS-OUTPUT-CONTEXT        PIC X(10000).
       01  LS-LINK-RESULT           PIC X(10).

       PROCEDURE DIVISION USING LS-LINK-NAME,
                                 LS-INPUT-CONTEXT,
                                 LS-OUTPUT-CONTEXT,
                                 LS-LINK-RESULT.

           DISPLAY "FINANCIAL-CALCULATOR: Processing calculation for: "
                   LS-LINK-NAME-DATA(1:LS-LINK-NAME-LEN)

           MOVE 10000.00 TO WS-PRINCIPAL-AMOUNT
           MOVE 0.05 TO WS-INTEREST-RATE
           MOVE 5.00 TO WS-TIME-PERIOD
           MOVE 12 TO WS-COMPOUND-FREQUENCY

           COMPUTE WS-CALCULATED-RESULT = WS-PRINCIPAL-AMOUNT *
               (1 + WS-INTEREST-RATE / WS-COMPOUND-FREQUENCY) **
               (WS-COMPOUND-FREQUENCY * WS-TIME-PERIOD)

           MOVE "Result calculated" TO LS-OUTPUT-CONTEXT
           MOVE "SUCCESS" TO LS-LINK-RESULT
           GOBACK.

       END PROGRAM FINANCIAL-CALCULATOR.