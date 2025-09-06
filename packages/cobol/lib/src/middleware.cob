      *================================================================*
      * CodeUChain COBOL Implementation - Middleware Interface        *
      *                                                                *
      * Generic middleware interface for COBOL implementation.        *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDDLEWARE-INTERFACE.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01  LS-MIDDLEWARE-NAME.
           05  LS-MIDDLEWARE-NAME-LEN    PIC S9(4) COMP.
           05  LS-MIDDLEWARE-NAME-DATA   PIC X(30).
       01  LS-CONTEXT-DATA          PIC X(10000).
       01  LS-OPERATION             PIC X(20).
       01  LS-RESULT                PIC X(10).

       PROCEDURE DIVISION USING LS-MIDDLEWARE-NAME,
                               LS-CONTEXT-DATA,
                               LS-OPERATION,
                               LS-RESULT.

           DISPLAY "MIDDLEWARE-INTERFACE: Operation called"
           DISPLAY "Operation: " LS-OPERATION

           MOVE 20 TO LS-MIDDLEWARE-NAME-LEN
           MOVE "MIDDLEWARE-INTERFACE" TO LS-MIDDLEWARE-NAME-DATA
           MOVE "SUCCESS" TO LS-RESULT
           GOBACK.

       END PROGRAM MIDDLEWARE-INTERFACE.