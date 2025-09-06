      *================================================================*
      * CodeUChain COBOL Implementation - Link Interface              *
      *                                                                *
      * Simple link interface for COBOL implementation.               *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINK-INTERFACE.
       AUTHOR. CodeUChain Team.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

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

           DISPLAY "LINK-INTERFACE: Process operation called for: "
                   LS-LINK-NAME-DATA(1:LS-LINK-NAME-LEN)
           DISPLAY "Input Context: " LS-INPUT-CONTEXT

           MOVE "SUCCESS" TO LS-LINK-RESULT
           MOVE LS-INPUT-CONTEXT TO LS-OUTPUT-CONTEXT
           GOBACK.

       END PROGRAM LINK-INTERFACE.