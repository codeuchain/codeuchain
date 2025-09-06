      *================================================================*
      * CodeUChain COBOL Implementation - Logging Middleware          *
      *                                                                *
      * Simple logging middleware for COBOL implementation.           *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGGING-MIDDLEWARE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO "codeuchain.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE.
       01  LOG-RECORD.
           05  LOG-TIMESTAMP         PIC X(20).
           05  LOG-LEVEL             PIC X(10).
           05  LOG-COMPONENT         PIC X(50).
           05  LOG-MESSAGE           PIC X(500).

       WORKING-STORAGE SECTION.
       01  LOG-STATUS                PIC XX.

       01  WS-MIDDLEWARE-NAME        PIC X(50).
       01  WS-MIDDLEWARE-DESCRIPTION PIC X(200).

       01  WS-CURRENT-TIME           PIC X(20).
       01  WS-LOG-LEVEL              PIC X(10).
       01  WS-LOG-COMPONENT          PIC X(50).
       01  WS-LOG-MESSAGE            PIC X(500).

       LINKAGE SECTION.
       01  LS-MIDDLEWARE-NAME.
           05  LS-MIDDLEWARE-NAME-LEN    PIC S9(4) COMP.
           05  LS-MIDDLEWARE-NAME-DATA   PIC X(30).
       01  LS-CONTEXT-DATA          PIC X(10000).
       01  LS-OPERATION.
           05  LS-OPERATION-LEN          PIC S9(4) COMP.
           05  LS-OPERATION-DATA         PIC X(20).
       01  LS-RESULT                PIC X(10).

       PROCEDURE DIVISION USING LS-MIDDLEWARE-NAME,
                               LS-CONTEXT-DATA,
                               LS-OPERATION,
                               LS-RESULT.

           EVALUATE LS-OPERATION-DATA(1:LS-OPERATION-LEN)
               WHEN "GET-NAME"
                   PERFORM GET-NAME-OPERATION
               WHEN "BEFORE"
                   PERFORM BEFORE-OPERATION
               WHEN "AFTER"
                   PERFORM AFTER-OPERATION
               WHEN OTHER
                   MOVE "INVALID" TO LS-RESULT
           END-EVALUATE.

           GOBACK.

       GET-NAME-OPERATION.
           MOVE 19 TO LS-MIDDLEWARE-NAME-LEN
           MOVE "LOGGING-MIDDLEWARE" TO LS-MIDDLEWARE-NAME-DATA
           MOVE "SUCCESS" TO LS-RESULT.

       BEFORE-OPERATION.
           MOVE "INFO" TO WS-LOG-LEVEL
           MOVE "CHAIN" TO WS-LOG-COMPONENT
           MOVE "Chain execution starting" TO WS-LOG-MESSAGE

           PERFORM WRITE-LOG-ENTRY
           MOVE "SUCCESS" TO LS-RESULT.

       AFTER-OPERATION.
           MOVE "INFO" TO WS-LOG-LEVEL
           MOVE "CHAIN" TO WS-LOG-COMPONENT
           MOVE "Chain execution completed" TO WS-LOG-MESSAGE

           PERFORM WRITE-LOG-ENTRY
           MOVE "SUCCESS" TO LS-RESULT.

       WRITE-LOG-ENTRY.
           OPEN EXTEND LOG-FILE

           IF LOG-STATUS = "00" OR LOG-STATUS = "05"
               ACCEPT WS-CURRENT-TIME FROM TIME

               MOVE WS-CURRENT-TIME TO LOG-TIMESTAMP
               MOVE WS-LOG-LEVEL TO LOG-LEVEL
               MOVE WS-LOG-COMPONENT TO LOG-COMPONENT
               MOVE WS-LOG-MESSAGE TO LOG-MESSAGE

               WRITE LOG-RECORD
               DISPLAY "LOGGING: Log entry written successfully"
           ELSE
               DISPLAY "LOGGING: Failed to open log file"
           END-IF

           CLOSE LOG-FILE.

       END PROGRAM LOGGING-MIDDLEWARE.