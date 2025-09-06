      *================================================================*
      * COBOL Implementation - Context Module                        *
      *                                                                *
      * Simple file-based context storage for COBOL implementation.   *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTEXT.
       AUTHOR. CodeUChain Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTEXT-FILE ASSIGN TO "context.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTEXT-FILE.
       01  CONTEXT-RECORD.
           05  CONTEXT-KEY           PIC X(50).
           05  CONTEXT-VALUE         PIC X(1000).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS               PIC XX.
       01  WS-OPERATION              PIC X(10).
       01  WS-ACTUAL-KEY            PIC X(50).

       LINKAGE SECTION.
       01  LS-KEY                    PIC X(50).
       01  LS-VALUE                  PIC X(1000).
       01  LS-RESULT                 PIC X(10).

       PROCEDURE DIVISION USING LS-KEY, LS-VALUE, LS-RESULT.

           UNSTRING LS-KEY DELIMITED BY SPACE
               INTO WS-OPERATION WS-ACTUAL-KEY
           END-UNSTRING

           EVALUATE WS-OPERATION
               WHEN "INSERT"
                   PERFORM INSERT-OPERATION
               WHEN "GET"
                   PERFORM GET-OPERATION
               WHEN OTHER
                   MOVE "INVALID" TO LS-RESULT
           END-EVALUATE.

           GOBACK.

       INSERT-OPERATION.
           OPEN EXTEND CONTEXT-FILE
           IF FILE-STATUS = "00"
               MOVE WS-ACTUAL-KEY TO CONTEXT-KEY
               MOVE LS-VALUE TO CONTEXT-VALUE
               WRITE CONTEXT-RECORD
               MOVE "SUCCESS" TO LS-RESULT
               DISPLAY "CONTEXT: Record inserted"
           ELSE
               CLOSE CONTEXT-FILE
               OPEN OUTPUT CONTEXT-FILE
               IF FILE-STATUS = "00"
                   MOVE WS-ACTUAL-KEY TO CONTEXT-KEY
                   MOVE LS-VALUE TO CONTEXT-VALUE
                   WRITE CONTEXT-RECORD
                   MOVE "SUCCESS" TO LS-RESULT
                   DISPLAY "CONTEXT: Record inserted"
               ELSE
                   MOVE "ERROR" TO LS-RESULT
                   DISPLAY "CONTEXT: Failed to create file"
               END-IF
           END-IF
           CLOSE CONTEXT-FILE.

       GET-OPERATION.
           OPEN INPUT CONTEXT-FILE
           IF FILE-STATUS = "00"
               MOVE "NOTFOUND" TO LS-RESULT
               MOVE SPACES TO LS-VALUE
               PERFORM UNTIL FILE-STATUS NOT = "00"
                   READ CONTEXT-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF CONTEXT-KEY = WS-ACTUAL-KEY
                           MOVE CONTEXT-VALUE TO LS-VALUE
                           MOVE "SUCCESS" TO LS-RESULT
                           EXIT PERFORM
                       END-IF
                   END-READ
               END-PERFORM
           ELSE
               MOVE "NOFILE" TO LS-RESULT
               MOVE SPACES TO LS-VALUE
           END-IF
           CLOSE CONTEXT-FILE.

       END PROGRAM CONTEXT.