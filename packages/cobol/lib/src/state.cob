      *================================================================*
      * COBOL Implementation - State Module                        *
      *                                                                *
      * Simple file-based state storage for COBOL implementation.   *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATE.
       AUTHOR. CodeUChain Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STATE-FILE ASSIGN TO "state.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  STATE-FILE.
       01  STATE-RECORD.
           05  STATE-KEY           PIC X(50).
           05  STATE-VALUE         PIC X(1000).

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
           OPEN EXTEND STATE-FILE
           IF FILE-STATUS = "00"
               MOVE WS-ACTUAL-KEY TO STATE-KEY
               MOVE LS-VALUE TO STATE-VALUE
               WRITE STATE-RECORD
               MOVE "SUCCESS" TO LS-RESULT
               DISPLAY "STATE: Record inserted"
           ELSE
               CLOSE STATE-FILE
               OPEN OUTPUT STATE-FILE
               IF FILE-STATUS = "00"
                   MOVE WS-ACTUAL-KEY TO STATE-KEY
                   MOVE LS-VALUE TO STATE-VALUE
                   WRITE STATE-RECORD
                   MOVE "SUCCESS" TO LS-RESULT
                   DISPLAY "STATE: Record inserted"
               ELSE
                   MOVE "ERROR" TO LS-RESULT
                   DISPLAY "STATE: Failed to create file"
               END-IF
           END-IF
           CLOSE STATE-FILE.

       GET-OPERATION.
           OPEN INPUT STATE-FILE
           IF FILE-STATUS = "00"
               MOVE "NOTFOUND" TO LS-RESULT
               MOVE SPACES TO LS-VALUE
               PERFORM UNTIL FILE-STATUS NOT = "00"
                   READ STATE-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF STATE-KEY = WS-ACTUAL-KEY
                           MOVE STATE-VALUE TO LS-VALUE
                           MOVE "SUCCESS" TO LS-RESULT
                           EXIT PERFORM
                       END-IF
                   END-READ
               END-PERFORM
           ELSE
               MOVE "NOFILE" TO LS-RESULT
               MOVE SPACES TO LS-VALUE
           END-IF
           CLOSE STATE-FILE.

       END PROGRAM STATE.