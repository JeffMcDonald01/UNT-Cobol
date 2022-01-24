       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROGRAM3.
       AUTHOR.  MCDONALD.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAYROLL-INPUT-FILE ASSIGN TO 'PAYROLL.DAT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PAYROLL-OUTPUT-FILE ASSIGN TO 'PAYROLL.DOC'
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-INPUT-FILE RECORDING MODE IS F.
       01                              PIC X(80).

       FD  PAYROLL-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-A-SINGLE-LINE         PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORKING-VARIABLES.
           05  PAYCHECK-WS             PIC S9(5)V99    VALUE ZERO.
           05  EOF-PAYROLL-WS          PIC X(3)        VALUE 'NO'.

       01  PAYROLL-INPUT-RECORD.
           05  SSN-IN                  PIC X(9).
           05  RATE-NUM-IN             PIC 99V99.
           05  HOURS-NUM-IN            PIC 99.
           05  NAME-IN                 PIC X(20).

       01  DETAILED-OUTPUT-LINE-SETUP.
           05  FILLER                  PIC X       VALUE SPACE.
           05  NAME-OUT                PIC X(20).
           05  FILLER                  PIC X(1).   VALUE SPACE
           05  SSN-OUT                 PIC XXXXXXXXX.
           05                          PIC X(7).
           05  RATE-NUM-OUT            PIC $$9.99BCR.
           05                          PIC X(5).
           05  HOURS-NUM-OUT           PIC X(2).
           05                          PIC X(6).
           05  PAYCHECK-NUM-OUT        PIC $$,$$9.99BCR.

       PROCEDURE DIVISION.
       100-MAINLINE.
           PERFORM 200-OPEN
           PERFORM 300-PROCESS UNTIL EOF-PAYROLL-WS = 'YES'
           PERFORM 900-CLOSE
           STOP RUN.

       200-OPEN.
           OPEN INPUT PAYROLL-INPUT-FILE OUTPUT PAYROLL-OUTPUT-FILE
           PERFORM 250-READ-ONE-RECORD.

       250-READ-ONE-RECORD.
           READ PAYROLL-INPUT-FILE INTO PAYROLL-INPUT-RECORD
               AT END MOVE 'YES' TO EOF-PAYROLL-WS
           END-READ.

       300-PROCESS.
           MOVE    NAME-IN         TO  NAME-OUT
           MOVE    SSN-IN          TO  SSN-OUT
           MOVE    RATE-NUM-IN     TO  RATE-NUM-OUT
           MOVE    HOURS-NUM-IN    TO  HOURS-NUM-OUT

           COMPUTE PAYCHECK-WS = RATE-NUM-IN * HOURS-NUM-IN
           MOVE    PAYCHECK-WS     TO  PAYCHECK-NUM-OUT

           MOVE DETAILED-OUTPUT-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINE
           PERFORM 250-READ-ONE-RECORD.

       900-CLOSE.
           CLOSE   PAYROLL-INPUT-FILE  PAYROLL-OUTPUT-FILE.












































