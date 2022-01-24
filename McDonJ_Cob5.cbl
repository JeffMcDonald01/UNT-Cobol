       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM5.
       AUTHOR. MCDONALD.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INV-INPUT-FILE ASSIGN TO 'INVENT5.DAT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INV-OUTPUT-FILE ASSIGN TO 'INV5OUT.DOC'
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  INV-INPUT-FILE RECORDING MODE IS F.
       01                              PIC X(80).

       FD  INV-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-A-SINGLE-LINE         PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORKING-VARIABLES.
           05  EOF-INV-WS              PIC X(3)        VALUE 'NO'.
           05  TOTAL-RECORDS-WS        PIC 999         VALUE ZERO.
           05  TOTAL-DOLLARS-WS        PIC S9(8)V99    VALUE ZERO.

       01  INV-INPUT-RECORD.
           05  REC-TYPE-IN             PIC X.
           05  BATCH-NO-IN             PIC X(2).
           05  SUPPLIER-NO-IN          PIC X(5).
           05  VOUCHER-NO-IN           PIC X(6).
           05  INVOICE-NO-IN           PIC X(8).
           05  ACCT-NO-IN              PIC X(4).
           05  STORE-NO-IN             PIC X(3).
           05  DATE-IN                 PIC X(8).
           05  FILLER                  PIC X(12).
           05  AMOUNT-IN               PIC S9(6)V99.
           05  SUPPLIER-IN             PIC X(23).

       01  DETAILED-OUTPUT-LINE-SETUP.
           05  REC-TYPE-OUT            PIC X(1).
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  DATE-OUT                PIC XX/XX/XXXX.
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  AMOUNT-OUT              PIC $$$$,$$9.99BCR.
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  ACCT-NO-OUT             PIC X(4).
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  INVOICE-NO-OUT          PIC X(8).
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  BATCH-NO-OUT            PIC X(2).
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  VOUCHER-NO-OUT          PIC X(6).
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  STORE-NO-OUT            PIC X(3).
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  SUPPLIER-NO-OUT         PIC X(5).
           05  FILLER                  PIC X(3)   VALUE SPACE.
           05  SUPPLIER-OUT            PIC X(23).

       01  TOTAL-RECORDS-LINE-SETUP.
           05  FILLER                  PIC X(6)  VALUE SPACE.
           05                          PIC X(38)   VALUE
           'NUMBER OF RECORDS PROCESSED IS:'.
           05  TOTAL-RECORDS-OUT       PIC ZZZ9.

       01  TOTAL-DOLLARS-LINE-SETUP.
           05  FILLER                  PIC X(6)  VALUE SPACE.
           05                          PIC X(35)   VALUE
           'TOTAL NET DOLLAR AMOUNT IS:'.
           05  TOTAL-DOLLARS-OUT       PIC $$$$,$$$,$$9.99BCR.

       PROCEDURE DIVISION.
       100-MAINLINE.
           PERFORM 200-OPEN
           PERFORM 300-PROCESS UNTIL EOF-INV-WS = 'YES'
           PERFORM 900-CLOSE
           STOP RUN.

       200-OPEN.
           OPEN INPUT INV-INPUT-FILE OUTPUT INV-OUTPUT-FILE
           PERFORM 250-READ-ONE-RECORD.

       250-READ-ONE-RECORD.
           READ INV-INPUT-FILE INTO INV-INPUT-RECORD
               AT END MOVE 'YES' TO EOF-INV-WS
           END-READ.

       300-PROCESS.
           MOVE  REC-TYPE-IN       TO      REC-TYPE-OUT
           MOVE  BATCH-NO-IN       TO      BATCH-NO-OUT
           MOVE  SUPPLIER-NO-IN    TO      SUPPLIER-NO-OUT
           MOVE  VOUCHER-NO-IN     TO      VOUCHER-NO-OUT
           MOVE  INVOICE-NO-IN     TO      INVOICE-NO-OUT
           MOVE  ACCT-NO-IN        TO      ACCT-NO-OUT
           MOVE  STORE-NO-IN       TO      STORE-NO-OUT
           MOVE  DATE-IN           TO      DATE-OUT
           MOVE  AMOUNT-IN         TO      AMOUNT-OUT
           MOVE  SUPPLIER-IN       TO      SUPPLIER-OUT

           ADD 1                   TO      TOTAL-RECORDS-WS
           ADD AMOUNT-IN           TO      TOTAL-DOLLARS-WS

           MOVE DETAILED-OUTPUT-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINES

           PERFORM 250-READ-ONE-RECORD.

       900-CLOSE.
           MOVE TOTAL-RECORDS-WS TO TOTAL-RECORDS-OUT
           MOVE TOTAL-DOLLARS-WS TO TOTAL-DOLLARS-OUT

           MOVE TOTAL-RECORDS-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 3 LINES

           MOVE TOTAL-DOLLARS-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 2 LINES

           CLOSE INV-INPUT-FILE INV-OUTPUT-FILE.


