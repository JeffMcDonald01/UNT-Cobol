       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM6.
       AUTHOR. MCDONALD.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INV-INPUT-FILE ASSIGN TO 'INVENT6.DAT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INV-OUTPUT-FILE ASSIGN TO 'INV6OUT.DOC'
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  INV-INPUT-FILE RECORDING MODE IS F.
       01                              PIC X(80).

       FD  INV-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-A-SINGLE-LINE         PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORKING-VARIABLES.
           05  EOF-INV-WS                PIC X(3)          VALUE 'NO'.
           05  TOTAL-RECORDS-WS          PIC 999           VALUE ZERO.
           05  TOTAL-DOLLARS-WS          PIC S9(8)V99      VALUE ZERO.
           05  NUMBER-LINES-PER-PAGE-WS  PIC 99            VALUE ZERO.

       01  INV-INPUT-RECORD.
           05  REC-TYPE-IN               PIC X.
           05  BATCH-NO-IN               PIC X(2).
           05  SUPPLIER-NO-IN            PIC X(5).
           05  VOUCHER-NO-IN             PIC X(6).
           05  INVOICE-NO-IN             PIC X(8).
           05  ACCT-NO-IN                PIC X(4).
           05  STORE-NO-IN               PIC X(3).
           05  DATE-IN                   PIC X(8).
           05  FILLER                    PIC X(12).
           05  AMOUNT-IN                 PIC S9(6)V99.
           05  SUPPLIER-IN               PIC X(23).



       01  REPORT-HEADER-LINE-SETUP.
           05                            PIC X(9)     VALUE SPACES.
           05                            PIC X(10) VALUE 'RUN DATE: '.
           05  REPORT-HEADER-DATE-OUT.
               10  HEADER-MO-OUT         PIC 99.
               10                        PIC X    VALUE '/'.
               10  HEADER-DY-OUT         PIC 99.
               10                        PIC X    VALUE '/'.
               10  HEADER-YR-OUT         PIC 9999.
           05                            PIC X(19)    VALUE SPACES.
           05                            PIC X(34)    VALUE 
           'INVENTORY REPORT FOR JEFF MCDONALD'.

           
       01  COLUMN-HEADER-LINE1-SETUP.
           05  FILLER        PIC X.
           05                PIC X(6)  VALUE 'RECORD'.
           05                PIC X(6)  VALUE SPACE.
           05                PIC X(4)  VALUE 'DATE'.
           05                PIC X(11) VALUE SPACE.
           05                PIC X(6)  VALUE 'AMOUNT'.
           05                PIC X(7) VALUE SPACE.
           05                PIC X(7)  VALUE 'ACCOUNT'.
           05                PIC X(5) VALUE SPACE.
           05                PIC X(7)  VALUE 'INVOICE'.
           05                PIC X(4) VALUE SPACE.
           05                PIC X(5)  VALUE 'BATCH'.
           05                PIC X(5) VALUE SPACE.
           05                PIC X(7)  VALUE 'VOUCHER'.
           05                PIC X(4) VALUE SPACE.
           05                PIC X(5)  VALUE 'STORE'.
           05                PIC X(5) VALUE SPACE.
           05                PIC X(8)  VALUE 'SUPPLIER'.
           05                PIC X(11) VALUE SPACE.
           05                PIC X(8)  VALUE 'SUPPLIER'.

       01  COLUMN-HEADER-LINE2-SETUP.
           05  FILLER        PIC X(2)   VALUE SPACES.
           05                PIC X(4)   VALUE 'TYPE'.
           05                PIC X(35)   VALUE SPACES.
           05                PIC X(6)   VALUE 'NUMBER'.
           05                PIC X(6)  VALUE SPACES.
           05                PIC X(6)   VALUE 'NUMBER'.
           05                PIC X(5)   VALUE SPACES.
           05                PIC X(6)   VALUE 'NUMBER'.
           05                PIC X(4)  VALUE SPACES.
           05                PIC X(6)   VALUE 'NUMBER'.
           05                PIC X(5)   VALUE SPACES.
           05                PIC X(6)   VALUE 'NUMBER'.
           05                PIC X(5)  VALUE SPACES.
           05                PIC X(6)   VALUE 'NUMBER'.
           05                PIC X(14)  VALUE SPACES.
           05                PIC X(4)   VALUE 'NAME'.

       01  DETAILED-OUTPUT-LINE-SETUP.
           05  FILLER                    PIC X(3)   VALUE SPACE.
           05  REC-TYPE-OUT              PIC X(1).
           05  FILLER                    PIC X(6)   VALUE SPACE.
           05  DATE-OUT                  PIC XX/XX/XXXX.
           05  FILLER                    PIC X(4)   VALUE SPACE.
           05  AMOUNT-OUT                PIC $$$$,$$9.99BCR.
           05  FILLER                    PIC X(5)   VALUE SPACE.
           05  ACCT-NO-OUT               PIC X(4).
           05  FILLER                    PIC X(6)   VALUE SPACE.
           05  INVOICE-NO-OUT            PIC X(8).
           05  FILLER                    PIC X(6)   VALUE SPACE.
           05  BATCH-NO-OUT              PIC X(2).
           05  FILLER                    PIC X(6)   VALUE SPACE.
           05  VOUCHER-NO-OUT            PIC X(6).
           05  FILLER                    PIC X(6)   VALUE SPACE.
           05  STORE-NO-OUT              PIC X(3).
           05  FILLER                    PIC X(7)   VALUE SPACE.
           05  SUPPLIER-NO-OUT           PIC X(5).
           05  FILLER                    PIC X(6)   VALUE SPACE.
           05  SUPPLIER-OUT              PIC X(23).

       01  TOTAL-RECORDS-LINE-SETUP.
           05  FILLER                    PIC X(6)  VALUE SPACE.
           05                            PIC X(38)   VALUE
           'NUMBER OF RECORDS PROCESSED IS:'.
           05  TOTAL-RECORDS-OUT         PIC ZZZ9.

       01  TOTAL-DOLLARS-LINE-SETUP.
           05  FILLER                    PIC X(6)  VALUE SPACE.
           05                            PIC X(35)   VALUE
           'TOTAL NET DOLLAR AMOUNT IS:'.
           05  TOTAL-DOLLARS-OUT         PIC $$$$,$$$,$$9.99BCR.

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

           MOVE  DETAILED-OUTPUT-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER ADVANCING 1 LINES

           ADD 1 TO NUMBER-LINES-PER-PAGE-WS

           IF NUMBER-LINES-PER-PAGE-WS >= 16

           THEN PERFORM 500-HEADER

           END-IF 

           PERFORM 250-READ-ONE-RECORD.

       500-HEADER.
           MOVE  REPORT-HEADER-LINE-SETUP  TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER PAGE

           MOVE  COLUMN-HEADER-LINE1-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 2 LINES

           MOVE  COLUMN-HEADER-LINE2-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINE

           MOVE SPACES TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 3 LINES

           MOVE 0 TO NUMBER-LINES-PER-PAGE-WS.


       900-CLOSE.
           MOVE TOTAL-RECORDS-WS TO TOTAL-RECORDS-OUT
           MOVE TOTAL-DOLLARS-WS TO TOTAL-DOLLARS-OUT

           MOVE TOTAL-RECORDS-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 3 LINES

           MOVE TOTAL-DOLLARS-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 2 LINES

           CLOSE INV-INPUT-FILE INV-OUTPUT-FILE.
