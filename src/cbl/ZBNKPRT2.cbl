       IDENTIFICATION DIVISION.
       PROGRAM-ID.   ZBNKPRT2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   IBM.
       OBJECT-COMPUTER.   IBM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE   ASSIGN BANK10
               ORGANIZATION IS SEQUENTIAL.
           SELECT REPORT-FILE    ASSIGN REPT10
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  AR-NAME                 PIC X(19).
           05  F1                      PIC X(5).
           05  AR-ADDRESS              PIC X(20).
           05  F2                      PIC XXXXX.
           05  AR-PHONE                PIC X(7).
           05  F3                      PIC XXX.
           05  AR-BIRTH-DATE           PIC X(6).
           05  F4                      PIC XXXX.
           05  AR-RECORD-TYPE          PIC X.
           05  F5                      PIC X(4).
       01  PRODUCT-RECORD.
           05  PR-NAME                 PIC X(19).
           05  F6                      PIC X(5).
           05  PR-NUMBER               PIC X(5).
           05  F7                      PIC X(5).
           05  PR-CREDITS              PIC 9.
           05  F8                      PIC X(34).
           05  PR-RECORD-TYPE          PIC X.
           05  F9                      PIC X(04).
       FD  REPORT-FILE.
       01  REPORT-LINE-OUT             PIC X(80).
       WORKING-STORAGE SECTION.
       01  SWITCHES-IN-PROGRAM.
           05  SW-END-OF-DATA          PIC X VALUE 'N'.
               88  END-OF-DATA               VALUE 'Y'.
       01  ACCUMS-AND-COUNTERS.
           05  ACCUM-CREDITS           PIC 999 VALUE 0.
           05  CTR-PRODUCTS            PIC 999 VALUE 0.
           05  CTR-ACCOUNTS            PIC 9(5) VALUE 0.
           05  CTR-LINES               PIC 99 VALUE 0.
       01  SAVE-AREAS.
           05  SAVE-NAME               PIC X(19).
       01  GRAND-TOTAL-LINE.
           05  FILLER                  PIC X(30)
                    VALUE ' TOTAL ACCOUNTS PROCESSED IS: '.
           05  GTL-ACCOUNTS-COUNT       PIC ZZZZZ.
       01  DETAIL-LINE.
           05  FILLER                  PIC X(5) VALUE SPACE.
           05  DL-NAME                 PIC X(19).
           05  FILLER                  PIC X(8) VALUE SPACE.
           05  DL-PRODUCTS             PIC ZZZ.
           05  FILLER                  PIC X(10) VALUE SPACE.
           05  DL-CREDITS              PIC ZZZZ.
       01  HEADING-1.
           05  FILLER                  PIC X(10) VALUE SPACE.
           05  FILLER                  PIC X(80) VALUE
               'A C C O U N T   C R E D I T S   R E P O R T'.
       01  HEADING-2.
           05  FILLER                  PIC X(5)  VALUE SPACE.
           05  FILLER                  PIC X(25) VALUE 'ACCOUNT NAME'.
           05  FILLER                  PIC X(15) VALUE 'PRODUCTS'.
           05  FILLER                  PIC X(7)  VALUE 'CREDITS'.
       PROCEDURE DIVISION.
       000-TOP-LEVEL.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-RECORDS UNTIL END-OF-DATA.
           PERFORM 300-WRAP-UP.
           STOP RUN.
       100-INITIALIZATION.
           OPEN INPUT  ACCOUNT-FILE.
           OPEN OUTPUT REPORT-FILE.
           PERFORM 211-PAGE-CHANGE-RTN.
           PERFORM 230-READ-A-RECORD.
           MOVE PR-NAME TO SAVE-NAME.
           ADD 1 TO CTR-ACCOUNTS.
       200-PROCESS-RECORDS.
           IF PR-RECORD-TYPE IS EQUAL TO '1'
           THEN
               PERFORM 210-PROCESS-1-RECORDS
               MOVE PR-NAME TO SAVE-NAME
               ADD 1 TO CTR-ACCOUNTS
           ELSE
               PERFORM 220-PROCESS-2-RECORDS.
           PERFORM 230-READ-A-RECORD.
       210-PROCESS-1-RECORDS.
           IF CTR-LINES IS GREATER THAN 30
           THEN
               PERFORM 211-PAGE-CHANGE-RTN.
           PERFORM 212-BUILD-DETAIL-LINE.
           MOVE DETAIL-LINE TO REPORT-LINE-OUT
           WRITE REPORT-LINE-OUT
           MOVE ZERO TO CTR-PRODUCTS.
       211-PAGE-CHANGE-RTN.
           MOVE HEADING-1 TO REPORT-LINE-OUT
           WRITE REPORT-LINE-OUT
           MOVE HEADING-2 TO REPORT-LINE-OUT
           WRITE REPORT-LINE-OUT
           MOVE ZERO TO CTR-LINES.
       212-BUILD-DETAIL-LINE.
           MOVE SAVE-NAME TO DL-NAME.
           MOVE CTR-PRODUCTS TO DL-PRODUCTS.
           MOVE ACCUM-CREDITS TO DL-CREDITS.
       220-PROCESS-2-RECORDS.
           ADD PR-CREDITS TO ACCUM-CREDITS.
           ADD 1 TO CTR-PRODUCTS.
       230-READ-A-RECORD.
           READ ACCOUNT-FILE
               AT END MOVE 'Y' TO SW-END-OF-DATA.
       300-WRAP-UP.
           MOVE CTR-ACCOUNTS TO GTL-ACCOUNTS-COUNT.
           MOVE GRAND-TOTAL-LINE TO  REPORT-LINE-OUT
           WRITE REPORT-LINE-OUT
           CLOSE REPORT-FILE ACCOUNT-FILE.

