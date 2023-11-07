000100*****************************************************************
000200*                                                               *
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   *
000400*   This demonstration program is provided for use by users     *
000500*   of Micro Focus products and may be used, modified and       *
000600*   distributed as part of your application provided that       *
000700*   you properly acknowledge the copyright of Micro Focus       *
000800*   in this material.                                           *
000900*                                                               *
001000*****************************************************************
001100
001200*****************************************************************
001300* Program:     DBANK51P.CBL                                     *
001400* Function:    Sequential read of bank data for batch job       *
001500*              VSAM version                                     *
001600*****************************************************************
001700
001800 IDENTIFICATION DIVISION.
001900 PROGRAM-ID.
002000     DBANK51P.
002100 DATE-WRITTEN.
002200     September 2002.
002300 DATE-COMPILED.
002400     Today.
002500
002600 ENVIRONMENT DIVISION.
002700
002800 INPUT-OUTPUT   SECTION.
002900   FILE-CONTROL.
003000     SELECT BNKACC-FILE
003100            ASSIGN       TO BNKACC
003200            ORGANIZATION IS INDEXED
003300            ACCESS MODE  IS SEQUENTIAL
003400            RECORD KEY   IS BAC-REC-ACCNO
003500            ALTERNATE KEY IS BAC-REC-PID WITH DUPLICATES
003600            FILE STATUS  IS WS-BNKACC-STATUS.
003700
003800     SELECT BNKCUST-FILE
003900            ASSIGN       TO BNKCUST
004000            ORGANIZATION IS INDEXED
004100            ACCESS MODE  IS RANDOM
004200            RECORD KEY   IS BCS-REC-PID
004300            ALTERNATE KEY IS BCS-REC-NAME
004400              WITH DUPLICATES
004500            ALTERNATE KEY IS BCS-REC-NAME-FF
004600              WITH DUPLICATES
004700            FILE STATUS  IS WS-BNKCUST-STATUS.
004800
004900     SELECT BNKATYP-FILE
005000            ASSIGN       TO BNKATYP
005100            ORGANIZATION IS INDEXED
005200            ACCESS MODE  IS RANDOM
005300            RECORD KEY   IS BAT-REC-TYPE
005400            FILE STATUS  IS WS-BNKATYP-STATUS.
005500
005600 DATA DIVISION.
005700
005800 FILE SECTION.
005900 FD  BNKACC-FILE.
006000 01  BNKACC-REC.
006100 COPY CBANKVAC.
006200
006300 FD  BNKCUST-FILE.
006400 01  BNKCUST-REC.
006500 COPY CBANKVCS.
006600
006700 FD  BNKATYP-FILE.
006800 01  BNKATYP-REC.
006900 COPY CBANKVAT.
007000
007100 WORKING-STORAGE SECTION.
007200 01  WS-MISC-STORAGE.
007300   05  WS-PROGRAM-ID                         PIC X(8)
007400       VALUE 'DBANK51P'.
007500   05  WS-COMMAREA-LENGTH                    PIC 9(5).
007600   05  WS-SUB1                               PIC S9(4) COMP.
007700
007800   05  WS-BNKACC-STATUS.
007900     10  WS-BNKACC-STAT1                     PIC X(1).
008000     10  WS-BNKACC-STAT2                     PIC X(1).
008100
008200   05  WS-BNKCUST-STATUS.
008300     10  WS-BNKCUST-STAT1                    PIC X(1).
008400     10  WS-BNKCUST-STAT2                    PIC X(1).
008500
008600   05  WS-BNKATYP-STATUS.
008700     10  WS-BNKATYP-STAT1                    PIC X(1).
008800     10  WS-BNKATYP-STAT2                    PIC X(1).
008900
009000 01  WS-COMMAREA.
009100 COPY CIOFUNCS.
009200 COPY CBANKD51.
009300
009400 LINKAGE SECTION.
009500 01  DFHCOMMAREA.
009600   05  LK-COMMAREA                           PIC X(1)
009700         OCCURS 1 TO 4096 TIMES
009800           DEPENDING ON WS-COMMAREA-LENGTH.
009900
010000 PROCEDURE DIVISION USING DFHCOMMAREA.
010100*****************************************************************
010200* Move the passed data to our area                              *
010300*****************************************************************
010400     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.
010500     MOVE DFHCOMMAREA TO WS-COMMAREA.
010600
010700*****************************************************************
010800* Initialize our output area                                    *
010900*****************************************************************
011000     MOVE SPACES TO CD51O-DATA.
011100
011200*****************************************************************
011300* Check what is required                                        *
011400*****************************************************************
011500     EVALUATE TRUE
011600       WHEN IO-REQUEST-FUNCTION-OPEN
011700        PERFORM OPEN-FILE THRU
011800                OPEN-FILE-EXIT
011900       WHEN IO-REQUEST-FUNCTION-READ
012000        PERFORM READ-FILE THRU
012100                READ-FILE-EXIT
012200       WHEN IO-REQUEST-FUNCTION-CLOSE
012300        PERFORM CLOSE-FILE THRU
012400                CLOSE-FILE-EXIT
012500       WHEN OTHER
012600        SET IO-REQUEST-STATUS-ERROR TO TRUE
012700     END-EVALUATE.
012800
012900*****************************************************************
013000* Move the result back to the callers area                      *
013100*****************************************************************
013200     MOVE WS-COMMAREA TO DFHCOMMAREA  (1:WS-COMMAREA-LENGTH).
013300
013400*****************************************************************
013500* Return to our caller                                          *
013600*****************************************************************
013700     GOBACK.
013800
013900
014000*****************************************************************
014100* Open the file so we can read ACC sequentially, others randomly*
014200*****************************************************************
014300 OPEN-FILE.
014400     OPEN INPUT BNKACC-FILE.
014500     OPEN INPUT BNKCUST-FILE.
014600     OPEN INPUT BNKATYP-FILE.
014700     IF CD51-REQUESTED-ALL
014800        MOVE LOW-VALUES TO BAC-REC-PID
014900        START BNKACC-FILE KEY GREATER THAN BAC-REC-PID
015000     ELSE
015100        MOVE CD51I-PID TO BAC-REC-PID
015200        START BNKACC-FILE KEY EQUAL BAC-REC-PID
015300     END-IF
015400     IF WS-BNKACC-STATUS = '00' AND
015500        WS-BNKCUST-STATUS = '00' AND
015600        WS-BNKATYP-STATUS = '00'
015700        SET IO-REQUEST-STATUS-OK TO TRUE
015800     ELSE
015900        SET IO-REQUEST-STATUS-ERROR TO TRUE
016000     END-IF.
016100 OPEN-FILE-EXIT.
016200     EXIT.
016300
016400*****************************************************************
016500* Read sequentially through the customer file                   *
016600*****************************************************************
016700 READ-FILE.
016800     READ BNKACC-FILE.
016900* If key is greater than the one we want, fake end-of-file
017000     IF NOT CD51-REQUESTED-ALL AND
017100        BAC-REC-PID IS NOT EQUAL TO CD51I-PID
017200        MOVE '10' TO WS-BNKACC-STATUS
017300     END-IF.
017400* Was read ok?
017500     IF WS-BNKACC-STATUS IS EQUAL TO '00'
017600        SET IO-REQUEST-STATUS-OK TO TRUE
017700     END-IF.
017800* Was read a duplicate key?
017900     IF WS-BNKACC-STATUS IS EQUAL TO '02'
018000        MOVE '00' TO WS-BNKACC-STATUS
018100        SET IO-REQUEST-STATUS-OK TO TRUE
018200     END-IF.
018300* Was read at end-of-file?
018400     IF WS-BNKACC-STATUS IS EQUAL TO '10'
018500        SET IO-REQUEST-STATUS-EOF TO TRUE
018600     END-IF.
018700     IF WS-BNKACC-STATUS IS NOT EQUAL TO '00' AND
018800        WS-BNKACC-STATUS IS NOT EQUAL TO '10'
018900        SET IO-REQUEST-STATUS-ERROR TO TRUE
019000     END-IF.
019100     IF WS-BNKACC-STATUS IS EQUAL TO '00'
019200        MOVE BAC-REC-PID TO CD51O-PID
019300        MOVE BAC-REC-ACCNO TO CD51O-ACC-NO
019400        MOVE BAC-REC-BALANCE TO CD51O-ACC-CURR-BAL
019500        MOVE BAC-REC-LAST-STMT-DTE TO CD51O-ACC-LAST-STMT-DTE
019600        MOVE BAC-REC-LAST-STMT-BAL TO CD51O-ACC-LAST-STMT-BAL
019700        IF BAC-REC-PID IS NOT EQUAL TO BCS-REC-PID
019800           MOVE BAC-REC-PID TO BCS-REC-PID
019900           READ BNKCUST-FILE
020000           IF WS-BNKCUST-STATUS IS NOT EQUAL TO '00'
020100              MOVE SPACES TO BCS-RECORD
020200              MOVE 'Customer name unavailable' TO BCS-REC-NAME
020300           END-IF
020400        END-IF
020500
020600        MOVE BCS-REC-NAME TO CD51O-NAME
020700        MOVE BCS-REC-ADDR1 TO CD51O-ADDR1
020800        MOVE BCS-REC-ADDR2 TO CD51O-ADDR2
020900        MOVE BCS-REC-STATE TO CD51O-STATE
021000        MOVE BCS-REC-CNTRY TO CD51O-CNTRY
021100        MOVE BCS-REC-POST-CODE TO CD51O-POST-CODE
021200        MOVE BCS-REC-EMAIL TO CD51O-EMAIL
021300
021400        MOVE BAC-REC-TYPE TO BAT-REC-TYPE
021500        READ BNKATYP-FILE
021600        IF WS-BNKATYP-STATUS IS NOT EQUAL TO '00'
021700           MOVE 'A/C description unavailable' TO CD51O-ACC-DESC
021800        ELSE
021900           MOVE BAT-REC-DESC TO CD51O-ACC-DESC
022000        END-IF
022100     END-IF.
022200 READ-FILE-EXIT.
022300     EXIT.
022400
022500*****************************************************************
022600* Close the file                                                *
022700*****************************************************************
022800 CLOSE-FILE.
022900     CLOSE BNKCUST-FILE.
023000     CLOSE BNKACC-FILE.
023100     CLOSE BNKATYP-FILE.
023200     IF WS-BNKCUST-STATUS = '00' AND
023300        WS-BNKACC-STATUS = '00' AND
023400        WS-BNKATYP-STATUS = '00'
023500        SET IO-REQUEST-STATUS-OK TO TRUE
023600     ELSE
023700       SET IO-REQUEST-STATUS-ERROR TO TRUE
023800     END-IF.
023900 CLOSE-FILE-EXIT.
024000     EXIT.
024100
024200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
