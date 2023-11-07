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
001300* Program:     DBANK52P.CBL                                     *
001400* Function:    Sequential read of bank data for batch job       *
001500*              VSAM version                                     *
001600*****************************************************************
001700
001800 IDENTIFICATION DIVISION.
001900 PROGRAM-ID.
002000     DBANK52P.
002100 DATE-WRITTEN.
002200     September 2002.
002300 DATE-COMPILED.
002400     Today.
002500
002600 ENVIRONMENT DIVISION.
002700
002800 INPUT-OUTPUT   SECTION.
002900   FILE-CONTROL.
003000     SELECT BNKTXN-FILE
003100            ASSIGN       TO BNKTXN
003200            ORGANIZATION IS INDEXED
003300            ACCESS MODE  IS SEQUENTIAL
003400            RECORD KEY   IS BTX-REC-TIMESTAMP
003500            ALTERNATE KEY IS BTX-REC-ALTKEY1 WITH DUPLICATES
003600            FILE STATUS  IS WS-BNKTXN-STATUS.
003700
003800 DATA DIVISION.
003900
004000 FILE SECTION.
004100 FD  BNKTXN-FILE.
004200 01  BNKTXN-REC.
004300 COPY CBANKVTX.
004400
004500 WORKING-STORAGE SECTION.
004600 01  WS-MISC-STORAGE.
004700   05  WS-PROGRAM-ID                         PIC X(8)
004800       VALUE 'DBANK52P'.
004900   05  WS-COMMAREA-LENGTH                    PIC 9(5).
005000   05  WS-SUB1                               PIC S9(4) COMP.
005100
005200   05  WS-BNKTXN-STATUS.
005300     10  WS-BNKTXN-STAT1                     PIC X(1).
005400     10  WS-BNKTXN-STAT2                     PIC X(1).
005500
005600 01  WS-COMMAREA.
005700 COPY CIOFUNCS.
005800 COPY CBANKD51.
005900 COPY CBANKD52.
006000
006100 COPY CBANKTXD.
006200
006300 LINKAGE SECTION.
006400 01  DFHCOMMAREA.
006500   05  LK-COMMAREA                           PIC X(1)
006600         OCCURS 1 TO 4096 TIMES
006700           DEPENDING ON WS-COMMAREA-LENGTH.
006800
006900 PROCEDURE DIVISION USING DFHCOMMAREA.
007000*****************************************************************
007100* Move the passed data to our area                              *
007200*****************************************************************
007300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.
007400     MOVE DFHCOMMAREA TO WS-COMMAREA.
007500
007600*****************************************************************
007700* Initialize our output area                                    *
007800*****************************************************************
007900     MOVE SPACES TO CD52O-DATA.
008000
008100*****************************************************************
008200* Check what is required                                        *
008300*****************************************************************
008400     EVALUATE TRUE
008500       WHEN IO-REQUEST-FUNCTION-OPEN
008600        PERFORM OPEN-FILE THRU
008700                OPEN-FILE-EXIT
008800       WHEN IO-REQUEST-FUNCTION-READ
008900        PERFORM READ-FILE THRU
009000                READ-FILE-EXIT
009100       WHEN IO-REQUEST-FUNCTION-CLOSE
009200        PERFORM CLOSE-FILE THRU
009300                CLOSE-FILE-EXIT
009400       WHEN OTHER
009500        SET IO-REQUEST-STATUS-ERROR TO TRUE
009600     END-EVALUATE.
009700
009800*****************************************************************
009900* Move the result back to the callers area                      *
010000*****************************************************************
010100     MOVE WS-COMMAREA TO DFHCOMMAREA  (1:WS-COMMAREA-LENGTH).
010200
010300*****************************************************************
010400* Return to our caller                                          *
010500*****************************************************************
010600     GOBACK.
010700
010800
010900*****************************************************************
011000* Open the file so we can read TXN sequentially                 *
011100*****************************************************************
011200 OPEN-FILE.
011300     OPEN INPUT BNKTXN-FILE.
011400     IF WS-BNKTXN-STATUS = '00'
011500        SET IO-REQUEST-STATUS-OK TO TRUE
011600     ELSE
011700        SET IO-REQUEST-STATUS-ERROR TO TRUE
011800     END-IF.
011900 OPEN-FILE-EXIT.
012000     EXIT.
012100
012200*****************************************************************
012300* Read sequentially through the transaction file                *
012400*****************************************************************
012500 READ-FILE.
012600     READ BNKTXN-FILE.
012700* Was read ok?
012800     IF WS-BNKTXN-STATUS IS EQUAL TO '00'
012900        SET IO-REQUEST-STATUS-OK TO TRUE
013000     END-IF.
013100* Was read at end-of-file?
013200     IF WS-BNKTXN-STATUS IS EQUAL TO '10'
013300        SET IO-REQUEST-STATUS-EOF TO TRUE
013400     END-IF.
013500     IF WS-BNKTXN-STATUS IS NOT EQUAL TO '00' AND
013600        WS-BNKTXN-STATUS IS NOT EQUAL TO '10'
013700        SET IO-REQUEST-STATUS-ERROR TO TRUE
013800     END-IF.
013900     IF WS-BNKTXN-STATUS IS EQUAL TO '00'
014000        IF BTX-REC-TYPE IS EQUAL TO '1' AND
014100           (BTX-REC-PID IS EQUAL TO CD52I-PID OR
014200            CD52-REQUESTED-ALL)
014300           MOVE BTX-REC-PID TO CD52O-PID
014400           MOVE BTX-REC-ACCNO TO CD52O-ACC-NO
014500           MOVE BTX-REC-TIMESTAMP TO CD52O-TIMESTAMP
014600           MOVE BTX-REC-AMOUNT TO CD52O-AMOUNT
014700           MOVE BTX-REC-DATA-OLD TO TXN-DATA-OLD
014800           MOVE TXN-T1-OLD-DESC TO CD52O-DESC
014900        ELSE
015000           GO TO READ-FILE
015100        END-IF
015200     END-IF.
015300 READ-FILE-EXIT.
015400     EXIT.
015500
015600*****************************************************************
015700* Close the file                                                *
015800*****************************************************************
015900 CLOSE-FILE.
016000     CLOSE BNKTXN-FILE.
016100     IF WS-BNKTXN-STATUS = '00'
016200        SET IO-REQUEST-STATUS-OK TO TRUE
016300     ELSE
016400       SET IO-REQUEST-STATUS-ERROR TO TRUE
016500     END-IF.
016600 CLOSE-FILE-EXIT.
016700     EXIT.
016800
016900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
