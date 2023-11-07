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
001300* Program:     DBANK01P.CBL                                     *
001400* Function:    Obtain User details                              *
001500*              VSAM version                                     *
001600*****************************************************************
001700
001800 IDENTIFICATION DIVISION.
001900 PROGRAM-ID.
002000     DBANK01P.
002100 DATE-WRITTEN.
002200     September 2002.
002300 DATE-COMPILED.
002400     Today.
002500
002600 ENVIRONMENT DIVISION.
002700
002800 DATA DIVISION.
002900
003000 WORKING-STORAGE SECTION.
003100 01  WS-MISC-STORAGE.
003200   05  WS-PROGRAM-ID                         PIC X(8)
003300       VALUE 'DBANK01P'.
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).
003500   05  WS-RESP                               PIC S9(8) COMP.
003600   05  WS-BNKCUST-RID                        PIC X(5).
003700
003800 01 WS-BNKCUST-REC.
003900 COPY CBANKVCS.
004000
004100 01  WS-COMMAREA.
004200 COPY CBANKD01.
004300
004400 COPY CABENDD.
004500
004600 LINKAGE SECTION.
004700 01  DFHCOMMAREA.
004800   05  LK-COMMAREA                           PIC X(1)
004900       OCCURS 1 TO 4096 TIMES
005000         DEPENDING ON WS-COMMAREA-LENGTH.
005100
005200* COPY CENTRY.
001400 PROCEDURE DIVISION.
001500
005300*****************************************************************
005400* Move the passed data to our area                              *
005500*****************************************************************
005600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.
005700     MOVE DFHCOMMAREA TO WS-COMMAREA.
005800
005900*****************************************************************
006000* Initialize our output area                                    *
006100*****************************************************************
006200     MOVE SPACES TO CD01O-DATA.
006300
006400*****************************************************************
006500* Now attempt to get the requested record                       *
006600*****************************************************************
006700     MOVE CD01I-PERSON-PID TO WS-BNKCUST-RID.
006800     EXEC CICS READ FILE('BNKCUST')
006900                    INTO(WS-BNKCUST-REC)
007000                    LENGTH(LENGTH OF WS-BNKCUST-REC)
007100                    RIDFLD(WS-BNKCUST-RID)
007200                    RESP(WS-RESP)
007300     END-EXEC.
007400
007500*****************************************************************
007600* Did we get the record OK                                      *
007700*****************************************************************
007800     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)
007900        MOVE BCS-REC-PID TO CD01O-PERSON-PID
008000        MOVE BCS-REC-NAME TO CD01O-PERSON-NAME
008100     END-IF.
008200
008300*****************************************************************
008400* Was the record not found?                                     *
008500*****************************************************************
008600     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
008700        MOVE 'Person not found' TO CD01O-PERSON-NAME
008800     END-IF.
008900
009000*****************************************************************
009100* Move the result back to the callers area                      *
009200*****************************************************************
009300     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).
009400
009500*****************************************************************
009600* Return to our caller                                          *
009700*****************************************************************
009800* COPY CRETURN.
001400     EXEC CICS RETURN
001500     END-EXEC.
001600     GOBACK.
001700

009900
010000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
