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
001300* Program:     BBANK10P.CBL                                     *
001400* Layer:       Business logic                                   *
001500* Function:    Signon to system to identify user                *
001600*****************************************************************
001700
001800 IDENTIFICATION DIVISION.
001900 PROGRAM-ID.
002000     BBANK10P.
002100 DATE-WRITTEN.
002200     September 2023.
002300 DATE-COMPILED.
002400     Today.
002500
002600 ENVIRONMENT DIVISION.
002700
002800 DATA DIVISION.
002900 WORKING-STORAGE SECTION.
003000 01  WS-MISC-STORAGE.
003100   05  WS-PROGRAM-ID                         PIC X(8)
003200       VALUE 'BBANK10P'.
003300   05  WS-INPUT-FLAG                         PIC X(1).
003400     88  INPUT-OK                            VALUE '0'.
003500     88  INPUT-ERROR                         VALUE '1'.
003600   05  WS-RETURN-FLAG                        PIC X(1).
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.
003900   05  WS-RETURN-MSG                         PIC X(75).
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.
004100   05  WS-PFK-FLAG                           PIC X(1).
004200     88  PFK-VALID                           VALUE '0'.
004300     88  PFK-INVALID                         VALUE '1'.
004400   05  WS-ERROR-MSG                          PIC X(75).
004500
004600 01  WS-BANK-DATA.
004700 COPY CBANKDAT.
004800
004900 01  WS-HELP-DATA.
005000 COPY CHELPD01.
005100
005200 01  WS-PERSON.
005300 COPY CBANKD01.
005400
005500 01  WS-SECURITY.
005600 COPY CPSWDD01.
005700
005800 COPY CABENDD.
005900
006000 LINKAGE SECTION.
006100 01  DFHCOMMAREA.
006200   05  LK-COMMAREA                           PIC X(6144).
006300
006400*COPY CENTRY.
001400 PROCEDURE DIVISION.
001500
           DISPLAY "TEST DISPLAY"
006500*****************************************************************
006600* Make ourselves re-entrant                                     *
006700*****************************************************************
006800     MOVE SPACES TO WS-ERROR-MSG.
006900
007000*****************************************************************
007100* Move the passed area to our area                              *
007200*****************************************************************
007300     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA.
007400
007500*****************************************************************
007600* Ensure error message is cleared                               *
007700*****************************************************************
007800     MOVE SPACES TO BANK-ERROR-MSG.
007900
008000*****************************************************************
008100* If this is the first time in, then we have to do set up the   *
008200* COMMAREA and ask for the first map to be displayed.           *
008300*****************************************************************
008400     IF BANK-NO-CONV-IN-PROGRESS
008500        SET BANK-CONV-IN-PROGRESS TO TRUE
008600        MOVE 'BBANK10P' TO BANK-LAST-PROG
008700        MOVE 'BBANK10P' TO BANK-NEXT-PROG
008800        MOVE LOW-VALUES TO BANK-SIGNON-ID
008900        MOVE LOW-VALUES TO BANK-USERID
009000        MOVE LOW-VALUES TO BANK-PSWD
009100        MOVE SPACES TO BANK-LAST-MAPSET
009200        MOVE SPACES TO BANK-LAST-MAP
009300        MOVE 'MBANK10' TO BANK-NEXT-MAPSET
009400        MOVE 'BANK10A' TO BANK-NEXT-MAP
009500        GO TO COMMON-RETURN
009600     END-IF.
009700
009800*****************************************************************
009900* This is the main process                                      *
010000*****************************************************************
010100
010200*****************************************************************
010300* Save the passed return message and then turn it off           *
010400*****************************************************************
010500     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.
010600     SET BANK-RETURN-MSG-OFF TO TRUE.
010700
010800*****************************************************************
010900* Check the AID to see if its valid at this point               *
011000*****************************************************************
011100     SET PFK-INVALID TO TRUE.
011200     IF BANK-AID-ENTER OR
011300        BANK-AID-PFK03
011400        SET PFK-VALID TO TRUE
011500     END-IF.
011600     IF BANK-AID-PFK01 AND
011700        BANK-HELP-INACTIVE
011800        SET BANK-HELP-ACTIVE TO TRUE
011900        SET PFK-VALID TO TRUE
012000     END-IF.
012100     IF BANK-AID-PFK04 AND
012200        BANK-HELP-ACTIVE
012300        SET PFK-VALID TO TRUE
012400     END-IF.
012500     IF PFK-INVALID
012600        SET BANK-AID-ENTER TO TRUE
012700     END-IF.
012800
012900*****************************************************************
013000* Check the AID to see if we have to quit                       *
013100*****************************************************************
013200     IF BANK-AID-PFK03
013300        MOVE 'BBANK10P' TO BANK-LAST-PROG
013400        MOVE 'BBANK99P' TO BANK-NEXT-PROG
013500        MOVE 'MBANK10' TO BANK-LAST-MAPSET
013600        MOVE 'BANK10A' TO BANK-LAST-MAP
013700        MOVE 'MBANK99' TO BANK-NEXT-MAPSET
013800        MOVE 'BANK99A' TO BANK-NEXT-MAP
013900        GO TO COMMON-RETURN
014000     END-IF.
014100
014200*****************************************************************
014300* Check the to see if user needs or has been using help         *
014400*****************************************************************
014500     IF BANK-HELP-ACTIVE
014600        IF BANK-AID-PFK04
014700           SET BANK-HELP-INACTIVE TO TRUE
014800           MOVE 00 TO BANK-HELP-SCREEN
014900           MOVE 'BBANK10P' TO BANK-LAST-PROG
015000           MOVE 'BBANK10P' TO BANK-NEXT-PROG
015100           MOVE 'MBANK10' TO BANK-LAST-MAPSET
015200           MOVE 'HELP10A' TO BANK-LAST-MAP
015300           MOVE 'MBANK10' TO BANK-NEXT-MAPSET
015400           MOVE 'BANK10A' TO BANK-NEXT-MAP
015500           GO TO COMMON-RETURN
015600        ELSE
015700           MOVE 01 TO BANK-HELP-SCREEN
015800           MOVE 'BBANK10P' TO BANK-LAST-PROG
015900           MOVE 'BBANK10P' TO BANK-NEXT-PROG
016000           MOVE 'MBANK10' TO BANK-LAST-MAPSET
016100           MOVE 'BANK10A' TO BANK-LAST-MAP
016200           MOVE 'MBANK10' TO BANK-NEXT-MAPSET
016300           MOVE 'HELP10A' TO BANK-NEXT-MAP
016400           MOVE 'BANK10' TO HELP01I-SCRN
016500*          COPY CHELPX01.
001600           EXEC CICS LINK PROGRAM('DHELP01P')
001700                    COMMAREA(HELP01-DATA)
001800                    LENGTH(LENGTH OF HELP01-DATA)
001900           END-EXEC
       
016600           MOVE HELP01O-DATA TO BANK-HELP-DATA
016700           GO TO COMMON-RETURN
016800     END-IF.
016900
017000     PERFORM VALIDATE-USER THRU
017100             VALIDATE-USER-EXIT.
017200
017300* If we had an error display error and return
017400     IF INPUT-ERROR
017500        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG
017600        MOVE 'SBANK10P' TO BANK-LAST-PROG
017700        MOVE 'SBANK10P' TO BANK-NEXT-PROG
017800        MOVE 'MBANK10' TO BANK-LAST-MAPSET
017900        MOVE 'BANK10A' TO BANK-LAST-MAP
018000        MOVE 'MBANK10' TO BANK-NEXT-MAPSET
018100        MOVE 'BANK10A' TO BANK-NEXT-MAP
018200        GO TO COMMON-RETURN
018300     END-IF.
018400
018500     MOVE 'BBANK20P' TO BANK-NEXT-PROG.
018600     GO TO COMMON-RETURN.
018700
018800 COMMON-RETURN.
018900     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA).
019000*COPY CRETURN.
001400     EXEC CICS RETURN
001500     END-EXEC.
001600     GOBACK.
001700
019100
019200 VALIDATE-USER.
019300     SET INPUT-OK TO TRUE.
019400     INSPECT BANK-SIGNON-ID
019500       CONVERTING 'abcdefghijklmnopqrstuvwxyz'
019600               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
019700     IF BANK-SIGNON-ID IS EQUAL TO 'GUEST'
019800        MOVE 'GUEST' TO BANK-USERID
019900        MOVE 'Guest' TO BANK-USERID-NAME
020000        GO TO VALIDATE-USER-EXIT
020100     END-IF.
020200     IF BANK-SIGNON-ID IS EQUAL TO LOW-VALUES
020300        MOVE 'Please input user id' TO WS-ERROR-MSG
020400        GO TO VALIDATE-USER-ERROR
020500     END-IF.
020600     IF BANK-PSWD IS EQUAL TO LOW-VALUES
020700        MOVE 'Please input password' TO WS-ERROR-MSG
020800        GO TO VALIDATE-USER-ERROR
020900     END-IF.
021000* We now make sure the user is valid.......
021100     MOVE SPACES TO CPSWDD01-DATA.
021200     MOVE BANK-SIGNON-ID TO CPSWDD01I-USERID.
021300     MOVE BANK-PSWD TO CPSWDD01I-PASSWORD
021400* If user starts with "Z" then treat as "B"
021500     IF CPSWDD01I-USERID(1:1) IS EQUAL TO 'Z'
021600        MOVE 'B' TO  CPSWDD01I-USERID(1:1)
021700     END-IF.
021800
021900     SET PSWD-SIGNON TO TRUE
022000
022100*COPY CPSWDX01.
001600     EXEC CICS LINK PROGRAM('SPSWD01P')
001700                    COMMAREA(CPSWDD01-DATA)
001800                    LENGTH(LENGTH OF CPSWDD01-DATA)
001900     END-EXEC
       
022200     IF CPSWDD01O-MESSAGE IS NOT EQUAL TO SPACES
022300        MOVE CPSWDD01O-MESSAGE TO WS-ERROR-MSG
022400        GO TO VALIDATE-USER-ERROR
022500     END-IF.
022600* We now make sure the user is actually a customer......
022700     MOVE SPACES TO CD01-DATA.
022800     MOVE BANK-SIGNON-ID TO CD01I-PERSON-PID.
022900* If user starts with "Z" then treat as "B"
023000     IF CD01I-PERSON-PID(1:1) IS EQUAL TO 'Z'
023100        MOVE 'B' TO  CD01I-PERSON-PID(1:1)
023200     END-IF.
023300*COPY CBANKX01.
001600     EXEC CICS LINK PROGRAM('DBANK01P')
001700                    COMMAREA(CD01-DATA)
001800                    LENGTH(LENGTH OF CD01-DATA)
001900     END-EXEC.

023400     IF CD01O-PERSON-PID IS EQUAL TO SPACES
023500        MOVE CD01O-PERSON-NAME TO WS-ERROR-MSG
023600        GO TO VALIDATE-USER-ERROR
023700     ELSE
023800        MOVE CD01O-PERSON-NAME TO BANK-USERID-NAME
023900        MOVE BANK-SIGNON-ID TO BANK-USERID
024000        IF BANK-USERID(1:1) IS EQUAL TO 'Z'
024100           MOVE 'B' TO  BANK-USERID(1:1)
024200        END-IF
024300        GO TO VALIDATE-USER-EXIT
024400     END-IF.
024500 VALIDATE-USER-ERROR.
024600     SET INPUT-ERROR TO TRUE.
024700 VALIDATE-USER-EXIT.
024800     EXIT.
024900
025000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
