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
001300* Prgram:      ZBNKEXT1.CBL                                     *
001400* Function:    Extract data to print bank statements            *
001500*****************************************************************
001600 IDENTIFICATION DIVISION.
001700 PROGRAM-ID.
001800     ZBNKEXT1.
001900 DATE-WRITTEN.
002000     September 2002.
002100 DATE-COMPILED.
002200     Today.
002300 ENVIRONMENT DIVISION.
002400 INPUT-OUTPUT   SECTION.
002500   FILE-CONTROL.
002600     SELECT EXTRACT-FILE
002700            ASSIGN       TO EXTRACT
002800            ORGANIZATION IS SEQUENTIAL
002900            ACCESS MODE  IS SEQUENTIAL
003000            FILE STATUS  IS WS-EXTRACT-STATUS.
003100
003200 DATA DIVISION.
003300 FILE SECTION.
003400 FD  EXTRACT-FILE
003500     RECORDING MODE IS V
003600     RECORD CONTAINS 66 TO 95 CHARACTERS.
003700 COPY CBANKXT1.
003800
003900 WORKING-STORAGE SECTION.
004000 COPY CTIMERD.
004100
004200 01  WS-MISC-STORAGE.
004300   05  WS-PROGRAM-ID                         PIC X(8)
004400       VALUE 'ZBNKEXT1'.
004500   05  WS-EXTRACT-STATUS.
004600     10  WS-EXTRACT-STAT1                    PIC X(1).
004700     10  WS-EXTRACT-STAT2                    PIC X(1).
004800
004900   05  WS-IO-STATUS.
005000     10  WS-IO-STAT1                         PIC X(1).
005100     10  WS-IO-STAT2                         PIC X(1).
005200
005300   05  WS-TWO-BYTES.
005400     10  WS-TWO-BYTES-LEFT                   PIC X(1).
005500     10  WS-TWO-BYTES-RIGHT                  PIC X(1).
005600   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES
005700                                             PIC 9(1) COMP.
005800
005900   05  WS-RECORD-COUNTER1                    PIC 9(5)
006000       VALUE ZERO.
006100   05  WS-RECORD-COUNTER2                    PIC 9(5)
006200       VALUE ZERO.
006300
006400   05  WS-LAST-PID                           PIC X(5)
006500       VALUE LOW-VALUES.
006600
006700 01  WS-ZBNKRPC1-FIELDS.
006800   05  WS-ZBNKRPC1-REQUESTED                 PIC X(1)
006900       VALUE LOW-VALUES.
007000     88  RPC-REQUESTED                       VALUE 'Y'.
007100   05  WS-ZBNKRPC1-PGM                       PIC X(8)
007200       VALUE SPACES.
007300   05  WS-ZBNKRPC1-IND                       PIC X(1)
007400       VALUE LOW-VALUES.
007500   05  WS-ZBNKRPC1-DATA.
007600     10  WS-ZBNKRPC1-DATA-PT1                PIC X(80).
007700     10  WS-ZBNKRPC1-DATA-PT2                PIC X(80).
007800
007900 01  WS-DATA-REPOSITORY.
008000   05  WS-DATA-ACCESS                        PIC X(3).
008100     88  DATA-ACCESS-DLI                     VALUE 'DLI'.
008200     88  DATA-ACCESS-SQL                     VALUE 'SQL'.
008300     88  DATA-ACCESS-VSM                     VALUE 'VSM'.
008400   05  WS-DATA-ACCESS-SQL-TYPE               PIC X(3).
008500     88  SQL-ACCESS-DB2                      VALUE 'DB2'.
008600     88  SQL-ACCESS-XDB                      VALUE 'XDB'.
008700
008800 01  WS-CONSOLE-MESSAGE                      PIC X(60).
008900
009000 01  WS-EXEC-PARM.
009100   05  WS-EXEC-PARM-LL                       PIC S9(4) COMP.
009200   05  WS-EXEC-PARM-DATA                     PIC X(12).
009300
009400 01  WS-PARM-PTR                             POINTER.
009500 01  WS-PARM-PTR-NUM REDEFINES WS-PARM-PTR   PIC X(4) COMP-5.
009600
009700 01  WS-COMMAREA.
009800 COPY CIOFUNCS.
009900 COPY CBANKD51.
010000 COPY CBANKD52.
010100
010200 COPY CABENDD.
010300
010400 COPY CIMSCONS.
010500
010600 COPY CIMSAIB.
010700
010800 01  WS-ENV-AREA                             PIC X(200).
010900 01  WS-ENV-AREA-R REDEFINES WS-ENV-AREA.
011000   05  WS-ENVIRON-DATA                       PIC X(100).
011100   05  WS-ENV-DATA REDEFINES WS-ENVIRON-DATA.
011200     10  WS-ENV-ID                           PIC X(8).
011300     10  WS-ENV-REL                          PIC X(4).
011400     10  WS-ENV-CTLTYPE                      PIC X(8).
011500     10  WS-ENV-APPTYPE                      PIC X(8).
011600     10  WS-ENV-RGNID                        PIC X(4).
011700     10  WS-ENV-APPNAME                      PIC X(8).
011800     10  WS-ENV-PSBNAME                      PIC X(8).
011900     10  WS-ENV-TRNNAME                      PIC X(8).
012000     10  WS-ENV-UID                          PIC X(8).
012100     10  WS-ENV-GRPNAME                      PIC X(8).
012200     10  WS-ENV-STATUS                       PIC X(4).
012300     10  WS-ENV-RECTOK                       POINTER.
012400     10  WS-ENV-ADDRPRM                      POINTER.
012500     10  WS-ENV-SHRQ                         PIC X(4).
012600     10  WS-ENV-UADS                         PIC X(8).
012700     10  WS-ENV-UIND                         PIC X(4).
012800   05  WS-RECOVER-TOKEN                      PIC X(18).
012900
013000 LINKAGE SECTION.
013100 01  LK-EXEC-PARM.
013200   05  LK-EXEC-PARM-LL                       PIC S9(4) COMP.
013300   05  LK-EXEC-PARM-DATA                     PIC X(32).
013400
013500 PROCEDURE DIVISION USING LK-EXEC-PARM.
013600*****************************************************************
013700* Perform RUN-TIME to initialse time and display start time     *
013800*****************************************************************
013900     PERFORM RUN-TIME.
014000
016800
016900*****************************************************************
017000* EXEC-CARD processing is slightly different from normal MVS    *
017100* processing in that we check the pointer (or address) of the   *
017200* parm area first. This is so that we can migrate it to         *
017300* distributed (Windows/Unix) environment wihout change.         *
017400*****************************************************************
017500     MOVE ZEROES TO WS-EXEC-PARM-LL.
017600     MOVE SPACES TO WS-EXEC-PARM-DATA.
017700
017800     SET WS-PARM-PTR TO ADDRESS OF LK-EXEC-PARM.
017900     IF WS-PARM-PTR-NUM IS NOT EQUAL TO ZEROS
018000        MOVE LK-EXEC-PARM-LL TO WS-EXEC-PARM-LL
018100        IF WS-EXEC-PARM-LL IS GREATER THAN
018200             LENGTH OF WS-EXEC-PARM-DATA
018300           MOVE LENGTH OF WS-EXEC-PARM-DATA TO WS-EXEC-PARM-LL
018400        END-IF
018500        IF WS-EXEC-PARM-LL IS GREATER THAN ZERO
018600           MOVE LK-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)
018700             TO WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)
018800        END-IF
018900     END-IF.
019000
019100     IF WS-EXEC-PARM-LL IS EQUAL TO ZERO
019200        MOVE 'No exec card parm present'
019300          TO WS-CONSOLE-MESSAGE
019400        PERFORM DISPLAY-CONSOLE-MESSAGE
019500        MOVE '  Selecting all records'
019600          TO WS-CONSOLE-MESSAGE
019700        PERFORM DISPLAY-CONSOLE-MESSAGE
019800        MOVE 3 TO WS-EXEC-PARM-LL
019900        MOVE 'ALL' TO WS-EXEC-PARM-DATA
020000     ELSE
020100       MOVE SPACES TO WS-CONSOLE-MESSAGE
020200       STRING 'Exec parm is "' DELIMITED BY SIZE
020300              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)
020400                DELIMITED BY SIZE
020500              '"' DELIMITED BY SIZE
020600         INTO WS-CONSOLE-MESSAGE
020700       PERFORM DISPLAY-CONSOLE-MESSAGE
020800       MOVE SPACES TO WS-CONSOLE-MESSAGE
020900       STRING '  Selecting records for ' DELIMITED BY SIZE
021000              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)
021100                DELIMITED BY SIZE
021200              ' only' DELIMITED BY SIZE
021300         INTO WS-CONSOLE-MESSAGE
021400       PERFORM DISPLAY-CONSOLE-MESSAGE
021500     END-IF.
021600     INSPECT WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)
021700       CONVERTING 'abcdefghijklmnopqrstuvwxyz'
021800               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
021900
022000*****************************************************************
022100* Check to see if we want to demonstrate MFE calling a module   *
022200* that resides on the mainframe.                                *
022300*****************************************************************
022400     IF RPC-REQUESTED
022500        PERFORM RPC-PROCESS
022600     END-IF.
022700
022800*****************************************************************
022900* Open our output file                                          *
023000*****************************************************************
023100     PERFORM EXTRACT-OPEN.
023200
023300*****************************************************************
023400* Open the customer details input then read the data and create *
023500* output records as appropriate.                                *
023600*****************************************************************
023700     PERFORM SOURCE1-OPEN.
023800     PERFORM UNTIL IO-REQUEST-STATUS-EOF
023900       IF NOT IO-REQUEST-STATUS-EOF
024000          PERFORM SOURCE1-READ
024100          IF IO-REQUEST-STATUS-OK
024200             ADD 1 TO WS-RECORD-COUNTER1
024300             IF WS-RECORD-COUNTER1 IS LESS THAN 6
024400                MOVE WS-COMMAREA TO WS-CONSOLE-MESSAGE
024500                PERFORM DISPLAY-CONSOLE-MESSAGE
024600             ELSE
024700                IF WS-RECORD-COUNTER2 IS EQUAL TO 6
024800                   MOVE 'Suppressing record display...'
024900                      TO WS-CONSOLE-MESSAGE
025000                   PERFORM DISPLAY-CONSOLE-MESSAGE
025100                END-IF
025200             END-IF
025300
025400             IF CD51O-PID IS NOT EQUAL TO WS-LAST-PID
025500                MOVE SPACES TO BANKXT01-REC0
025600                MOVE '0' TO BANKXT01-0-TYPE
025700                MOVE CD51O-PID TO BANKXT01-1-PID
025800                MOVE CD51O-NAME TO BANKXT01-0-NAME
025900                MOVE CD51O-EMAIL TO BANKXT01-0-EMAIL
026000                PERFORM EXTRACT-PUT
026100                MOVE SPACES TO BANKXT01-REC1
026200                MOVE '1' TO BANKXT01-1-TYPE
026300                MOVE CD51O-PID TO BANKXT01-1-PID
026400                MOVE CD51O-NAME TO BANKXT01-1-NAME
026500                MOVE CD51O-ADDR1 TO BANKXT01-1-ADDR1
026600                MOVE CD51O-ADDR2 TO BANKXT01-1-ADDR2
026700                MOVE CD51O-STATE TO BANKXT01-1-STATE
026800                MOVE CD51O-CNTRY TO BANKXT01-1-CNTRY
026900                MOVE CD51O-POST-CODE TO BANKXT01-1-PST-CDE
027000                PERFORM EXTRACT-PUT
027100                MOVE CD51O-PID TO WS-LAST-PID
027200             END-IF
027300             MOVE SPACES TO BANKXT01-REC2
027400             MOVE '2' TO BANKXT01-2-TYPE
027500             MOVE CD51O-PID TO BANKXT01-2-PID
027600             MOVE CD51O-ACC-NO TO BANKXT01-2-ACC-NO
027700             MOVE CD51O-ACC-DESC TO BANKXT01-2-ACC-DESC
027800             MOVE CD51O-ACC-CURR-BAL TO BANKXT01-2-ACC-CURR-BAL
027900             MOVE CD51O-ACC-LAST-STMT-DTE
028000               TO BANKXT01-2-ACC-LAST-STMT-DTE
028100             MOVE CD51O-ACC-LAST-STMT-BAL
028200               TO BANKXT01-2-ACC-LAST-STMT-BAL
028300             PERFORM EXTRACT-PUT
028400          END-IF
028500       END-IF
028600     END-PERFORM.
028700     PERFORM SOURCE1-CLOSE.
028800
028900*****************************************************************
029000* Open the transactions details file then read the data and     *
029100* create output records as appropriate.                         *
029200*****************************************************************
029300     PERFORM SOURCE2-OPEN.
029400     PERFORM UNTIL IO-REQUEST-STATUS-EOF
029500       IF NOT IO-REQUEST-STATUS-EOF
029600          PERFORM SOURCE2-READ
029700          IF IO-REQUEST-STATUS-OK
029800             ADD 1 TO WS-RECORD-COUNTER2
029900             IF WS-RECORD-COUNTER2 IS LESS THAN 6
030000                MOVE WS-COMMAREA TO WS-CONSOLE-MESSAGE
030100                PERFORM DISPLAY-CONSOLE-MESSAGE
030200             ELSE
030300                IF WS-RECORD-COUNTER2 IS EQUAL TO 6
030400                   MOVE 'Suppressing record display...'
030500                      TO WS-CONSOLE-MESSAGE
030600                   PERFORM DISPLAY-CONSOLE-MESSAGE
030700                END-IF
030800             END-IF
030900
031000             MOVE SPACES TO BANKXT01-REC3
031100             MOVE '3' TO BANKXT01-3-TYPE
031200             MOVE CD52O-PID TO BANKXT01-3-PID
031300             MOVE CD52O-ACC-NO TO BANKXT01-2-ACC-NO
031400             MOVE CD52O-AMOUNT TO BANKXT01-3-AMOUNT
031500             MOVE CD52O-TIMESTAMP TO BANKXT01-3-TIMESTAMP
031600             MOVE CD52O-DESC TO BANKXT01-3-DESC
031700             PERFORM EXTRACT-PUT
031800          END-IF
031900       END-IF
032000     END-PERFORM.
032100     PERFORM SOURCE2-CLOSE.
032200
032300*****************************************************************
032400* Close our output file                                         *
032500*****************************************************************
032600     PERFORM EXTRACT-CLOSE.
032700
032800*****************************************************************
032900* Display messages to show what we created                      *
033000*****************************************************************
033100     MOVE 'SOURCE data has been extracted'
033200       TO WS-CONSOLE-MESSAGE.
033300     PERFORM DISPLAY-CONSOLE-MESSAGE.
033400     MOVE SPACES TO WS-CONSOLE-MESSAGE.
033500     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE
033600            ' from SOURCE1 (Customer details)'
033700              DELIMITED BY SIZE
033800       INTO WS-CONSOLE-MESSAGE.
033900     PERFORM DISPLAY-CONSOLE-MESSAGE.
034000     MOVE SPACES TO WS-CONSOLE-MESSAGE.
034100     STRING WS-RECORD-COUNTER2 DELIMITED BY SIZE
034200            ' from SOURCE2 (Transactions)'
034300              DELIMITED BY SIZE
034400       INTO WS-CONSOLE-MESSAGE.
034500     PERFORM DISPLAY-CONSOLE-MESSAGE.
034600     MOVE 'End Of Job'
034700       TO WS-CONSOLE-MESSAGE.
034800     PERFORM DISPLAY-CONSOLE-MESSAGE.
034900
035000*****************************************************************
035100* Perform RUN-TIME to calculate run time and display end time   *
035200*****************************************************************
035300     PERFORM RUN-TIME.
035400
035500*****************************************************************
035600* Step return code and return                                   *
035700*****************************************************************
035800     MOVE 0 TO RETURN-CODE.
035900
036000     GOBACK.
036100
036200*****************************************************************
036300* Open the source file                                          *
036400*****************************************************************
036500 SOURCE1-OPEN.
036600     MOVE SPACES TO WS-COMMAREA.
036700     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.
036800     SET IO-REQUEST-FUNCTION-OPEN TO TRUE.
036900     CALL 'DBANK51P' USING WS-COMMAREA.
037000     IF IO-REQUEST-STATUS-OK
037100        MOVE 'SOURCE1 (Customer details) file opened OK'
037200          TO WS-CONSOLE-MESSAGE
037300        PERFORM DISPLAY-CONSOLE-MESSAGE
037400     ELSE
037500        MOVE 'SOURCE1 (Customer details) file open failure...'
037600          TO WS-CONSOLE-MESSAGE
037700        PERFORM DISPLAY-CONSOLE-MESSAGE
037800        PERFORM ABORT-PROGRAM
037900        END-IF.
038000 SOURCE2-OPEN.
038100     MOVE SPACES TO WS-COMMAREA.
038200     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.
038300     SET IO-REQUEST-FUNCTION-OPEN TO TRUE.
038400     CALL 'DBANK52P' USING WS-COMMAREA.
038500     IF IO-REQUEST-STATUS-OK
038600        MOVE 'SOURCE2 (Transactions) file opened OK'
038700          TO WS-CONSOLE-MESSAGE
038800        PERFORM DISPLAY-CONSOLE-MESSAGE
038900     ELSE
039000        MOVE 'SOURCE2 (Transactions) file open failure...'
039100          TO WS-CONSOLE-MESSAGE
039200        PERFORM DISPLAY-CONSOLE-MESSAGE
039300        PERFORM ABORT-PROGRAM
039400        END-IF.
039500
039600*****************************************************************
039700* Read a record from the source file                            *
039800*****************************************************************
039900 SOURCE1-READ.
040000     MOVE SPACES TO WS-COMMAREA.
040100     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.
040200     SET IO-REQUEST-FUNCTION-READ TO TRUE.
040300     CALL 'DBANK51P' USING WS-COMMAREA.
040400     IF IO-REQUEST-STATUS-ERROR
040500        MOVE 'SOURCE1 (Customer details) Error reading file ...'
040600          TO WS-CONSOLE-MESSAGE
040700         PERFORM DISPLAY-CONSOLE-MESSAGE
040800         PERFORM ABORT-PROGRAM
040900     END-IF.
041000 SOURCE2-READ.
041100     MOVE SPACES TO WS-COMMAREA.
041200     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.
041300     SET IO-REQUEST-FUNCTION-READ TO TRUE.
041400     CALL 'DBANK52P' USING WS-COMMAREA.
041500     IF IO-REQUEST-STATUS-ERROR
041600        MOVE 'SOURCE2 (Transactions) Error reading file ...'
041700          TO WS-CONSOLE-MESSAGE
041800         PERFORM DISPLAY-CONSOLE-MESSAGE
041900         PERFORM ABORT-PROGRAM
042000     END-IF.
042100
042200*****************************************************************
042300* Close the source file.                                        *
042400*****************************************************************
042500 SOURCE1-CLOSE.
042600     MOVE SPACES TO WS-COMMAREA.
042700     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.
042800     SET IO-REQUEST-FUNCTION-CLOSE TO TRUE.
042900     CALL 'DBANK51P' USING WS-COMMAREA.
043000     IF IO-REQUEST-STATUS-ERROR
043100        MOVE 'SOURCE1 (Customer details) Error closing file ...'
043200          TO WS-CONSOLE-MESSAGE
043300         PERFORM DISPLAY-CONSOLE-MESSAGE
043400         PERFORM ABORT-PROGRAM
043500     END-IF.
043600 SOURCE2-CLOSE.
043700     MOVE SPACES TO WS-COMMAREA.
043800     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.
043900     SET IO-REQUEST-FUNCTION-CLOSE TO TRUE.
044000     CALL 'DBANK52P' USING WS-COMMAREA.
044100     IF IO-REQUEST-STATUS-ERROR
044200        MOVE 'SOURCE2 (Transactions) Error closing file ...'
044300          TO WS-CONSOLE-MESSAGE
044400         PERFORM DISPLAY-CONSOLE-MESSAGE
044500         PERFORM ABORT-PROGRAM
044600     END-IF.
044700
044800*****************************************************************
044900* Open the seqential extract file as output                     *
045000*****************************************************************
045100 EXTRACT-OPEN.
045200     OPEN OUTPUT EXTRACT-FILE.
045300     IF WS-EXTRACT-STATUS = '00'
045400        MOVE 'EXTRACT file opened OK'
045500          TO WS-CONSOLE-MESSAGE
045600        PERFORM DISPLAY-CONSOLE-MESSAGE
045700     ELSE
045800        MOVE 'EXTRACT file open failure...'
045900          TO WS-CONSOLE-MESSAGE
046000        PERFORM DISPLAY-CONSOLE-MESSAGE
046100        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS
046200        PERFORM DISPLAY-IO-STATUS
046300        PERFORM ABORT-PROGRAM
046400        END-IF.
046500
046600*****************************************************************
046700* Write a record to the squential file                          *
046800*****************************************************************
046900 EXTRACT-PUT.
047000     IF BANKXT01-1-TYPE IS EQUAL TO '0'
047100        WRITE BANKXT01-REC0
047200     END-IF.
047300     IF BANKXT01-1-TYPE IS EQUAL TO '1'
047400        WRITE BANKXT01-REC1
047500     END-IF.
047600     IF BANKXT01-2-TYPE IS EQUAL TO '2'
047700        WRITE BANKXT01-REC2
047800     END-IF.
047900     IF BANKXT01-3-TYPE IS EQUAL TO '3'
048000        WRITE BANKXT01-REC3
048100     END-IF.
048200     IF WS-EXTRACT-STATUS NOT = '00'
048300        MOVE 'EXTRACT Error Writing file ...'
048400          TO WS-CONSOLE-MESSAGE
048500        PERFORM DISPLAY-CONSOLE-MESSAGE
048600        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS
048700        PERFORM DISPLAY-IO-STATUS
048800        PERFORM ABORT-PROGRAM
048900     END-IF.
049000
049100*****************************************************************
049200* Close the seqential extract file                              *
049300*****************************************************************
049400 EXTRACT-CLOSE.
049500     CLOSE EXTRACT-FILE.
049600     IF WS-EXTRACT-STATUS = '00'
049700        MOVE 'EXTRACT file closed OK'
049800          TO WS-CONSOLE-MESSAGE
049900        PERFORM DISPLAY-CONSOLE-MESSAGE
050000     ELSE
050100        MOVE 'EXTRACT file close failure...'
050200          TO WS-CONSOLE-MESSAGE
050300        PERFORM DISPLAY-CONSOLE-MESSAGE
050400        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS
050500        PERFORM DISPLAY-IO-STATUS
050600        PERFORM ABORT-PROGRAM
050700     END-IF.
050800
050900*****************************************************************
051000* Display the file status bytes. This routine will display as   *
051100* two digits if the full two byte file status is numeric. If    *
051200* second byte is non-numeric then it will be treated as a       *
051300* binary number.                                                *
051400*****************************************************************
051500 DISPLAY-IO-STATUS.
051600     IF WS-IO-STATUS NUMERIC
051700        MOVE SPACE TO WS-CONSOLE-MESSAGE
051800        STRING 'File status -' DELIMITED BY SIZE
051900               WS-IO-STATUS DELIMITED BY SIZE
052000          INTO WS-CONSOLE-MESSAGE
052100        PERFORM DISPLAY-CONSOLE-MESSAGE
052200     ELSE
052300        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY
052400        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT
052500        MOVE SPACE TO WS-CONSOLE-MESSAGE
052600        STRING 'File status -' DELIMITED BY SIZE
052700               WS-IO-STAT1 DELIMITED BY SIZE
052800               '/' DELIMITED BY SIZE
052900               WS-TWO-BYTES DELIMITED BY SIZE
053000          INTO WS-CONSOLE-MESSAGE
053100        PERFORM DISPLAY-CONSOLE-MESSAGE
053200     END-IF.
053300
053400*****************************************************************
053500* 'ABORT' the program.                                          *
053600* Post a message to the console and issue a STOP RUN            *
053700*****************************************************************
053800 ABORT-PROGRAM.
053900     IF WS-CONSOLE-MESSAGE NOT = SPACES
054000        PERFORM DISPLAY-CONSOLE-MESSAGE
054100     END-IF.
054200     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.
054300     PERFORM DISPLAY-CONSOLE-MESSAGE.
054400     MOVE 16 TO RETURN-CODE.
054500     GOBACK.
054600
054700*****************************************************************
054800* This process will attempt to call a small module which is     *
054900* meant toreside on th emainframe                               *
055000*****************************************************************
055100 RPC-PROCESS.
055200     MOVE '0' TO WS-ZBNKRPC1-IND.
055300     MOVE LOW-VALUES TO WS-ZBNKRPC1-DATA-PT1.
055400     MOVE HIGH-VALUES TO WS-ZBNKRPC1-DATA-PT2.
055500     MOVE 'ZBNKRPC1' TO WS-ZBNKRPC1-PGM.
055600     CALL WS-ZBNKRPC1-PGM USING WS-ZBNKRPC1-DATA
055700       ON EXCEPTION
055800         MOVE '1' TO WS-ZBNKRPC1-IND
055900     END-CALL.
056000     IF WS-ZBNKRPC1-IND IS EQUAL TO '1'
056100        MOVE 'Call to ZBNKRPC1 failed. Program not found.'
056200          TO WS-CONSOLE-MESSAGE
056300        PERFORM DISPLAY-CONSOLE-MESSAGE
056400     ELSE
056500        IF WS-ZBNKRPC1-DATA-PT1 IS EQUAL TO LOW-VALUES AND
056600           WS-ZBNKRPC1-DATA-PT2 IS EQUAL TO HIGH-VALUES
056700           MOVE 'Call to ZBNKRPC1 was to a stub program.'
056800             TO WS-CONSOLE-MESSAGE
056900           PERFORM DISPLAY-CONSOLE-MESSAGE
057000           MOVE 'Passed data area was unchanged.'
057100             TO WS-CONSOLE-MESSAGE
057200           PERFORM DISPLAY-CONSOLE-MESSAGE
057300        ELSE
057400           MOVE WS-ZBNKRPC1-DATA-PT1 TO WS-CONSOLE-MESSAGE
057500           PERFORM DISPLAY-CONSOLE-MESSAGE
057600           MOVE WS-ZBNKRPC1-DATA-PT2 TO WS-CONSOLE-MESSAGE
057700           PERFORM DISPLAY-CONSOLE-MESSAGE
057800        END-IF
057900     END-IF.
058000
058100*****************************************************************
058200* Display CONSOLE messages...                                   *
058300*****************************************************************
058400 DISPLAY-CONSOLE-MESSAGE.
058500     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE.
058600     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE
058700       UPON CONSOLE.
058800     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.
058900
059000*COPY CTIMERP.
001500 RUN-TIME.
001600     IF TIMER-START IS EQUAL TO ZERO
001700        ACCEPT TIMER-START FROM TIME
001800        MOVE 'Timer started' TO WS-CONSOLE-MESSAGE
001900        PERFORM DISPLAY-CONSOLE-MESSAGE
002000     ELSE
002100        ACCEPT TIMER-END FROM TIME
002200        MOVE 'Timer stopped' TO WS-CONSOLE-MESSAGE
002300        PERFORM DISPLAY-CONSOLE-MESSAGE
002400        COMPUTE TIMER-ELAPSED =
002500                  ((TIMER-END-HH * 60 * 60 * 100) +
002600                   (TIMER-END-MM * 60 * 100) +
002700                   (TIMER-END-SS * 100) +
002800                    TIMER-END-DD) -
002900                  ((TIMER-START-HH * 60 * 60 * 100) +
003000                   (TIMER-START-MM * 60 * 100) +
003100                   (TIMER-START-SS * 100) +
003200                    TIMER-START-DD)
003300        MOVE TIMER-ELAPSED-R TO TIMER-RUN-TIME-ELAPSED
003400        MOVE TIMER-RUN-TIME TO WS-CONSOLE-MESSAGE
003500        PERFORM DISPLAY-CONSOLE-MESSAGE
003600     END-IF.
003700
059100
059200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
