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
001300* Program:     SBANK00P.CBL (CICS Version)                      *
001400* Layer:       Screen handling                                  *
001500* Function:    Screen handling control module                   *
001600*****************************************************************
001700
001800 IDENTIFICATION DIVISION.
001900 PROGRAM-ID.
002000     SBANK00P.
002100 DATE-WRITTEN.
002200     September 2002.
002300 DATE-COMPILED.
002400     Today.
002500
002600 ENVIRONMENT DIVISION.
002700
002800 DATA DIVISION.
002900 WORKING-STORAGE SECTION.
003000 01  WS-MISC-STORAGE.
003100   05  WS-PROGRAM-ID                         PIC X(8)
003200       VALUE 'SBANK00P'.
003300   05  WS-TRAN-ID                            PIC X(4).
003400   05  WS-SCREEN-LOGIC-PGM                   PIC X(8)
003500       VALUE SPACES.
003600   05  WS-DYNAMIC-PGM                        PIC X(8)
003700       VALUE 'UNKNOWN'.
003800   05  WS-ABSTIME                            PIC S9(15) COMP-3.
003900   05  WS-RESP                               PIC S9(8) COMP.
004000   05  WS-INPUT-SOURCE-MSG.
004100     10  FILLER                              PIC X(20)
004200         VALUE 'Input received from '.
004300     10  WS-INPUT-SOURCE-MSG-CALL-TYPE       PIC X(8).
004400 01  WS-BANK-DATA-AREAS.
004500   05  WS-BANK-DATA.
004600 COPY CBANKDAT.
004700   05  WS-BANK-EXT-DATA.
004800 COPY CBANKEXT.
004900
005000 01  TS-DATA.
005100   05  TS-QUEUE-NAME                         PIC X(8).
005200   05  TS-QUEUE-NAME-PARTS REDEFINES TS-QUEUE-NAME.
005300     10  TS-QUEUE-NAME-PART1                 PIC X(4).
005400     10  TS-QUEUE-NAME-PART2                 PIC 9(4).
005500   05  TS-QUEUE-LEN                          PIC S9(4) COMP.
005600   05  TS-QUEUE-ITEM                         PIC S9(4) COMP.
005700   05  TS-QUEUE-DATA                         PIC X(6144).
005800
005900 COPY DFHAID.
006000
006100 COPY DFHBMSCA.
006200
006300 COPY CABENDD.
006400
006500 01  load-ptr pointer.
006600
006700
006800 LINKAGE SECTION.
006900 01  DFHCOMMAREA.
007000   05  LK-TS-QUEUE-NAME                      PIC X(8).
007100   05  LK-CALL-TYPE                          PIC X(8).
007200     88  CALL-TYPE-CICSECI                   VALUE 'CICSECI'.
007300     88  CALL-TYPE-WEBSERV                   VALUE 'WEBSERV'.
007400   05  LK-PASSED-DATA                        PIC X(1024).
007500*COPY CBANKEXT.
007600
007700 PROCEDURE DIVISION.
007800*****************************************************************
007900* Write entry to log to show we have been invoked               *
008000*****************************************************************
008100*     COPY CTRACE.
000100*****************************************************************
000200*                                                               *
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     *
000400*                                                               *
000500*****************************************************************
000600
000700*****************************************************************
000800* CTRACE.CPY                                                    *
000900*---------------------------------------------------------------*
001000* This copybook is used to provide an a trace of what           *
001100* transactions have been run so we get an idea of activity      *
001200* There are different versions for CICS and IMS.                *
001300*****************************************************************
001400*
001500* Comment out the instructions and recompile to not use the trace
001600     EXEC CICS LINK PROGRAM('STRAC00P')
001700                    COMMAREA(WS-PROGRAM-ID)
001800                    LENGTH(LENGTH OF WS-PROGRAM-ID)
001900    END-EXEC.
002000
002100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
       
008200
008300*****************************************************************
008400* Store our transaction-id                                      *
008500*****************************************************************
008600     MOVE EIBTRNID TO WS-TRAN-ID.
008700
008800*****************************************************************
008900* If we have a commarea then its either not the first time in   *
009000* from a terminal or we have come from other than a terminal so *
009100* display the call type so we know where we came from           *
009200*****************************************************************
009300     IF EIBCALEN IS NOT LESS THAN 8
009400        IF CALL-TYPE-CICSECI OR
009500           CALL-TYPE-WEBSERV
009600           MOVE LK-CALL-TYPE TO WS-INPUT-SOURCE-MSG-CALL-TYPE
009700*          EXEC CICS WRITE OPERATOR
009800*                    TEXT(WS-INPUT-SOURCE-MSG)
009900*                    TEXTLENGTH(LENGTH OF WS-INPUT-SOURCE-MSG)
010000*          END-EXEC
010100         END-IF
010200      END-IF.
010300
010400*****************************************************************
010500* If this is the first time in, then we assume we are running   *
010600* from a CICS terminal so we display map BANK10M and return with*
010700* with our COMMAREA set up.                                     *
010800*****************************************************************
010900     IF (EIBCALEN IS EQUAL TO 0) OR
011000        (EIBCALEN IS NOT EQUAL TO 0 AND
011100         LK-TS-QUEUE-NAME IS EQUAL TO 'INET****')
011200        MOVE LOW-VALUES TO WS-BANK-DATA-AREAS
011300        IF EIBCALEN IS EQUAL TO 0
011400           SET BANK-ENV-CICS TO TRUE
011500           EXEC CICS RETRIEVE
011600                     INTO(TS-DATA)
011700                     LENGTH(LENGTH OF TS-DATA)
011800                     RESP(WS-RESP)
011900           END-EXEC
012000           IF TS-DATA(1:7) IS EQUAL TO 'COLOUR='
012100              MOVE TS-DATA(8:1) TO BANK-COLOUR-SETTING
012200           END-IF
012300        ELSE
012400           SET BANK-ENV-INET TO TRUE
012500        END-IF
012600        SET BANK-NO-CONV-IN-PROGRESS TO TRUE
012700        MOVE SPACES TO BANK-LAST-MAPSET
012800        MOVE SPACES TO BANK-LAST-MAP
012900        MOVE SPACES TO BANK-LAST-PROG
013000        MOVE SPACES TO BANK-NEXT-PROG
013100        MOVE WS-TRAN-ID TO BANK-CICS-TRANCODE
013200        EXEC CICS ASKTIME
013300                  ABSTIME(WS-ABSTIME)
013400        END-EXEC
013500        MOVE BANK-ENV TO TS-QUEUE-NAME-PART1
013600*       MOVE WS-ABSTIME TO TS-QUEUE-NAME-PART2
013601        MOVE EIBTASKN   TO TS-QUEUE-NAME-PART2
013700        EXEC CICS DELETEQ TS
013800                  QUEUE(TS-QUEUE-NAME)
013900                  RESP(WS-RESP)
014000        END-EXEC
014100        MOVE SPACES TO TS-QUEUE-DATA
014200        MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN
014300        MOVE 0 TO TS-QUEUE-ITEM
014400        MOVE 0 TO WS-RESP
014500        EXEC CICS WRITEQ TS
014600                  QUEUE(TS-QUEUE-NAME)
014700                  FROM(TS-QUEUE-DATA)
014800                  LENGTH(TS-QUEUE-LEN)
014900                  ITEM(TS-QUEUE-ITEM)
015000                  RESP(WS-RESP)
015100        END-EXEC
015200        exec cics write operator
015300                        text(ts-queue-name)
015400                        textlength(length of ts-queue-name)
015500        end-exec
015600
015700        IF BANK-ENV-INET
015800           MOVE TS-QUEUE-NAME TO LK-TS-QUEUE-NAME
015900        END-IF
016000     ELSE
016100        MOVE LOW-VALUES TO WS-BANK-DATA
016200        MOVE LK-TS-QUEUE-NAME TO TS-QUEUE-NAME
016300        MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN
016400        MOVE 1 TO TS-QUEUE-ITEM
016500        EXEC CICS READQ TS
016600                  QUEUE(TS-QUEUE-NAME)
016700                  INTO(TS-QUEUE-DATA)
016800                  LENGTH(TS-QUEUE-LEN)
016900                  ITEM(TS-QUEUE-ITEM)
017000        END-EXEC
017100        MOVE TS-QUEUE-DATA TO WS-BANK-DATA
017200        IF BANK-ENV-INET
017300           MOVE LK-PASSED-DATA (1:EIBCALEN) TO WS-BANK-EXT-DATA
017400           IF CALL-TYPE-WEBSERV
017500              INSPECT WS-BANK-EXT-DATA REPLACING ALL '~' BY
017600                LOW-VALUES
017700           END-IF
017800        END-IF
017900     END-IF.
018000
018100*****************************************************************
018200* If we get this far then this is not the first time in as we   *
018300* have a COMMAREA. Check that BANK-ENV is set correctly to      *
018400* ensure we are running in the correct environment etc          *
018500*****************************************************************
018600     IF NOT BANK-ENV-CICS AND
018700        NOT BANK-ENV-INET
018800        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT
018900        MOVE 'S001' TO ABEND-CODE
019000        MOVE 'Inavlid environment' TO ABEND-REASON
019100*        COPY CABENDPO.
              PERFORM ZZ-ABEND 
019200     END-IF.
019300
019400*****************************************************************
019500* This is the main process                                      *
019600*****************************************************************
019700
019800*****************************************************************
019900* Map the AID in the EIB to our common area                     *
020000*****************************************************************
020100     IF BANK-ENV-INET
020200        MOVE EXT-IP-AID TO BANK-AID
020300     ELSE
020400        EVALUATE TRUE
020500          WHEN EIBAID IS EQUAL TO DFHENTER
020600            SET BANK-AID-ENTER TO TRUE
020700          WHEN EIBAID IS EQUAL TO DFHCLEAR
020800            SET BANK-AID-CLEAR TO TRUE
020900          WHEN EIBAID IS EQUAL TO DFHPA1
021000            SET BANK-AID-PA1   TO TRUE
021100          WHEN EIBAID IS EQUAL TO DFHPA2
021200            SET BANK-AID-PA2   TO TRUE
021300          WHEN EIBAID IS EQUAL TO DFHPF1
021400            SET BANK-AID-PFK01 TO TRUE
021500          WHEN EIBAID IS EQUAL TO DFHPF2
021600            SET BANK-AID-PFK02 TO TRUE
021700          WHEN EIBAID IS EQUAL TO DFHPF3
021800            SET BANK-AID-PFK03 TO TRUE
021900          WHEN EIBAID IS EQUAL TO DFHPF4
022000            SET BANK-AID-PFK04 TO TRUE
022100          WHEN EIBAID IS EQUAL TO DFHPF5
022200            SET BANK-AID-PFK05 TO TRUE
022300          WHEN EIBAID IS EQUAL TO DFHPF6
022400            SET BANK-AID-PFK06 TO TRUE
022500          WHEN EIBAID IS EQUAL TO DFHPF7
022600            SET BANK-AID-PFK07 TO TRUE
022700          WHEN EIBAID IS EQUAL TO DFHPF8
022800            SET BANK-AID-PFK08 TO TRUE
022900          WHEN EIBAID IS EQUAL TO DFHPF9
023000            SET BANK-AID-PFK09 TO TRUE
023100          WHEN EIBAID IS EQUAL TO DFHPF10
023200            SET BANK-AID-PFK10 TO TRUE
023300          WHEN EIBAID IS EQUAL TO DFHPF11
023400            SET BANK-AID-PFK11 TO TRUE
023500          WHEN EIBAID IS EQUAL TO DFHPF12
023600            SET BANK-AID-PFK12 TO TRUE
023700          WHEN EIBAID IS EQUAL TO DFHPF13
023800            SET BANK-AID-PFK01 TO TRUE
023900          WHEN EIBAID IS EQUAL TO DFHPF14
024000           SET BANK-AID-PFK02 TO TRUE
024100          WHEN EIBAID IS EQUAL TO DFHPF15
024200            SET BANK-AID-PFK03 TO TRUE
024300          WHEN EIBAID IS EQUAL TO DFHPF16
024400            SET BANK-AID-PFK04 TO TRUE
024500          WHEN EIBAID IS EQUAL TO DFHPF17
024600            SET BANK-AID-PFK05 TO TRUE
024700          WHEN EIBAID IS EQUAL TO DFHPF18
024800            SET BANK-AID-PFK06 TO TRUE
024900          WHEN EIBAID IS EQUAL TO DFHPF19
025000            SET BANK-AID-PFK07 TO TRUE
025100          WHEN EIBAID IS EQUAL TO DFHPF20
025200            SET BANK-AID-PFK08 TO TRUE
025300          WHEN EIBAID IS EQUAL TO DFHPF21
025400            SET BANK-AID-PFK09 TO TRUE
025500          WHEN EIBAID IS EQUAL TO DFHPF22
025600            SET BANK-AID-PFK10 TO TRUE
025700          WHEN EIBAID IS EQUAL TO DFHPF23
025800            SET BANK-AID-PFK11 TO TRUE
025900          WHEN EIBAID IS EQUAL TO DFHPF24
026000            SET BANK-AID-PFK12 TO TRUE
026100          WHEN OTHER
026200            SET BANK-AID-ENTER TO TRUE
026300        END-EVALUATE
026400     END-IF.
026500
026600*****************************************************************
026700* Check the AID to see if we have to toggle the colour setting  *
026800*****************************************************************
026900     IF BANK-AID-PFK02
027000        SET BANK-AID-ENTER TO TRUE
027100        IF COLOUR-ON
027200           SET COLOUR-OFF TO TRUE
027300        ELSE
027400           SET COLOUR-ON TO TRUE
027500        END-IF
027600     END-IF.
027700
027800*****************************************************************
027900* If the BANK-NEXT-PROG is not the same as BANK-LAST-PROG then  *
028000* we have to go to the next program                             *
028100*****************************************************************
028200 CHECK-PROGRAM-SWITCH.
028300     IF BANK-NEXT-PROG IS NOT EQUAL TO BANK-LAST-PROG
028400        EXEC CICS LINK PROGRAM(BANK-NEXT-PROG)
028500                       COMMAREA(WS-BANK-DATA-AREAS)
028600                       LENGTH(LENGTH OF WS-BANK-DATA-AREAS)
028700        END-EXEC
028800        GO TO CHECK-PROGRAM-SWITCH
028900     END-IF.
029000
029100*****************************************************************
029200* We determine what the last screen displayed was and call the  *
029300* the appropriate routine to handle it.                         *
029400*****************************************************************
029500     EVALUATE TRUE
029600       WHEN BANK-LAST-MAPSET IS EQUAL TO SPACES
029700         MOVE 'SBANK10P' TO WS-SCREEN-LOGIC-PGM
029800       WHEN OTHER
029900         STRING 'SBANK' DELIMITED BY SIZE
030000                BANK-LAST-MAPSET(6:2) DELIMITED BY SIZE
030100                'P' DELIMITED BY SIZE
030200           INTO WS-SCREEN-LOGIC-PGM
030300     END-EVALUATE.
030400     SET BANK-MAP-FUNCTION-GET TO TRUE.
030500     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)
030600                    COMMAREA(WS-BANK-DATA-AREAS)
030700                    LENGTH(LENGTH OF WS-BANK-DATA-AREAS)
030800     END-EXEC.
030900
031000*****************************************************************
031100* Now we have to see what is required from the business logic   *
031200* Essentially the choices will be switch to another program     *
031300* (which will be in BANK-NEXT-PROG) or display thge next screen *
031400* (which will be in BANK-NEXT-MAPSET/BANK-NEXT-MAP)             *
031500*****************************************************************
031600* Check for a program switch first
031700 CHECK-FOR-PGM-SWITCH.
031800     IF BANK-NEXT-PROG IS NOT EQUAL TO BANK-LAST-PROG
031900        EXEC CICS LINK PROGRAM(BANK-NEXT-PROG)
032000                       COMMAREA(WS-BANK-DATA-AREAS)
032100                       LENGTH(LENGTH OF WS-BANK-DATA-AREAS)
032200        END-EXEC
032300        GO TO CHECK-FOR-PGM-SWITCH
032400     END-IF.
032500
032600*****************************************************************
032700* We determine which screen we have to display and call the     *
032800* appropriate routine to handle it.                             *
032900*****************************************************************
033000*    MOVE LOW-VALUE TO MAPAREA.
033100     STRING 'SBANK' DELIMITED BY SIZE
033200             BANK-NEXT-MAPSET(6:2) DELIMITED BY SIZE
033300            'P' DELIMITED BY SIZE
033400        INTO WS-SCREEN-LOGIC-PGM.
033500     SET BANK-MAP-FUNCTION-PUT TO TRUE.
033600     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)
033700                    COMMAREA(WS-BANK-DATA-AREAS)
033800                    LENGTH(LENGTH OF WS-BANK-DATA-AREAS)
033900     END-EXEC.
034000
034100*****************************************************************
034200* Now we have to have finished and can return to our invoker.   *
034300* Before retuning, we write out any data we wish to preserve    *
034400* to TS. So we can retrieve this data we keep the TS queue id   *
034500*****************************************************************
034600* Now return to CICS
034700     MOVE WS-BANK-DATA TO TS-QUEUE-DATA.
034800     MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN.
034900     MOVE 1 TO TS-QUEUE-ITEM.
035000     MOVE 0 TO WS-RESP.
035100     EXEC CICS WRITEQ TS
035200               QUEUE(TS-QUEUE-NAME)
035300               FROM(TS-QUEUE-DATA)
035400               LENGTH(TS-QUEUE-LEN)
035500               ITEM(TS-QUEUE-ITEM)
035600               REWRITE
035700               RESP(WS-RESP)
035800     END-EXEC.
035900
036000     IF BANK-ENV-INET
036100        IF CALL-TYPE-WEBSERV
036200           INSPECT WS-BANK-EXT-DATA REPLACING ALL LOW-VALUES BY
036300             '~'
036400        END-IF
036500        MOVE WS-BANK-EXT-DATA TO LK-PASSED-DATA
036600     END-IF.
036700
036800     IF BANK-CICS-TRANCODE IS EQUAL TO SPACES OR
036900        BANK-ENV-INET
037000        EXEC CICS RETURN
037100        END-EXEC
037200     ELSE
037300        EXEC CICS RETURN
037400                  TRANSID(BANK-CICS-TRANCODE)
037500                  COMMAREA(TS-QUEUE-NAME)
037600                  LENGTH(LENGTH OF TS-QUEUE-NAME)
037700        END-EXEC
037800     END-IF.
037900     GOBACK.
038000
       ZZ-ABEND SECTION.
001600    
           STRING ABEND-CULPRIT DELIMITED BY SIZE
001700            ' Abend ' DELIMITED BY SIZE
001800            ABEND-CODE DELIMITED BY SIZE
001900            ' - ' DELIMITED BY SIZE
002000             ABEND-REASON DELIMITED BY SIZE
002100         INTO ABEND-MSG
           END-STRING.
002200     
           EXEC CICS WRITE
002300               OPERATOR
002400               TEXT(ABEND-MSG)
002500               TEXTLENGTH(LENGTH OF ABEND-MSG)
002600     END-EXEC.
002700         
           EXEC CICS ABEND
002800           ABCODE(ABEND-CODE)
002900     END-EXEC.
       
038100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
