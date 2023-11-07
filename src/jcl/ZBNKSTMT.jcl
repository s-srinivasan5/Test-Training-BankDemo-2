//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,MSGCLASS=A,MSGLEVEL=(2,0),       00000100
//  NOTIFY=MFIDEMO                                                      00000200
//EXTRACT  EXEC YBNKEXTV,REQUEST=B0004                                  00000300
//*XTRACT.SYSOUT DD DUMMY                                               00000400
//SORT     EXEC YBNKSRT1,GEN='+1'                                       00000500
//*ORT.SYSOUT DD DUMMY                                                  00000600
//PRINT    EXEC YBNKPRT1,GEN='+1',SOUT=A                                00000700
//*                                                                     00000800
//* *** $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     00000900
