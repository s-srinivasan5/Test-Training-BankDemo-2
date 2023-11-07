@echo off

rem
rem  Copyright (C) 2011-2011 Micro Focus IP Development Limited.
rem  All rights reserved.
rem
rem  The software and information contained herein are proprietary to,
rem  and comprise valuable trade secrets of, Micro Focus IP
rem  Development Limited, which intends to preserve as trade secrets
rem  such software and  information. This software is an unpublished
rem  copyright of Micro Focus and may not be used, copied,
rem  transmitted, or stored in any manner. This software and
rem  information or any other copies thereof may not be provided or
rem  otherwise made available to any other person.
rem

REM Cleanup
del *.obj /q
del .\cicsrdef\*.dll /q
del *.stb /q
del *.pp* /q
del *.lib /q
del *.def /q

rem compile_opts= -cics -l -isuffix .inc -nolaxdcl -f w -range -deb -defext -not_symbol 0xAC -optexec plitest -ipath .\include -langlvl SAA2 -ppcics
set compile_opts= -cics -l -isuffix .inc -nolaxdcl      -range -deb -defext -not_symbol 0xAC -optexec plitest -ipath .\include -langlvl SAA2 -ppcics

mfplx APCT00.pli -dll %compile_opts% -out:.\cicsrdef\APCT00.dll
if not "%ERRORLEVEL%" == "0" goto error
mfplx APCT01.pli -dll %compile_opts% -out:.\cicsrdef\APCT01.dll
if not "%ERRORLEVEL%" == "0" goto error
mfplx APCT02.pli -dll %compile_opts% -out:.\cicsrdef\APCT02.dll
if not "%ERRORLEVEL%" == "0" goto error
mfplx APCT03.pli -dll %compile_opts% -out:.\cicsrdef\APCT03.dll
if not "%ERRORLEVEL%" == "0" goto error
mfplx APCT04.pli -dll %compile_opts% -out:.\cicsrdef\APCT04.dll
if not "%ERRORLEVEL%" == "0" goto error

goto good_end

REM - Following command line will only work on Windows (probably don't need to compile bms on all platforms?)

REM mfbmscl ocmcs.bms /binary=c:/APCT/

:error
echo -
echo -
echo --------------------------------------
echo bld failed
echo --------------------------------------
:good_end
