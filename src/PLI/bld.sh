# Cleanup
rm -f *.o 
rm -f ./cicsrdef/*.so 
rm -f *.stb
rm -f *.pp*
export COMPILE_OPTS="-l -isuffix .inc -cics -nolaxdcl -range -deb -defext -not_symbol 0xAC -ipath ./include:$COBDIR/include -langlvl SAA2 -ppcics "
mfplx APCT00.pli  $COMPILE_OPTS -o ./cicsrdef/APCT00.so
