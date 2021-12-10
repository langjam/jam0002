clear

lj2=`pwd`
pfr=${lj2}/pfr.bash
d2f=${lj2}/d2f.bash
f2j=${lj2}/f2j.bash

pfrdir=${lj2}/pfrsrc
d2fdir=${lj2}/d2fsrc
f2jdir=${lj2}/f2jsrc

${pfr} ${lj2} producer.cmm cmm.ohm cmm.glue ${lj2}/support.js >producer.c
${pfr} ${lj2} consumer.cmm cmm.ohm cmm.glue ${lj2}/support.js >consumer.c
${pfr} ${lj2} os.cmm cmm.ohm cmm.glue ${lj2}/support.js >os.c
${pfr} ${lj2} main.cmm cmm.ohm cmm.glue ${lj2}/support.js >main.c

echo transiled files
echo producer.cmm '->' producer.c
echo consumer.cmm '->' consumer.c
echo os.cmm '->' os.c
echo main.cmm '->' main.c


# echo gcc producer.c
# gcc os.c
# echo gcc consumer.c
# gcc consumer.c
# echo gcc os.c
# gcc producer.c
echo gcc main.c
gcc main.c
# echo gcc util.c
# gcc util.c


${d2f} ${lj2} DaisyChain >fb.pl
echo DaisyChain.drawio '->' fb.pl

${f2j} ${f2jdir} <fb.pl >fb.json
echo fb.pl '->' fb.json '(i.e. DaisyChain.drawio -> fb.json)'

