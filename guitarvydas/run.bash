clear

lj2=`pwd`
pfr=${lj2}/pfr.bash
d2f=${lj2}/d2f.bash
f2j=${lj2}/f2j.bash

pfrdir=${lj2}/pfrsrc
d2fdir=${lj2}/d2fsrc
f2jdir=${lj2}/f2jsrc

echo transiled files
echo producer.cmm '->' producer.c
${pfr} ${lj2} producer.cmm cmm.ohm cmm.glue ${lj2}/support.js >producer.c
echo consumer.cmm '->' consumer.c
${pfr} ${lj2} consumer.cmm cmm.ohm cmm.glue ${lj2}/support.js >consumer.c
echo os.cmm '->' os.c
${pfr} ${lj2} os.cmm cmm.ohm cmm.glue ${lj2}/support.js >os.c
echo main.cmm '->' main.c
${pfr} ${lj2} main.cmm cmm.ohm cmm.glue ${lj2}/support.js >main.c



# these compile fine, but need to be linked with other files
# echo gcc producer.c
# gcc producer.c
# echo gcc consumer.c
# gcc consumer.c

#echo ATM gcc os.c has compilation errors/warning
# echo gcc os.c
# gcc os.c

#echo ATM gcc main.c has compilation errors/warning
# echo gcc main.c
# gcc main.c

# echo gcc util.c
#echo ATM gcc util.c has compilation errors/warning
# gcc util.c


${d2f} ${lj2} DaisyChain >fb.pl
echo DaisyChain.drawio '->' fb.pl

${f2j} ${f2jdir} <fb.pl >fb.json
echo fb.pl '->' fb.json '(i.e. DaisyChain.drawio -> fb.json)'

