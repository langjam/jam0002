clear
lj2=`pwd`
${lj2}/pfr.bash ${lj2} producer.cmm cmm.ohm cmm.glue ${lj2}/support.js >producer.c
${lj2}/pfr.bash ${lj2} consumer.cmm cmm.ohm cmm.glue ${lj2}/support.js >consumer.c
${lj2}/pfr.bash ${lj2} os.cmm cmm.ohm cmm.glue ${lj2}/support.js >os.c
${lj2}/pfr.bash ${lj2} main.cmm cmm.ohm cmm.glue ${lj2}/support.js >main.c

echo transiled files
echo producer.cmm '->' producer.c
echo consumer.cmm '->' consumer.c
echo os.cmm '->' os.c
echo main.cmm '->' main.c


# compiling os.c results in compilation errors
# echo gcc producer.c
# gcc os.c
# echo gcc consumer.c
# gcc consumer.c
# echo gcc os.c
# gcc producer.c
# echo gcc main.c
# gcc main.c



