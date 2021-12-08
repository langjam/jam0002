# pfr producer.cpre cpre.ohm cpre.glue support.js
#pfr consumer.cpre cpre.ohm cpre.glue support.js

#pfr small.txt small.ohm small.glue support.js
# pfr short.cmm short.ohm short.glue support.js
# pfr short.cpre cpre.ohm cpre.glue support.js
# #pfr os.cpre cpre.ohm cpre.glue support.js

clear
cdir=`pwd`
#node parse.js cmm.cmm cmm.ohm cmm.glue ${cdir}/support.js
node pfr.js producer.cmm cmm.ohm cmm.glue ${cdir}/support.js >producer.c
node pfr.js consumer.cmm cmm.ohm cmm.glue ${cdir}/support.js >consumer.c
node pfr.js os.cmm cmm.ohm cmm.glue ${cdir}/support.js >os.c
node pfr.js main.cmm cmm.ohm cmm.glue ${cdir}/support.js >main.c

# echo gcc producer.c
# gcc producer.c
# echo gcc consumer.c
# gcc consumer.c
echo gcc os.c
gcc os.c
