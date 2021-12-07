# pfr producer.cpre cpre.ohm cpre.glue support.js
#pfr consumer.cpre cpre.ohm cpre.glue support.js

#pfr small.txt small.ohm small.glue support.js
# pfr short.cmm short.ohm short.glue support.js
# pfr short.cpre cpre.ohm cpre.glue support.js
# #pfr os.cpre cpre.ohm cpre.glue support.js

cdir=`pwd`
#node parse.js short.cmm short.ohm short.glue ${cdir}/support.js
node parse.js producer.cmm short.ohm short.glue ${cdir}/support.js >producer.c
node parse.js consumer.cmm short.ohm short.glue ${cdir}/support.js >consumer.c
node parse.js os.cmm short.ohm short.glue ${cdir}/support.js

