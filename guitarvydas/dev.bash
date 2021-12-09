clear
lj2=`pwd`
d2f=${lj2}/d2f.bash
pfr=${lj2}/pfr.bash
d2fdir=${lj2}/d2fsrc
set -x
${lj2}/pfr.bash ${lj2} producer.cmm cmm.ohm cmm.glue ${lj2}/support.js >producer.c
${lj2}/pfr.bash ${lj2} DaisyChain.drawio $d2fdir/drawio.ohm $d2fdir/drawio.glue $d2fdir/support.js
${pfr} ${lj2} DaisyChain.drawio $d2fdir/drawio.ohm $d2fdir/drawio.glue $d2fdir/support.js
${d2f} ${lj2} DaisyChain


