# #!/bin/bash
# # convert a .drawio diagram into a factbase
lj2=$1
d2fdir=${lj2}/d2fsrc
pfr=${lj2}/pfr.bash
${pfr} ${lj2} $2.drawio $d2fdir/drawio.ohm $d2fdir/drawio.glue $d2fdir/support.js \
    | ${pfr} ${lj2} - $d2fdir/diagram.ohm $d2fdir/diagram.glue $d2fdir/support.js \
    | ${pfr} ${lj2} - $d2fdir/styleexpander.ohm $d2fdir/styleexpander.glue $d2fdir/support.js \
    | ${pfr} ${lj2} - $d2fdir/factbase.ohm $d2fdir/factbase.glue $d2fdir/support.js \
    | sed -E -e 's/</\n</g' \
    | sort \
    | sed -E -e '/^[ \t]*$/d'


