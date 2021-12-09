# $1 is the root directory for langjam0002
# $2 is the source file
# $3 is the grammar file
# $4 is the glue file
# $5 is the support.js file
lj2=$1
node ${lj2}/pfrsrc/pfr.js $2 $3 $4 $5
