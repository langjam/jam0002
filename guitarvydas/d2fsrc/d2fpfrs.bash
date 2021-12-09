#!/bin/bash
# source file comes from cwd, whereas all others come from bin/d2fapp (.ohm, .glue, support.js)
temp=`which pfrs`
d2fapp=`dirname ${temp}`
grammar=${d2fapp}/$2
glue=${d2fapp}/$3
temp2=${d2fapp}/d2fsupport.js
support=`realpath ${temp2}`
currdir=`pwd`
dir=`dirname ${currdir}`
appdir=`dirname ${support}`
if [ "$1" = "-" ]
then
    arg1="-"
    docker run --rm -i -v "${appdir}/:${appdir}" pt ipfrs ${arg1} ${grammar} ${glue} ${support}
else
    arg1=`realpath $1`
    docker run --rm -v "${dir}/:${dir}" -v "${appdir}:${appdir}" pt ipfrs ${arg1} ${grammar} ${glue} ${support}
fi
