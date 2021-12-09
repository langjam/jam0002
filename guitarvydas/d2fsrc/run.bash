#!/bin/bash
# example of how to use d2f
# use d2f instead of run.bash
cp d2f ${HOME}/app/bin
cp d2x ${HOME}/app/bin
d2x helloworld >helloworld.xml
d2f helloworld
