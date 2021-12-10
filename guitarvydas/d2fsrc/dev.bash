fname=tmp_${RANDOM}
./run.bash >${fname}
cat ${fname}
grep contains ${fname}
grep synonym ${fname}
rm -f ${fname}
