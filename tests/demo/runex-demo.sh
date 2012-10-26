#!/bin/sh

solve=../../src/algorithm/solvex

runex () {
  n=${REPEAT:-64}
  B=${BUDGET:-1.0}
  prb=$1
  obj=$2
  alg=$3
  cst=$4

  log=${prb}.${obj}.B${B}.c${cst}.${alg}.log
  mv $log ${log}.bak
  i=0
  while [ $i -ne $n ]
  do 
    $solve -stat -i 32 -j 32 -a $alg -c $cst -s -d -B $B ${prb}.p ${obj}.s \
    | grep -e '^solution:\|user' >> $log
    i=$((i+1))
  done
}

REPEAT=1
for alg in Blinkered; do
  for vcost in 0.04; do
    runex svm svmguide2 $alg $vcost
    runex obmark_free ackley $alg $vcost
  done
done
for alg in Myopic; do
  for vcost in 0.04; do
    BUDGET=3 runex wafer 156_130 $alg $vcost
  done
done
