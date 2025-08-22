#!/bin/bash

# JX - check the missed ensemble members

# default to check the forecast???.nc
Iorder=0
if [ $# -eq 1 ] ; then
   # default to check the forecast???.nc
   Ntot=$1
   Fixpre='forecast'
   Fixsuf='.nc'
elif [ $# -eq 3 ]; then
   Fixpre=$2
   Fixsuf=$3
elif [ $# -gt 3 ]; then
   Fixpre=$2
   Fixsuf=$3
   Iorder=$4
else
   echo "Usage: ~  ensemble-number <forecast  .nc>"
   tellerror "Need input ensemble number < or fixpre and fixsuf form for the filename>"
   exit 1
fi

Fout=Checkmem_${Fixpre}_${Iorder}.log
[ -s ${Fout} ] && rm ${Fout}

echo "Output the results into " ${Fout} " ..."

fsize=0
for ii in `seq 1 ${Ntot}`; do
    cmem=`echo 00${ii} | tail -4c`
    Fnam=${Fixpre}${cmem}${Fixsuf}
    if [ -s ${Fnam} ]; then
       #tmpsize=`stat -c %s ${Fnam}`
       tmpsize=$(stat -c%s ${Fnam})
       if [ "$tmpsize" -gt "$fsize" ]; then
          fsize=${tmpsize}
       fi
    fi
done
if [ $fsize -eq 0 ]; then
   echo "missing all the files: " ${Fixpre}???${Fixsuf}"!"
   echo "">${Fout}
   exit 0
fi
# recording the missing numbers
Smem=""
imiss=0

for ii in `seq 1 ${Ntot}`; do
   cmem=`echo 00${ii} | tail -4c`
   Fnam=${Fixpre}${cmem}${Fixsuf}
   if [ ! -s ${Fnam} ]; then
      (( imiss = imiss + 1 ))
      if [ ${imiss} -eq 1 ]; then
         Smem=$(echo ${cmem}) 
      else
         Smem=$(echo ${Smem} ${cmem})
      fi
   else
      tmpsize=$(stat -c%s ${Fnam})
      if [ "$tmpsize" -lt "$fsize" ]; then
         (( imiss = imiss + 1 ))
         if [ ${imiss} -eq 1 ]; then
            Smem=$(echo ${cmem}) 
         else
            Smem=$(echo ${Smem} ${cmem})
         fi
      fi 
   fi 
done 
echo ${Smem} > ${Fout}
