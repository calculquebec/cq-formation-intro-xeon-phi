#!/bin/bash
#PBS -l nodes=1:ppn=16:mics=2
#PBS -l walltime=0:10:00
#PBS -o miccheck.out
#PBS -e miccheck.err
#PBS -V
#PBS -N schedtest


cd $PBS_O_WORKDIR

echo "Job started on `hostname` at `date`"
module add ifort_icc/15.0
miccheck
