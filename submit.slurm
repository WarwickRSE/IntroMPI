#!/bin/bash

#A submission script for the SCRTP cluster using a special partition
#This is a set of reserved resources for the duration of the workshop
#After the workshop, remove the "-p rsetraining" line to run in the normal way

#Give job a meaningful name
#SBATCH --job-name=mpitest
#16 tasks per node and one node
#We will start 16 MPI ranks, one per task
#SBATCH --ntasks-per-node=16
#SBATCH --nodes=1
#Maximum available memory per process
#SBATCH --mem-per-cpu=3882
#Ten minutes wall time
#SBATCH --time=00:10:00
#SBATCH -p rsetraining

#Load the Intel MPI module
module load intel impi

#Run the code
#Srun will start the 16 tasks by default
srun ./a.out

