#!/bin/bash
 
#SBATCH --partition=cpu
#SBATCH --qos=cpu
#SBATCH --job-name=DEM_test_paper
#SBATCH --output=%x.o%j
 
##SBATCH --error=%x.e%j  #If error is not specified stderr is redirected to stdout
#SBATCH --nodes=1
#SBATCH --cpus-per-task=24
 
#SBATCH --mem=64G
#SBATCH --time=2-10:30:00 
 
## Uncomment the following line if you want to use an account other than your default account (see hpc-show-user-account)
##SBATCH --account=borisnahuel.rojotanzi@unipr.it 
#SBATCH --mail-user=borisnahuel.rojotanzi@unipr.it --mail-type=FAIL,END
 
##SBATCH --exclusive  # uncomment to require a whole node with at least 28 cores 
 
echo "#SLURM_JOB_NODELIST: $SLURM_JOB_NODELIST"
# Comment out the following line in case of exclusive request 
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
echo "#OMP_NUM_THREADS   : $OMP_NUM_THREADS"
 

module purge
module load intel
module load cmake
module load openmpi3

srun ./DEM > output.log


