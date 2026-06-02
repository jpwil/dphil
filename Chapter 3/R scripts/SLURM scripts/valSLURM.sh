#!/bin/bash 

# Specify a job name 
#SBATCH -J WITHOUT_PD_20260107

# Account name and target partition 
#SBATCH -A iddo-watson.prj 
#SBATCH -p short

# Log locations which are relative to the current 
# working directory of the submission 

#SBATCH --output=out/%x_%j.out # Standard output
#SBATCH --error=error/%x_%j.err # Standard error

# array settings
#SBATCH --array 1-500:1 
#SBATCH --requeue 

# Parallel environment settings 

#  For more information on these please see the documentation 

#  Allowed parameters: 

#   -c, --cpus-per-task 

#   -N, --nodes 

#   -n, --ntasks 

#SBATCH -c 1 

# Some useful data about the job to help with debugging 

echo "------------------------------------------------" 
echo "Slurm Job ID: $SLURM_JOB_ID" 
echo "Run on host: "`hostname` 
echo "Operating system: "`uname -s` 
echo "Architecture: "`uname -m` 
echo "Username: "`whoami` 
echo "Started at: "`date` 
echo "------------------------------------------------" 

# Array specific data
echo `date`: Executing task ${SLURM_ARRAY_TASK_ID} of job ${SLURM_ARRAY_JOB_ID} on `hostname` as user ${USER} 
echo SLURM_ARRAY_TASK_MIN=${SLURM_ARRAY_TASK_MIN}, SLURM_ARRAY_TASK_MAX=${SLURM_ARRAY_TASK_MAX}, SLURM_ARRAY_TASK_STEP=${SLURM_ARRAY_TASK_STEP} 

# Begin script here   

#Rscript R/valSLURM_WITH_PD.r ${SLURM_ARRAY_TASK_ID} results-share/20250701_WITH_PD_OM.rds
Rscript R/valSLURM_WITHOUT_PD.r ${SLURM_ARRAY_TASK_ID} results-share/20250701_valOrig_withoutPD/originalModel_2.rds

# End of job script 
echo "------------------------------------------------" 
echo "Completed at: "`date` 
echo "------------------------------------------------" 
