#!/bin/bash 

# Specify a job name 
#SBATCH -J sumSLURM

# Account name and target partition 
#SBATCH -A iddo-watson.prj 
#SBATCH -p short
  
# Log locations which are relative to the current 
# working directory of the submission 

#SBATCH --output=out/%x_%j.out # Standard output
#SBATCH --error=error/%x_%j.err # Standard error

# Some useful data about the job to help with debugging 

echo "------------------------------------------------" 
echo "Slurm Job ID: $SLURM_JOB_ID" 
echo "Run on host: "`hostname` 
echo "Operating system: "`uname -s` 
echo "Architecture: "`uname -m` 
echo "Username: "`whoami` 
echo "Started at: "`date` 
echo "------------------------------------------------" 

# Begin script here   

# results/20251005_WITH_PD/
# results/20251005_WITHOUT_PD/
# results/20251215_WITH_PD_BOOT_500/
# results/20251215_WITHOUT_PD_BOOT_500/

Rscript R/sumSLURM2.r results/202605_WITHOUT_PD_BOOT/

# End of job script 

echo "------------------------------------------------" 
echo "Completed at: "`date` 
echo "------------------------------------------------" 