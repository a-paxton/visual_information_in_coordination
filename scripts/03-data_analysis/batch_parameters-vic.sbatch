#!/bin/bash

#SBATCH --job-name=vic_param_20230125         # Name of the job.
#SBATCH --output=slurm_output/vic_param_20230125-%A_%a.out       # Provide unique output files for each array task.
#SBATCH --error=slurm_output/vic_param_20230125%A_%a.err        # Provide unique error files for each array task.
#SBATCH --array=1-221                # Provide index values the data in our folder
#SBATCH --ntasks=1                    # We've only got one script we're running.
#SBATCH --time=00:30:00               # Don't let it run forever

# load modules
source /etc/profile.d/modules.sh
module load gcc/11.2.0 r/4.2.2

# save our task ID as an environmental variable
export SLURM_ARRAY_TASK_ID

# print to output for confirmation that it's started
echo $SLURM_ARRAY_TASK_ID ": Running SLURM task"

# run the R script
Rscript $HOME/visual_information_in_coordination/scripts/03-data_processing/crqa_parameter_identification-vic.R

# print to output for confirmation that it's ended
echo $SLURM_ARRAY_TASK_ID ": Job done"