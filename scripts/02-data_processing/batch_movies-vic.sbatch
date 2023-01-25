#!/bin/bash

#SBATCH --job-name=vic_fdm_20220123         # Name of the job.
#SBATCH --output=slurm_output/vic_fdm-20220123-%A_%a.out       # Provide unique output files for each array task.
#SBATCH --error=slurm_output/vic_fdm-20220123-%A_%a.err        # Provide unique error files for each array task.
#SBATCH --array=1-3                # Provide index values the data in our folder
#SBATCH --ntasks=1                    # We've only got one script we're running.
#SBATCH --time=01:00:00               # Don't let it run forever

# # install what we need -- only run once, before everything else
# module load python39
# pip install opencv-python
# pip install pandas

# save our task ID as an environmental variable
export SLURM_ARRAY_TASK_ID

# print to output for confirmation that it's started
echo $SLURM_ARRAY_TASK_ID ": Running SLURM task"

# run the process
python ~/visual_information_in_coordination/scripts/02-data_processing/process_one_movie.py $SLURM_ARRAY_TASK_ID

# print to output for confirmation that it's ended
echo $SLURM_ARRAY_TASK_ID ": Job done"
