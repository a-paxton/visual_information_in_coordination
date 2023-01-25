# coding: utf-8

# # Visual Information in Coordination: Video processing

# This script is adapted from analyses from Paxton et al. (2018,
# Cognitive Science Society), based on Paxton & Dale
# (2013, Behavior Research Methods).

# ***

# ## Basics

# load in the libraries we need
import os, glob, cv2
import numpy as np
import pandas as pd

# read in the environmental variable
task_number = int(float(os.environ["SLURM_ARRAY_TASK_ID"])) - 1
print('Task number: '+os.environ["SLURM_ARRAY_TASK_ID"])
print('Python received: File number '+str(task_number))

# move into our data folder
os.chdir('data')

# figure out what our next file is
video_files = glob.glob('/home/aep17003/visual_information_in_coordination/videos-raw/*.wmv')
next_video_file = video_files[task_number]
print(next_video_file+ ': Video file processing')

# grab our conversation identifier
conv_id = os.path.basename(next_video_file) # movie
conv = os.path.splitext(conv_id)[0]

# ***

# ## Basic frame differencing

# Uses bilateral filtering from OpenCV to help control noise:
# https://docs.opencv.org/3.1.0/d4/d13/tutorial_py_filtering.html

# open the video capture
video_capture = cv2.VideoCapture(next_video_file)

# read in the first frame
return_frame_value, frame1 = video_capture.read()

# establish the first frame as the first PREVIOUS frame
previous_frame = cv2.cvtColor(frame1,cv2.COLOR_BGR2GRAY)
previous_frame = cv2.bilateralFilter(previous_frame,9,75,75)

# get image dimensions
image_dimensions = previous_frame.shape
single_person_height = image_dimensions[0]
single_person_width = round(image_dimensions[1]/3)
# middle_gap = image_dimensions[1] - (single_person_width*2)

# outline the two participants' spaces
person_left_x_start = 0 + round(image_dimensions[1]*.09)
person_left_x_end = person_left_x_start + single_person_width
person_right_x_start = image_dimensions[1] - round(image_dimensions[1]*.09) - single_person_width
person_right_x_end = image_dimensions[1] - round(image_dimensions[1]*.09)

# specify left participant's matrix
person_left_rows = np.array(range(single_person_height))
person_left_columns = np.array(range(person_left_x_start,
                                     person_left_x_end))

# specify right participant's matrix
person_right_rows = np.array(range(single_person_height))
person_right_columns = np.array(range(person_right_x_start,
                                      person_right_x_end))

# reset difference counter and dataframes
difference_number = 0;
dyad_differenced_dataframe = pd.DataFrame()
participant_differenced_dataframe = pd.DataFrame()

# loop through frames
n = 0
while(n < 100):

    # read in next frame
    n += 1
    return_frame_value, frame2 = video_capture.read()

    # read it in if there's something left to read
    if (return_frame_value == True):
        next_frame = cv2.cvtColor(frame2,cv2.COLOR_BGR2GRAY)
        next_frame = cv2.bilateralFilter(next_frame,9,75,75)

        # take the absolute difference of the pixel displacement between the frames
        differenced_frame = np.matrix(cv2.absdiff(next_frame, previous_frame))

        # save entire frame's columnwise mean
        whole_frame_movement = differenced_frame.mean(axis=1)

        # carve up left and right matrices
        person_left_matrix = differenced_frame[person_left_rows[:, None],
                                               person_left_columns]
        person_right_matrix = differenced_frame[person_right_rows[:, None],
                                                person_right_columns]

        # get average movement for each person
        person_left_movement = person_left_matrix.mean()
        person_right_movement = person_right_matrix.mean()

        # increment differenced frame number
        difference_number = difference_number + 1

        # add the current data to the dataframe
        dyad_differenced_dataframe = pd.concat([dyad_differenced_dataframe,
                pd.Series(difference_number)
                            .append(pd.Series(np.squeeze(np.asarray(whole_frame_movement.flatten()))))
                            .reset_index(drop=True)])
        participant_differenced_dataframe = pd.concat([participant_differenced_dataframe,
                pd.Series(difference_number)
                            .append(pd.Series('left'))
                            .append(pd.Series(person_left_movement))
                            .reset_index(drop=True)])
        participant_differenced_dataframe = pd.concat([participant_differenced_dataframe,
                pd.Series(difference_number)
                            .append(pd.Series('right'))
                            .append(pd.Series(person_right_movement))
                            .reset_index(drop=True)])
    else:
        break

# close out the video capture when done
video_capture.release()
# cv2.destroyAllWindows()

# rename columns
#dyad_differenced_dataframe.columns = ['difference_number', ['frame_col'+str(i) for i in range(image_dimensions[0])]]
#participant_differenced_dataframe.columns = ['difference_number','participant','movement']

# add the dyad name to the dataframes
#dyad_differenced_dataframe['dyad'] = conv
#participant_differenced_dataframe['dyad'] = conv

# write to files
dyad_differenced_dataframe.to_csv('~/visual_information_in_coordination/data/movement_dataframes-column_differences/movement_data-'+conv+'-raw_dyad.csv',
                                 sep=',',index=False, header=True)
participant_differenced_dataframe.to_csv('~/visual_information_in_coordination/data/movement_dataframes-aggregated/movement_data-'+conv+'-participant.csv',
                                 sep=',',index=False, header=True)

# let us know what's up
print(next_video_file+ ': Data saved to file')
