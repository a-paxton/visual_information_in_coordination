# coding: utf-8

# # Visual Information in Coordination: Video processing

# This script extracts basic movement from frame-difference measures.

# ***

# ## Basics

# load in the libraries we need
import os, glob, cv2
import numpy as np
import pandas as pd

# move into our data folder
os.chdir('data')

# figure out what our next file is (glob hat tip to https://stackoverflow.com/a/26403164)
next_video_file = '/home/aep17003/visual_information_in_coordination/data/videos/1104_ZT_2_Coop_Video.mp4'

# grab our conversation identifier
conv_id = os.path.basename(next_video_file) # movie
conv = os.path.splitext(conv_id)[0]
print(conv + ': Video file processing')

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

# specify height (without bottom edge because of timers)
person_height_start = 0
person_height_stop = 980

# specify width based on Zoom or not
person_left_x_start = 0
if "_Z" in conv: 
    person_left_x_end = 950
    person_right_x_start = 970
else:
    person_left_x_end = 700
    person_right_x_start = 1250
person_right_x_end = 1920
    
# reset difference counter and dataframes
difference_number = 0;
dyad_differenced_dataframe = pd.DataFrame()
participant_differenced_dataframe = pd.DataFrame()

# loop through frames (if frame exists)
return_frame_value, frame2 = video_capture.read()
while return_frame_value:
    
    # read it in if there's something left to read
    if (return_frame_value == True):
        next_frame = cv2.cvtColor(frame2,cv2.COLOR_BGR2GRAY)
        next_frame = cv2.bilateralFilter(next_frame,9,75,75)

        # take the absolute difference of the pixel displacement between the frames
        differenced_frame = np.matrix(cv2.absdiff(next_frame, previous_frame))

        # save entire frame's columnwise mean
        whole_frame_matrix = differenced_frame[person_height_start:person_height_stop,
                                               :]
        whole_frame_movement = whole_frame_matrix.mean(axis=1)

        # carve up left and right matrices
        person_left_matrix = differenced_frame[person_height_start:person_height_stop,
                                               person_left_x_start:person_left_x_end]
        person_right_matrix = differenced_frame[person_height_start:person_height_stop,
                                                person_right_x_start:person_right_x_end]

        # get average movement for each person
        person_left_movement = person_left_matrix.mean()
        person_right_movement = person_right_matrix.mean()

        # increment differenced frame number
        difference_number = difference_number + 1

        # add the current data to the dataframe
        values = pd.Series(np.squeeze(np.asarray(whole_frame_movement.flatten())))        
        values_varname = 'diff_' + str(difference_number)
        dyad_differenced_dataframe = pd.concat([dyad_differenced_dataframe, 
                                                pd.DataFrame({values_varname: values})], 
                                               axis = 1)
        participant_differenced_dataframe = pd.concat([participant_differenced_dataframe,
                                                       pd.DataFrame({'difference_number': [difference_number],
                                                                     'participant': ['left'],
                                                                     'movement': [person_left_movement]}),
                                                      pd.DataFrame({'difference_number': [difference_number],
                                                                     'participant': ['right'],
                                                                     'movement': [person_right_movement]})])
        
        # set the next frame to be the previous one and check if there's another
        previous_frame = next_frame
        return_frame_value, frame2 = video_capture.read()

# # loop through frames -- debugging only
# n = 0
# while(n < 100):

#     # read in next frame
#     n += 1
#     return_frame_value, frame2 = video_capture.read()

#     # read it in if there's something left to read
#     if (return_frame_value == True):
#         next_frame = cv2.cvtColor(frame2,cv2.COLOR_BGR2GRAY)
#         next_frame = cv2.bilateralFilter(next_frame,9,75,75)

#         # take the absolute difference of the pixel displacement between the frames
#         differenced_frame = np.matrix(cv2.absdiff(next_frame, previous_frame))

#         # save entire frame's columnwise mean
#         whole_frame_matrix = differenced_frame[person_height_start:person_height_stop,
#                                                :]
#         whole_frame_movement = whole_frame_matrix.mean(axis=1)

#         # carve up left and right matrices
#         person_left_matrix = differenced_frame[person_height_start:person_height_stop,
#                                                person_left_x_start:person_left_x_end]
#         person_right_matrix = differenced_frame[person_height_start:person_height_stop,
#                                                 person_right_x_start:person_right_x_end]

#         # get average movement for each person
#         person_left_movement = person_left_matrix.mean()
#         person_right_movement = person_right_matrix.mean()

#         # increment differenced frame number
#         difference_number = difference_number + 1

#         # add the current data to the dataframe
#         values = pd.Series(np.squeeze(np.asarray(whole_frame_movement.flatten())))        
#         values_varname = 'diff_' + str(difference_number)
#         dyad_differenced_dataframe = pd.concat([dyad_differenced_dataframe, 
#                                                 pd.DataFrame({values_varname: values})], 
#                                                axis = 1)
#         participant_differenced_dataframe = pd.concat([participant_differenced_dataframe,
#                                                        pd.DataFrame({'difference_number': [difference_number],
#                                                                      'participant': ['left'],
#                                                                      'movement': [person_left_movement]})]).reset_index(drop=True)
#         participant_differenced_dataframe = pd.concat([participant_differenced_dataframe,
#                                                        pd.DataFrame({'difference_number': [difference_number],
#                                                                      'participant': ['right'],
#                                                                      'movement': [person_right_movement]})]).reset_index(drop=True)
        
#         # set the next frame to be the previous one
#         previous_frame = next_frame
        
#     else:
#         break

# close out the video capture when done
video_capture.release()
# cv2.destroyAllWindows()

# rename columns for dyad-level dataframe
dyad_differenced_dataframe = dyad_differenced_dataframe.transpose().reset_index()
dyad_differenced_dataframe.columns = ['difference_number'] + ['frame_col'+str(i) for i in range(np.size(whole_frame_movement))]

# add the dyad name to the dataframes
dyad_differenced_dataframe['dyad'] = conv
participant_differenced_dataframe['dyad'] = conv
participant_differenced_dataframe = participant_differenced_dataframe.reset_index(drop = True)

# write to files
dyad_differenced_dataframe.to_csv('./movement_dataframes-column_differences/movement_data-'+conv+'-raw_dyad.csv',
                                 sep=',',index=False, header=True)
participant_differenced_dataframe.to_csv('./movement_dataframes-aggregated/movement_data-'+conv+'-participant.csv',
                                 sep=',',index=False, header=True)

# let us know what's up
print(conv+ ': Data saved to file')
