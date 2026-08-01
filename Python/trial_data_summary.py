# -*- coding: utf-8 -*-
"""
Spyder Editor

""" 

from pathlib import Path
import pandas as pd

# change the path as necessary, using the following paths:
    
    # motor1_data/######_motor1
    # motor2_data/######_motor2
    # back_dual_data/######_back_dual
    # hand_dual_data/######_hand_dual

path_dir = Path('/Users/aatkin/Documents/Tactile_Suppression_Study/hand_dual_data/271309_hand_dual')

list_dfs = []

# remove the strength, duration, and vibrotactileStimTime columns for the motor1 and motor2 files
# 'strength', 'duration', 'vibrotactileStimTime',

for path_file in path_dir.glob('trial*'):
    df_small = pd.read_csv(path_file,
        usecols=['trial_idx', 'strength', 'duration', 'vibrotactileStimTime', 'startx_cm', 'starty_cm', 'targetx_cm', 'targety_cm', 'targetSize', 'distance', 'response', 'target_RT', 'movement_time', 'startToTarget_time', 'prompt_RT', 'enterTargetPosX', 'enterTargetPosY', 'ballistic_x_error', 'ballistic_y_error', 'endpoint_x_error', 'endpoint_y_error'],
        ).tail(1)
    list_dfs.append(df_small)
    
df = pd.concat(list_dfs, axis=0) 

# df.sort_values(by='trial_idx', ascending=True).reset_index(drop=True)
df.sort_values(by='trial_idx', ascending=False, inplace=True)

# rename the newly-generated summary file as necessary:
    
    # ######_motor1.csv
    # ######_motor1.csv
    # ######_back_dual.csv
    # ######_hand_dual.csv

df.to_csv('/Users/aatkin/Desktop/hand_dual.csv', index=False)

print(df)
