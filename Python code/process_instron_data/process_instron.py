#!/usr/bin/env python
'''
Script for processing instron data
*Data should be placed in a folder called "data" in the same directory as this script

Ultimate strength

Outputs .csv file with only the best (most consecutive pts) trial for each plant/root

Author: Adam Stager
Contact: astager@udel.edu
'''

import os
import csv
import numpy as np
import matplotlib.pyplot as plt
import math
import sys
import time
from scipy import stats


class InstronData:
    def __init__(self, data_path):
        self.data_path = data_path
        self.data_row = 10          # data begins on this row from raw instron .csv **********changed from 8**********
        self.specimen_note_row = 4
        self.deflection_col = 1        # raw_data column for extension in (mm)
        self.load_col = 2          # raw_data column containing load data (N)
        self.curr_file = None
        self.bs_x_value = None


    def extract_files(self):
        files = []  # extract file paths from all folders in the data directory
        path = self.data_path
        for r, d, f in os.walk(path):  # r=root, d=directories, f = files
            for file in f:
                if '.csv' in file:
                    files.append(os.path.join(r, file))#attaches all files together

        files.sort()
        print(files)
        return files


    def process_files(self, files, draw_plots):
        self.save_trial_data('filename','Row', 'WR', 'Rep', 'Sample','Error Note', 'Ultimate Load', 'Breaking Strength')
        for filename in files:
            raw_data, metadata = self.load_data(filename)
            self.display_file_info(filename, metadata)
            self.curr_file = filename
            m, c, ul, bs = self.get_info(raw_data)
            self.get_slope(raw_data)
            try:
                sample = metadata[3].split('_')[3]
            except:
                sample = 'Sample number not found'
            rep = metadata[2].split('.')[0]
            self.save_trial_data(filename, metadata[0], metadata[1], rep, sample, m, ul, bs)
            if draw_plots:
                self.plot_trials(raw_data)
                #self.plot_line_fit(m,c) # blue is ransac fit to longest consecutive trial
                plt.show()

    def get_info(self, data):
        bs_found = False
        data_len = len(data)
        x = np.zeros(data_len)
        y = np.zeros(data_len)
        x_linear = np.zeros(data_len)
        y_linear = np.zeros(data_len)
        for i in range(data_len):
            x[i] = data[i][1]  # extension (m)
            y[i] = data[i][2]  # load (N)
            if i > 5 and not bs_found:
                dy0 = float(data[i][2]) - float(data[i-1][2])
                dy1 = float(data[i-1][2]) - float(data[i-2][2])
                dy2 = float(data[i-2][2]) - float(data[i-3][2])
                dy3 = float(data[i-3][2]) - float(data[i-4][2])
                dy4 = float(data[i-4][2]) - float(data[i-5][2])
                if dy0+dy1+dy2+dy3+dy4 < -1: #-0.1
                    plt.plot(float(data[i-5][1]), float(data[i-5][2]), 'xr')
                    self.bs_x_val = data[i-5][1]
                    bs = data[i-5][2]
                    bs_found = True

        #for i in range(np.argmax(y)):
        #    x_linear[i] = data[i][1]  # extension (m)
        #    y_linear[i] = data[i][2]  # load (N)
        ul = max(y)

        plt.plot(x[y.argmax()], max(y),'xg')
        #try:
        #slope, intercept, r_value, p_value, std_err = stats.linregress(x_linear,y_linear)
        slope = 0
        intercept = 0
        try:
            if float(self.bs_x_val) < float(x[y.argmax()]):
                print('~~~~~~~~~~ERR: BS OCCURS BEFORE UL~~~~~~~~~~~~~')
                print('BS_x: ' + str(self.bs_x_val) + ', UL_x: ' + str(x[y.argmax()]))
                slope = 'BS BEFORE UL'
                intercept = self.curr_file
            if float(bs) > float(ul):
                print('~~~~~~~~~~ERR: UL less than BS~~~~~~~~~~~~~')
                print('BS_y: ' + str(bs) + ', UL_y: ' + str(ul))
                slope = 'BS > UL'
                intercept = self.curr_file
            print(slope,ul,bs)
            return slope, intercept, ul, bs
        except:
            print('no data in this row - setting slope and intercept as zero')
            return 'BAD DATA', 0, 0, 0


    def get_slope(self, data):
        data_len = len(data)
        x = np.zeros(data_len)
        y = np.zeros(data_len)
        for i in range(data_len):
            x[i] = data[i][1]  # extension (m)
            y[i] = data[i][2]  # load (N)

        # Find the slope by finding the mode of all slopes before the ultimate load
        slopes = np.zeros(y.argmax())
        curr_slope = 0
        consec_pts = 0
        best_consec_pts = 0
        for i in range(data_len):
            if i > 1 and i < y.argmax():
                slope = (float(data[i][2]) - float(data[i-1][2]))/(float(data[i][1]) - float(data[i-1][1]))  # get slope
                if abs(slope - curr_slope) > 0.1:
                    if consec_pts > best_consec_pts:
                        best_consec_pts = consec_pts
                        best_slope = curr_slope
                        print(best_consec_pts)
                    curr_slope = slope
                    consec_pts = 0
                else:
                    consec_pts = consec_pts + 1


        self.plot_line_fit(slope,0)
        print(slopes)




    def load_data(self, filename):
        """
        Load raw data from instron .csv file

        :param filename (string) - PATH to data or just filenamee.csv if in same directory
        :return raw_data (list) - len(raw_data) rows
                                  rows format [time, angle_pot, angle_imu, load_x, load_y]
        :return metadata (list) - [operator, plot, plant, whirl]
        """
        # Initialize and fill raw_data array
        raw_data = []
        with open(filename) as csv_file:
            print('Opening ' + str(filename))
            csv_reader = csv.reader(csv_file, delimiter=',')
            line_count = 0
            for row in csv_reader:
                if line_count >= self.data_row:
                    raw_data.append(row)
                elif line_count == self.specimen_note_row:
                    try:
                        specimen_note = row[1]
                        specimen = specimen_note.splitlines()[1]
                    except:
                        specimen = row[1]
                line_count += 1
            print(specimen_note)
            split_filename = filename.split('/')
            split_filename_info = split_filename[6].split('_')
            row = split_filename_info[0]  # plot is folder name
            whorl= split_filename_info[1]
            root = split_filename_info[2]

        #print(raw_data)
        return raw_data, [row, whorl, root, specimen]


    def display_file_info(self, filename, metadata):
        print('\n')
        print('Processing: ' + str(filename))
        print('##### ' + 'VegState: ' + str(metadata[0])+ ' |' + ' Plant: ' + str(metadata[1])+ ' |' \
                       + ' Whorl: ' + str(metadata[2])+ ' |'+ ' SpecimenNote: ' + str(metadata[3]))


    def save_trial_data(self, filename, row, wrl, rep, sample, error, ul, bs):
        ''''Filename, VegState, Plant, Whorl, SpecimenNote, ultimate load, break strength'''
        data_buffer = str(filename) + ',' +str(row) + ',' + str(wrl) + ',' + str(rep) + ',' + str(sample)+',' + str(error)+',' + str(ul)+',' + str(bs) + '\n'
        data_csv.write(data_buffer)


    def plot_line_fit(self, slope, intercept):
        """Plot a line from slope and intercept"""
        axes = plt.gca()
        x_vals = np.array(axes.get_xlim())
        y_vals = intercept + slope * x_vals
        plt.plot(x_vals, y_vals, '--')

    def plot_trials(self, data):
        num_data = len(data)

        # Plot all data
        for i in range(num_data):
            plt.plot(float(data[i][1]), float(data[i][2]), 'ok', alpha = 0.01)



def start_data_collection():
    ### Open file for writing ###
    global data_csv
    currentDate = time.strftime('%m_%d_%y__%H_%M_%S')
    data_csv = open("/Users/Sparks Lab/Downloads/process_instron.py/data_%s.csv" %currentDate,"w") #OG: "/home/seeterra/Desktop/sparks_instron/data_%s.csv"


############# BEGIN MAIN ###############
if __name__ == "__main__":

    draw_plots = True       # check every plot by setting True, run everything fast with False
    start_data_collection()

    instron_data = InstronData('/Users/Sparks Lab/Downloads/process_instron.py/data') #'/Users/Sparks Lab/Downloads/process_instron.py/data' OG: '/home/seeterra/Desktop/sparks_instron/data'
    curr_files = instron_data.extract_files()
    instron_data.process_files(curr_files, draw_plots)
