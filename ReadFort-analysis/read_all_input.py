import os
import glob
import math
import datetime
import csv
import shutil
import subprocess
import pandas as pd
import matplotlib.pyplot as plt
from tqdm import tqdm

# Current directory
current_directory = os.getcwd()
files = os.scandir(current_directory)
fold_num = sum([os.path.isdir(listx) for listx in os.listdir("./")])
file_num = sum([os.path.isfile(listx) for listx in os.listdir("./")])
tot_num = file_num + fold_num

version = 'VENUS 96'
versiondate = datetime.date(2023, 12, 9)

def displaywelcome():
    string = '\n'
    string += '  ' + '=' * 80 + '\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '||' + '{:^80}'.format('VENUS ICS output generator') + '||\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '||' + '{:^80}'.format('CQU: Jiawei Yang, Jun Li*') + '||\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '||' + '{:^80}'.format('Version:' + version) + '||\n'
    string += '||' + '{:^80}'.format(versiondate.strftime("%d.%m.%y")) + '||\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '  ' + '=' * 80 + '\n\n'
    string += '''
  This script can batch process input files with different initial conditions. 
  There are 4 functions: 
  1. Use "ICS = pi * bmax^2 * P" to calculate ICS values; 
  2. Processing style=2 to calculate the reaction probability of different bmax; 
  3. Calculate the statistical error of the dynamic results under each initial 
     condition; 
  4. Statistical internal energy and total energy of diatomic molecules under 
     different initial conditions.
  '''
    print(string)

# Define a function to copy the readfort999.f file to the desired folder
def copy_readfort_file(source_file_path, target_folder):
    target_file_path = os.path.join(target_folder, 'readfort999.f')
    if os.path.exists(target_file_path):
        os.remove(target_file_path)

    shutil.copy(source_file_path, target_folder)
    # shell
    command_1 = f"gfortran -o readfort999 readfort999.f"
    command_2 = f"./readfort999"
    subprocess.run(command_1, cwd=target_folder, shell=True)
    subprocess.run(command_2, cwd=target_folder, shell=True)

# Iterate over files and folders in the current directory
for entry in tqdm(files, total=tot_num, desc="Processing Folders"):
    if entry.is_dir():
        folder_name = entry.name
        if "-inpark" in folder_name:
            source_file_path = os.path.join(current_directory, 'readfort999.f')
            copy_readfort_file(source_file_path, entry.path)
        else:
            # Iterate through file again to see if any of the folders contain "inpark".
            for root, dirs, files in os.walk(entry.path):
                for folder in dirs:
                    if "-inpark" in folder:
                        folder_path = os.path.join(root, folder)
                        source_file_path = os.path.join(current_directory, 'readfort999.f')
                        copy_readfort_file(source_file_path, folder_path)

displaywelcome()
style = int(input('''\nType of task:
1.calculate ICS
2.style2 bmax
3.statistical-error (SE)
4.Enj and Etot

Please enter the number corresponding to the type of calculation: '''))

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#style = 1
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

if style == 1:
    test_ICS = input('Whether to calculate ICS (y/n): ')
    if test_ICS == 'n':
        print('Good-bye!')
    elif test_ICS == 'y':
        nre = str(input('reaction path (2+3/4): '))
        lines_dict = {}
        et_ics_dict = {}

        # two new list
        array_values = []
        values = []

        def traverse_folders(folder_path, folder_name):  
            for entry in os.scandir(folder_path):
                if entry.is_dir():
                    # If it is judged to be a folder, the function is called recursively
                    traverse_folders(entry.path, folder_name)
                else:
                    # If it's a file, check if the filename contains "energy.convergence-0.001"
                    if "energy.convergence-0.001" in entry.name:
                        with open(entry.path, 'r') as file:
                            for line in file:
                                if 'total path 2' in line:
                                    re2 = int(line.split()[-1])
                                elif 'non-converged path 2' in line:
                                    ncre2 = int(line.split()[-1])
                                elif 'total path 3' in line:
                                    re3 = int(line.split()[-1])
                                elif 'non-converged path 3' in line:
                                    ncre3 = int(line.split()[-1])
                                elif 'total path 4' in line:
                                    re4 = int(line.split()[-1])
                                elif 'non-converged path 4' in line:
                                    ncre4 = int(line.split()[-1])
                                elif 'sum of converged traj' in line:
                                    tot = int(line.split()[-1])
                            re2 = re2 - ncre2
                            re3 = re3 - ncre3
                            re4 = re4 - ncre4
                            if nre == '2+3':
                                p = (re2+re3)/tot
                            elif nre == '4':
                                p = re4/tot
                            lines_dict[folder_name] = p

        # Define the root folder path
        root_folder = '.'

        # Iterate through folders containing "GP-11AP-O3"
        for folder_path in glob.glob(os.path.join(root_folder, 'input_*')):
            traverse_folders(folder_path, folder_path)

        # Sort the dictionary by the numerical value of the key
        sorted_dict = dict(sorted(lines_dict.items()))

        # Read the fourth column of the file "array-bmax.txt"
        with open('array-bmax.txt', 'r') as array_file:
            Et_d, v_d, j_d, array_data = [], [], [], []
            for line in array_file:
                columns = line.split()
                Et_d.append(float(columns[0]))
                v_d.append(int(columns[1]))
                j_d.append(int(columns[2]))
                array_data.append(float(columns[3]))

        # output write to 'ICS-output.csv'
        output_file = 'ICS-output.csv'
        with open(output_file, 'w', newline='') as f:
            csv_writer = csv.writer(f)
            csv_writer.writerow(['Folder Name', 'Et', 'v', 'j', 'ICS'])
            for (key, value), Et, v, j, array_value in zip(sorted_dict.items(), Et_d, v_d, j_d, array_data):
                array_values.append(float(array_value))
                values.append(float(value))
                ICS = format(math.pi*(float(array_value)**2)*float(value), ".3f")
                csv_writer.writerow([key, Et, v, j, ICS])
                et_ics_dict[float(Et)] = float(ICS)

        # Sort by Et
        sorted_et_ics = dict(sorted(et_ics_dict.items()))
        sorted_Et = list(sorted_et_ics.keys())
        sorted_ICS = list(sorted_et_ics.values())

        # plot
        plt.figure(figsize=(5, 3))
        plt.plot(sorted_Et, sorted_ICS, marker='o', linestyle='-')
        plt.xlabel('$\mathregular{E_t}$ (kcal/mol)')
        plt.ylabel('ICS ($\mathregular{Å^2}$)')
        plt.title('Plot of ICS')
        plt.grid(True)
        plt.savefig('ICS-output.png', dpi=600, bbox_inches='tight')
        print(output_file, " has been generated!")

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#style = 2
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

elif style == 2:
    test_bmax = input('Whether to calculate P(b) of each bmax (y/n): ')
    if test_bmax == 'n':
        print('Good-bye!')
    elif test_bmax == 'y':
        nre = str(input('reaction path (2+3/4): '))
        b_test_dict = {}

        # two new list
        array_values = []
        values = []

        # Define functions for traversing folders
        def traverse_folders(folder_path, folder_name):
            # Iterate through all files and folders in the current folder
            for entry in os.scandir(folder_path):
                if entry.is_dir():
                    traverse_folders(entry.path, folder_name)
                else:
                    if "energy.convergence-0.001" in entry.name:
                        with open(entry.path, 'r') as file:
                            for line in file:
                                if 'total path 2' in line:
                                    re2 = int(line.split()[-1])
                                elif 'non-converged path 2' in line:
                                    ncre2 = int(line.split()[-1])
                                elif 'total path 3' in line:
                                    re3 = int(line.split()[-1])
                                elif 'non-converged path 3' in line:
                                    ncre3 = int(line.split()[-1])
                                elif 'total path 4' in line:
                                    re4 = int(line.split()[-1])
                                elif 'non-converged path 4' in line:
                                    ncre4 = int(line.split()[-1])
                                elif 'sum of converged traj' in line:
                                    tot = int(line.split()[-1])
                            re2 = re2 - ncre2
                            re3 = re3 - ncre3
                            re4 = re4 - ncre4
                            if nre == '2+3':
                                p = (re2+re3)/tot
                            elif nre == '4':
                                p = re4/tot
                            b_test_dict[folder_name] = p

        # Define the root folder path
        root_folder = '.'

        # Iterate through folders containing "GP-11AP-O3"
        for folder_path in glob.glob(os.path.join(root_folder, 'input_*')):
            traverse_folders(folder_path, folder_path)

        # Sort the dictionary by the numerical value of the key
        sorted_dict = dict(sorted(b_test_dict.items()))

        # Read the fourth column of the file "array-bmax.txt"
        with open('array-bmax.txt', 'r') as array_file:
            array_data = [line.split()[3] for line in array_file]

        # output write to 'bmax-output.csv'
        output_file = 'bmax-output.csv'
        with open(output_file, 'w', newline='') as f:
            csv_writer = csv.writer(f)
            csv_writer.writerow(['Folder Name', 'array_value', 'value'])
            for (key, value), array_value in zip(sorted_dict.items(), array_data):
                csv_writer.writerow([key, array_value, value])
                array_values.append(float(array_value))
                values.append(float(value))

        # plot
        plt.figure(figsize=(5, 3))
        plt.plot(array_values, values, marker='o', linestyle='-')
        plt.xlabel('${b}$ (Å)')
        plt.ylabel('${P_b}$')
        plt.title('Plot of ${b}$$_{max}$ test')
        plt.grid(True)
        plt.savefig('bmax-output.png', dpi=600, bbox_inches='tight')

        print(output_file, " has been generated!")

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#style = 3
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

elif style == 3:
    test_se = input('Whether to calculate SE (y/n): ')
    if test_se == 'n':
        print('Good-bye!')
    elif test_se == 'y':
        nre = str(input('reaction path (2+3/4): '))
        # new dict
        SE_dict = {}
        tot_dict = {}
        re4_dict = {}

        # new list
        array_values = []
        values = []

        # Define functions for traversing folders
        def traverse_folders(folder_path, folder_name):
            for entry in os.scandir(folder_path):
                if entry.is_dir():
                    traverse_folders(entry.path, folder_name)
                else:
                    if "energy.convergence-0.001" in entry.name:
                        with open(entry.path, 'r') as file:
                            for line in file:
                                if 'total path 2' in line:
                                    re2 = int(line.split()[-1])
                                elif 'non-converged path 2' in line:
                                    ncre2 = int(line.split()[-1])
                                elif 'total path 3' in line:
                                    re3 = int(line.split()[-1])
                                elif 'non-converged path 3' in line:
                                    ncre3 = int(line.split()[-1])
                                elif 'total path 4' in line:
                                    re4 = int(line.split()[-1])
                                elif 'non-converged path 4' in line:
                                    ncre4 = int(line.split()[-1])
                                elif 'sum of converged traj' in line:
                                    tot = int(line.split()[-1])
                            re2 = re2 - ncre2
                            re3 = re3 - ncre3
                            re4 = re4 - ncre4
                            if nre == '2+3':
                                SE = format(((tot-(re2+re3))/(tot*(re2+re3)))**0.5, ".2f")
                            elif nre == '4':
                                SE = format(((tot-re4)/(tot*re4))**0.5, ".2f")
                            SE_dict[folder_name] = float(SE)
                            tot_dict[folder_name] = tot
                            re4_dict[folder_name] = re4

        # Define the root folder path
        root_folder = '.'

        # Iterate through folders containing "GP-11AP-O3"
        for folder_path in glob.glob(os.path.join(root_folder, 'input_*')):
            traverse_folders(folder_path, folder_path)

        # Sort the dictionary by the numerical value of the key
        sorted_SE_dict = dict(sorted(SE_dict.items()))
        sorted_tot_dict = dict(sorted(tot_dict.items()))
        sorted_re4_dict = dict(sorted(re4_dict.items()))
        sorted_file = list(sorted_SE_dict.keys())
        sorted_SE = list(sorted_SE_dict.values())
        sorted_tot = list(sorted_tot_dict.values())
        sorted_re4 = list(sorted_re4_dict.values())

        # output write to 'SE-output.csv'
        output_file = 'SE-output.csv'

        with open(output_file, 'w', newline='') as csvfile:
            fieldnames = ['File', 'tot', 're4', 'SE']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            for key, value, tot, re4 in zip(sorted_file, sorted_SE, sorted_tot, sorted_re4):
                writer.writerow({'File':key, 'tot':tot, 're4':re4, 'SE':value})

        num = sum(i > 0.05 for i in sorted_SE)

        print("The number of initial conditions with a statistical error > 0.05 is", num)
        print(output_file, " has been generated!")

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#style = 4
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

elif style == 4:
    get_E_nj = input('Whether to get Enj and Etot (y/n): ')
    if get_E_nj == 'n':
        print('Good-bye!')
    elif get_E_nj == 'y':
        lines_dict = {}
        sorted_dict = {}

        # new list
        array_values = []
        values = []

        def traverse_folders(folder_path, folder_name):
            for entry in os.scandir(folder_path):
                if entry.is_dir():
                    traverse_folders(entry.path, folder_name)
                else:
                    # If it's one file, check if it has "AND ENERGY" in its name
                    if "out0001" in entry.name:
                        with open(entry.path, 'r') as file:
                            for line in file:
                                if 'AND ENERGY' in line:
                                    E_nj = float(line.split()[-2])
                            lines_dict[folder_path] = E_nj
        # Define the root folder path
        root_folder = '.'

        # Iterate through folders containing "GP-11AP-O3"
        for folder_path in glob.glob(os.path.join(root_folder, 'input_*')):
            traverse_folders(folder_path, folder_path)

        # Sort the dictionary by the numerical value of the key
        sorted_dict = dict(sorted(lines_dict.items()))
        #sorted_file = list(sorted_dict.keys())
        #sorted_Enj = list(sorted_dict.values())

        # Read the fourth column of the file "array-bmax.txt"
        with open('array-bmax.txt', 'r') as array_file:
            Et_d, v_d, j_d = [], [], []
            for line in array_file:
                columns = line.split()
                Et_d.append(float(columns[0]))
                v_d.append(int(columns[1]))
                j_d.append(int(columns[2]))

        output_file = 'Enj-output.csv'
        with open(output_file, 'w', newline='') as f:
            csv_writer = csv.writer(f)
            csv_writer.writerow(['Folder Name', 'Enj', 'Et', 'v', 'j', 'Etot'])
            for (key, value), Et, v, j in zip(sorted_dict.items(), Et_d, v_d, j_d):
                array_values.append(float(Et))
                values.append(float(value))
                Etot = format(float(Et)+float(value), ".1f")
                csv_writer.writerow([key, value, Et, v, j, Etot])
                
        print(output_file, " has been generated!")

