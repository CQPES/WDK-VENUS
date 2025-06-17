import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import datetime
import math
import csv
import shutil
import os
import re
from tqdm import tqdm

version = 'VENUS 96'
versiondate = datetime.date(2024, 7, 10)

def displaywelcome():
    string = '\n'
    string += '  ' + '=' * 80 + '\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '||' + '{:^80}'.format('VENUS fort.26/28 output generator') + '||\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '||' + '{:^80}'.format('CQU: Jiawei Yang, Jun Li*') + '||\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '||' + '{:^80}'.format('Version:' + version) + '||\n'
    string += '||' + '{:^80}'.format(versiondate.strftime("%d.%m.%y")) + '||\n'
    string += '||' + '{:^80}'.format('') + '||\n'
    string += '  ' + '=' * 80 + '\n\n'
    string += '''
  This script allows to quickly read venus output files (fort.26) to obtain the 
  scattering Angle distribution (DCS), vibration quantum number (v) distribution, 
  rotational quantum number (j) distribution and the total energy of each 
  trajectory (Et). This script also can read the venus output files (fort.28) 
  to obtian the XYZ files of trajectories.
  '''
    print(string)

# This code reads the file from back to front, the following function is used to get 
# the line number when processing different target strings
def find_line_index(lines, start_index, target_string):
    index = start_index - 1
    while index >= 0 and target_string not in lines[index]:
        index -= 1
    return index

def process_file(lines):
    data_dict = {}

    for i, line in enumerate(lines):
        line = line.strip()

        if 'kcal/mol' in line:
            tokens = line.split()
            deltaH = float(tokens[1])
            re_path = float(tokens[-1])

            if deltaH < 0.001:
                j = find_line_index(lines, i, 'VI,VF:')
                if j >= 0:
                    vivf = float(lines[j].split()[1])

                    k = find_line_index(lines, j, 'REL. TRANS. ENERGY=')
                    if k >= 0:
                        E_tra = lines[k].split()[3].replace('D', 'E')
                        temp = lines[k].split()[-1].replace('D', 'E')
                        # Single-atom fragment output value is 0
                        l = find_line_index(lines, k, 'RESULTS FOR FRAGMENT B')
                        if l >= 0:
                            if len(lines[l + 1].strip()) != 0:
                                E_b_tot = lines[l + 1].split()[2].replace('D', 'E')
                                E_b_v = lines[l + 1].split()[5].replace('D', 'E')
                                E_b_j = lines[l + 1].split()[8].replace('D', 'E')
                                Nb_str = lines[l + 3].split('=')[-2]
                                Jb_str = lines[l + 3].split('=')[-1]
                                Nb = Nb_str.split()[0]
                                Jb = Jb_str.split()[0]
                            else:
                                E_b_tot = 0
                                E_b_v = 0
                                E_b_j = 0
                                Nb = 0
                                Jb = 0

                            m = find_line_index(lines, l, 'RESULTS FOR FRAGMENT A')
                            if m >= 0:
                                if len(lines[m + 1].strip()) != 0:
                                    E_a_tot = lines[m + 1].split()[2].replace('D', 'E')
                                    E_a_v = lines[m + 1].split()[5].replace('D', 'E')
                                    E_a_j = lines[m + 1].split()[8].replace('D', 'E')
                                    Na_str = lines[m + 3].split('=')[-2]
                                    Ja_str = lines[l + 3].split('=')[-1]
                                    Na = Na_str.split()[0]
                                    Ja = Ja_str.split()[0]
                                else:
                                    E_a_tot = 0
                                    E_a_v = 0
                                    E_a_j = 0
                                    Na = 0
                                    Ja = 0

                                n = find_line_index(lines, m, 'IMPACT PARAMETER')
                                if n >= 0:
                                    b_p_a = lines[n].split()[-1]
                                    b_p = float(b_p_a.split('(')[0])

                                    data_dict[tokens[0]] = {'path': re_path, 'vivf': vivf, 'b': b_p,
                                                            'E_b_tot': E_b_tot, 'E_b_v': E_b_v, 'E_b_j': E_b_j,
                                                            'Nb': Nb, 'Jb': Jb, 'E_a_tot': E_a_tot, 'E_a_v': E_a_v,
                                                            'E_a_j': E_a_j, 'Na': Na, 'Ja': Ja, 'E_tra': E_tra,
                                                            'temp': temp}

    return data_dict

folders = [folder for folder in os.listdir() if os.path.isdir(folder)]

for folder in tqdm(folders, desc="Processing Folders", unit="folder"):
    # path
    file_path = os.path.join(folder, 'fort.26')

    if os.path.exists(file_path):
        with open(file_path, 'r') as file:
            lines = file.readlines()

        # write to csv
        data_dict = process_file(lines)
        output_file_path = os.path.join(folder, f'output.csv')
        with open(output_file_path, 'w', newline='') as csvfile:
            fieldnames = ['traj_num', 'path', 'b', 'vivf', 'E_b_tot', 'E_b_v', 'E_b_j', 'Nb', 'Jb',
                          'E_a_tot', 'E_a_v', 'E_a_j', 'Na', 'Ja', 'E_tra', 'temp']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

            writer.writeheader()
            writer.writerows({'traj_num': key, **values} for key, values in data_dict.items())

# Initialisation of some parameters
re2,re3,re4,tot,bmax = 0,0,0,0,0

for entry in os.scandir('.'):
    if "energy.convergence-0.001" in entry.name:
        with open(entry.path, 'r') as file:
            for line in file:
                if 'total path 2' in line:
                    re2 = int(line.split()[-1])
                if 'total path 3' in line:
                    re3 = int(line.split()[-1])
                if 'total path 4' in line:
                    re4 = int(line.split()[-1])
                if 'sum of total traj' in line:
                    tot = int(line.split()[-1])

for entry_b in os.scandir('../'):
    if entry_b.is_file() and 'input_' in entry_b.name:
        with open(entry_b.path, 'r') as file:
            lines = file.readlines()
            for i in range(len(lines)):
                if '%bmax' in lines[i]:
                    bmax = float(lines[i+1].split()[0])

radian = math.pi/180

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
# Selecting the analysis mode
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

displaywelcome()
style = int(input('''\nType of task:
1.DCS diagram
2.v distribution (HB=0/1GB=1)
3.j distribution
4.prints the specified trajectories

Please enter the number corresponding to the type of calculation: '''))

if style == 1:
    nbin = int(input('which bins: '))
    nre = str(input('reaction path (2+3/4): '))
    NMAX = int(input('The Maximum vibrational quantum number: '))
    #path_vivf_dict = {}
    d_dcs_dict = {}

    if nre == '4':
        re = re4
    elif nre == '2+3':
        re = re2 + re3
    ics = float(math.pi*(bmax**2)*(re)/tot)
    print(ics)

    # Get all folders in the current directory
    folders = [folder for folder in os.listdir() if os.path.isdir(folder)]
    # Initialise an empty DataFrame for storing the extracted data
    all_data = pd.DataFrame(columns=['path', 'vivf'])
    # Loop through each folder
    for folder in tqdm(folders, desc="Processing Folders"):
        file_path = os.path.join(folder, 'output.csv')

        if os.path.exists(file_path):
            df = pd.read_csv(file_path)
            if 'path' in df.columns and 'Nb' in df.columns and 'Jb' in df.columns:
                df['Nb'] = pd.to_numeric(df['Nb'], errors='coerce')
                df['Jb'] = pd.to_numeric(df['Jb'], errors='coerce')
                df = df.dropna(subset=['Nb'])
                df = df.dropna(subset=['Jb'])
                df = df[df['Nb'] < NMAX+0.5]

                numbers = [int(num.strip()) for num in nre.split('+')]
                subset = df[df['path'].isin(numbers)][['path', 'vivf']]

                all_data = pd.concat([all_data, subset], ignore_index=True)
    
    print(len(all_data))

    # Interval division
    bins = np.linspace(0, 180, nbin+1)
    hist, edges = np.histogram(all_data['vivf'].astype(float), bins=bins)

    for i in range(len(hist)):
        start = edges[i]
        end = edges[i + 1]
        count = hist[i]
        degree = 180/nbin*(i+0.5)
        dcs = format(ics/(2*math.pi)*count/tot/math.sin(degree*radian),".3f")
        d_dcs_dict[float(degree)] = float(dcs)

    # Extract sorted D and DCS
    sorted_d_dcs = dict(sorted(d_dcs_dict.items()))
    sorted_d = list(sorted_d_dcs.keys())
    sorted_dcs = list(sorted_d_dcs.values())

    output_file = 'DCS_output.csv'
    with open(output_file, 'w', newline='') as csvfile:
        fieldnames = ['Degree','DCS']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for key, value in zip(sorted_d, sorted_dcs):
            writer.writerow({'Degree':key,'DCS':value})

    # plot figure
    plt.figure(figsize=(5, 3))
    plt.plot(sorted_d, sorted_dcs, marker='o', linestyle='-')
    plt.xlabel('Degree ($\mathregular{o}$)')
    plt.ylabel('DCS')
    plt.title('Plot of DCS')
    plt.grid(True)
    plt.savefig('DCS.png', dpi=600, bbox_inches='tight')

    print("Generated figure: DCS.png and file: ", output_file)

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#style = 2
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

elif style == 2:
    nre = str(input('reaction path (2+3): '))
    NMAX = float(input('The Maximum vibrational quantum number: '))
    B_style = int(input('Please select statistical method (HB=0/1GB=1):'))
    p_nb_dict = {}

    folders = [folder for folder in os.listdir() if os.path.isdir(folder)]
    all_data = pd.DataFrame(columns=['path', 'Nb'])
    
    for folder in tqdm(folders, desc='Processing Folders'):
        file_path = os.path.join(folder, 'output.csv')

        if os.path.exists(file_path):
            df = pd.read_csv(file_path)
            if 'path' in df.columns and 'Nb' in df.columns and 'Jb' in df.columns:
                df['Nb'] = pd.to_numeric(df['Nb'], errors='coerce')
                df['Jb'] = pd.to_numeric(df['Jb'], errors='coerce')
                df = df.dropna(subset=['Nb'])
                df = df.dropna(subset=['Jb'])
                df = df[df['Nb'] < NMAX+0.5]
        
                numbers = [int(num.strip()) for num in nre.split('+')]
                subset = df[df['path'].isin(numbers)][['path', 'Nb']]

                all_data = pd.concat([all_data, subset], ignore_index=True)

    print(len(all_data))
    # find the max nb_set
    max_nb = all_data['Nb'].astype(float).max()
    print(max_nb)
    max_nb = round(max_nb)

    if B_style == 0:

        # Interval division
        bins = np.arange(-0.5, max_nb + 1.5, 1)

        # Counting the number of Nb in different intervals
        hist, edges = np.histogram(all_data['Nb'].astype(float), bins=bins)

        for i in range(len(hist)):
            count = hist[i]
            p_nb = count/len(all_data)
            p_nb_dict[int(i)] = float(p_nb)

        sorted_nb = dict(sorted(p_nb_dict.items()))
        nb_v = list(sorted_nb.keys())
        nb_p = list(sorted_nb.values())

        hist_output_file = 'nb_histogram.csv'
        with open(hist_output_file, 'w', newline='') as csvfile:
            fieldnames = ['v','Population']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            for key, value in zip(nb_v, nb_p):
                writer.writerow({'v':key,'Population':value})
    
        # Plotting of histograms
        plt.figure(figsize=(5, 3))
        plt.bar(bins[:-1], nb_p, width=0.8, align='edge')
        plt.xlabel('${v}$')
        plt.ylabel('Population')
        plt.title('The histogram of ${v_b}$')
        plt.grid(True)
        plt.savefig('nb_histogram.png', dpi=600, bbox_inches='tight')

        print("Generated figure: nb_histogram.png and file:", hist_output_file)

    elif B_style == 1:

        GP = pd.DataFrame(columns=['Nb','GP','Ep_Nb','Eh_Nb'])

        Beta = (2*(math.log(2))**0.5)/0.1
        GP['Nb'] = abs(round(all_data['Nb']))
        GP['Ep_Nb'] = 1588.8*(all_data['Nb'] + 0.5)
        GP['Eh_Nb'] = 1588.8*(abs(round(all_data['Nb'])) + 0.5)
        E0 = 1588.8*0.5
        GP['GP'] = (Beta*np.exp(-(Beta**2)*((GP['Ep_Nb'] - GP['Eh_Nb'])/(2 * E0))**2) / np.pi**0.5)/len(all_data)

        GP_grouped = GP.groupby('Nb', as_index=False).agg({'GP': 'sum', 'Ep_Nb': 'first', 'Eh_Nb': 'first'})

        GP_grouped.to_csv('nb_1GB_histogram.csv', index=False)
        hist_output_file = 'nb_1GB_histogram.csv'

        # Plotting of histograms
        plt.figure(figsize=(5, 3))
        plt.bar(GP_grouped['Nb'], GP_grouped['GP'], width=0.8, align='edge')
        plt.xlabel('${v}$')
        plt.ylabel('P')
        plt.title('The 1GB histogram of ${v_b}$')
        plt.grid(True)
        plt.savefig('nb_1GB_histogram.png', dpi=600, bbox_inches='tight')

        print("Generated figure: nb_1GB_histogram.png and file: ", hist_output_file)

    
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#style = 3
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

elif style == 3:
    nre = str(input('reaction path (2+3): '))
    NMAX = float(input('The Maximum vibrational quantum number: '))
    p_jb_dict = {}

    folders = [folder for folder in os.listdir() if os.path.isdir(folder)]
    all_data = pd.DataFrame(columns=['path', 'Jb'])
    
    for folder in tqdm(folders, desc="Processing Folders"):
        file_path = os.path.join(folder, 'output.csv')

        if os.path.exists(file_path):
            df = pd.read_csv(file_path)
            if 'path' in df.columns and 'Nb' in df.columns and 'Jb' in df.columns:
                df['Nb'] = pd.to_numeric(df['Nb'], errors='coerce')
                df['Jb'] = pd.to_numeric(df['Jb'], errors='coerce')
                df = df.dropna(subset=['Nb'])
                df = df.dropna(subset=['Jb'])
                df = df[df['Nb'] < NMAX+0.5]

                numbers = [int(num.strip()) for num in nre.split('+')]
                subset = df[df['path'].isin(numbers)][['path', 'Jb']]

                all_data = pd.concat([all_data, subset], ignore_index=True)

    print(len(all_data))
    
    max_jb = all_data['Jb'].astype(float).max()
    print(max_jb)
    max_jb = round(max_jb)

    bins = np.arange(-0.5, max_jb+1.5, 1)
    hist, edges = np.histogram(all_data['Jb'].astype(float), bins=bins)

    for i in range(len(hist)):
        count = hist[i]
        p_jb = count/len(all_data)
        p_jb_dict[int(i)] = float(p_jb)

    sorted_jb = dict(sorted(p_jb_dict.items()))
    jb_j = list(sorted_jb.keys())
    jb_p = list(sorted_jb.values())

    hist_output_file = 'jb_histogram.csv'
    with open(hist_output_file, 'w', newline='') as csvfile:
        fieldnames = ['j','Population']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for key, value in zip(jb_j, jb_p):
            writer.writerow({'j':key,'Population':value})

    plt.figure(figsize=(5, 3))
    plt.bar(bins[:-1], jb_p, align='edge')
    plt.xlabel('${j}$')
    plt.ylabel('Population')
    plt.title('The histogram of ${j_b}$')
    plt.grid(True)
    plt.savefig('jb_histogram.png', dpi=600, bbox_inches='tight')

    print("Generated figure: jb_histogram.png and file: ", hist_output_file)

#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#style = 4
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

elif style == 4:
    def traj_numbers(ntraj):
        traj_numbers = set()
        for part in ntraj.split():
            if '-' in part:
                start, end = map(int, part.split('-'))
                traj_numbers.update(range(start, end + 1))
            else:
                traj_numbers.add(int(part))
        return sorted(traj_numbers)
    
    # Create TRAJ-XYZ folder
    if os.path.exists("TRAJ-XYZ"):
        shutil.rmtree("TRAJ-XYZ")
    os.makedirs("TRAJ-XYZ")

    file_counts = {}

    nfiles = int(input("The file number: "))
    ntraj = input("The number of the trajectory ('3 4 9' and/or '10-15'): ")
    ntraj_all = traj_numbers(ntraj)

    natoms = int(input("Total atomic number: "))
    symbol = input("Elemental symbol ('A B C'): ").split()

    # PATH
    folder_name = f'{nfiles:04d}'
    file_path = os.path.join(folder_name, 'fort.28')

    if not os.path.exists(file_path):
        print(f"NO {file_path}!")
    else:
        with open(file_path, 'r') as source_file:
            for line in source_file:
                match = re.search(r'NUMBER\s+(\d+)', line)
                if match:
                    number_str = match.group(1).strip()
                    if int(number_str) in ntraj_all:
                        if number_str not in file_counts:
                            file_counts[number_str] = 1
                        else:
                            file_counts[number_str] += 1
                    
                        traj_filename = 'traj{:04d}.xyz'.format(int(match.group(1)))
                    
                        with open(traj_filename, 'a') as traj_file:
                            traj_file.write(f'{natoms}\n')
                            traj_file.write(str(file_counts[number_str]) + '\n')
                            for i in range(natoms):
                                next_line = source_file.readline()
                                prefix = symbol[i % len(symbol)]
                                traj_file.write(prefix + next_line)

        files = [f for f in os.listdir('.') if f.endswith('.xyz')]
        for f in files:
            shutil.move(f, 'TRAJ-XYZ')
    print("TRAJ-XYZ folder generated!")
