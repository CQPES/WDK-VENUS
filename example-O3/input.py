import numpy as np
import os 

# Check if the file exists
if not os.path.isfile('array-bmax.txt'):
    array_data = np.loadtxt("array.txt")
else:
    array_data = np.loadtxt("array-bmax.txt")

array_data = np.atleast_2d(array_data)


# Get the number of rows in the array
num_rows = array_data.shape[0]

# Open input file
with open("input", "r") as input_file:
    input_lines = input_file.readlines()
    for i in range(len(input_lines)):
        inline = input_lines[i]
        if '%style' in inline:
            next_line = input_lines[i+1]
            s = int(next_line)
if s == 1:
    print('Style 1: Automatic bmax testing')
    bmax = input('Specify the starting value for the bmax test (0.1): ')
elif s == 0:
    print('Style 0: QCT dynamic simulation')
else:
    print('Style 2: Validation bmax value')

# Iterate through the rows of the array
for i in range(num_rows):
    num_digits = len(str(num_rows))
    output_file_name = f"input_{i+1:0{num_digits}d}"

    with open(output_file_name, "w") as output_file:
        for line in input_lines:
            line = line.replace("aaa", str(array_data[i, 0]))
            line = line.replace("bbb", str(int(array_data[i, 1])))
            line = line.replace("ccc", str(int(array_data[i, 2])))
            
            if s == 1:
                line = line.replace("ddd", str(bmax))
            else:
                line = line.replace("ddd", str(array_data[i, 3]))
            output_file.write(line)

print(f"{num_rows} input file generated!")
