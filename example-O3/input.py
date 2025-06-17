import numpy as np
import os 

# 检查文件是否存在  
if not os.path.isfile('array-bmax.txt'):
    array_data = np.loadtxt("array.txt")
else:
    array_data = np.loadtxt("array-bmax.txt")

array_data = np.atleast_2d(array_data)


# 获取数组的行数
num_rows = array_data.shape[0]

# 打开输入文件
with open("input", "r") as input_file:
    input_lines = input_file.readlines()
    for i in range(len(input_lines)):
        inline = input_lines[i]
        if '%style' in inline:
            next_line = input_lines[i+1]
            s = int(next_line)

# 遍历数组的行数
for i in range(num_rows):
    num_digits = len(str(num_rows))
    output_file_name = f"input_{i+1:0{num_digits}d}"

    with open(output_file_name, "w") as output_file:
        for line in input_lines:
            line = line.replace("aaa", str(array_data[i, 0]))
            line = line.replace("bbb", str(int(array_data[i, 1])))
            line = line.replace("ccc", str(int(array_data[i, 2])))
            
            if s == 1:
                line = line.replace("ddd", str(0.1))
            else:
                line = line.replace("ddd", str(array_data[i, 3]))
            output_file.write(line)

print(f"生成了 {num_rows} 个输出文件。")
