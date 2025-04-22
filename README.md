# WDK-VENUS

## Introduction

The external program is written in Python and documented in a file called workflow.ipynb. Each command block in the file executes a task, some of which can be executed directly and some of which require the user to enter parameters to control the scope of the task execution.

## Notes

`matlab.engine` is a Python interface provided by MATLAB that allows users to interact with MATLAB in a Python environment. To use it, you need to install it by running the following command:

```matlab
>> cd (fullfile(matlabroot,'extern','engines','python'))
>> system('python setup.py install')
```

## Requirements

- Python >= 3.9
- MATLAB >= R2022b

## License

Apache-2.0
