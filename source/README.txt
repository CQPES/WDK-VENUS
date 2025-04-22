||******WDK-VENUS********||
||   source code in source   ||
||****************************||

Four files need to be compiled in this folder:

readinput.f90-------------|------exe
writeinput.f90-------------|
testbmax.f90--------------|
slurm_script.f90-----------|

There is a makefile file in the folder, just type 'make' to compile.

(1) The slurm_script.f90 file is based on the slurm script and can be modified by the user if they use a different job management system.
(2) The reaction paths in the testbmax.f90 file need to be modified according to the reaction path under study.
(3) The user can set how many seconds to read the fort.999 file in the writeinput.f90 file.
(4) The writeinput.f90 file requires the user to change the path according to their directory:
      !#################################################
      ftmp='cp /home/jwyang/WDK-VENUS/'   !change for your dict ##
      !################################################
(5) The writeinput.f90 contains the input file template for VENUS, which can be modified by the user as required.