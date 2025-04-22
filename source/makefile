Objects = readinput.o \
          testbmax.o \
          writeinput.o \
	  slurm_script.o

FC = gfortran

FFLAGS = -O3

Bin = exe

${Bin}:${Objects}

	${FC} -o ${Bin} $(FFLAGS) ${Objects}

%.o :%.f90

	${FC} -c -O3 -o $@ $<

%.o :%.F90

	${FC} -c -O3 -o $@ $<



.PHONY:clean

clean:

	rm -f exe *.mod $(Objects)
