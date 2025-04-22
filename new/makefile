Objects = interface.o \
          venus96c-E.o \
          venus-wkb.o \
          O3_1_1Ap.o

FC = ifort

FFLAGS = -O3

Bin = venus-E.e

${Bin}:${Objects}

	${FC} -o ${Bin} $(FFLAGS) ${Objects}

%.o :%.f90

	${FC} -c -O3 -o $@ $<

%.o :%.F90

	${FC} -c -O3 -o $@ $<



.PHONY:clean

clean:

	rm -f *.e *.mod $(Objects)
