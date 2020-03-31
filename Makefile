PROG =	hsmc

SRCS =	hsmc.f90

OBJS =	hsmc.o

LIBS =	

CC = cc
CFLAGS = -O
FC = f77
FFLAGS = -O
F90 = gfortran
#F90FLAGS = -O -g -std=f2008 -Wall -Wextra -fcheck=all -finit-real=snan
F90FLAGS = -O3 -fopenmp -std=f2008 -Wall -Wextra 
LDFLAGS = -fopenmp

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

hsmc.o: hsmc.o
