#
# © 2024. Triad National Security, LLC. All rights reserved.
#
# This program was produced under U.S. Government contract 89233218CNA000001
# for Los Alamos National Laboratory (LANL), which is operated by
# Triad National Security, LLC for the U.S. Department of Energy/National
# Nuclear Security Administration. All rights in the program are reserved
# by Triad National Security, LLC, and the U.S. Department of Energy/National
# Nuclear Security Administration. The Government is granted for itself
# and others acting on its behalf a nonexclusive, paid-up, irrevocable
# worldwide license in this material to reproduce, prepare. derivative works,
# distribute copies to the public, perform publicly and display publicly,
# and to permit others to do so.
#
#
# Author:
#    Kai Gao <kaigao@lanl.gov>
#

# paths
bindir = $(PWD)/../bin
objdir = $(PWD)/../obj
moddir = $(PWD)/../obj

# intel paths, must be set correctly
intelroot = $(HOME)/intel
mklroot = $(intelroot)/mkl
mpiroot = $(intelroot)/mpi

# dependencies
object = main.o
obj = $(addprefix $(objdir)/, $(object))

# targets
exec = $(bindir)/x_ewsmod2d
all: makedir $(exec)

# options
fc = ifx
inc =
lflags = -qmkl -qopenmp \
        -L$(intelroot)/lib \
        -L$(mklroot)/lib \
        -L$(mpiroot)/lib \
        -L$(mpiroot)/lib/release \
        $(mklroot)/lib/libmkl_core.a \
        $(mklroot)/lib/libmkl_intel_thread.a \
        $(mklroot)/lib/libmkl_intel_lp64.a \
        $(mklroot)/lib/libmkl_blas95_lp64.a \
        $(mklroot)/lib/libmkl_lapack95_lp64.a
fflags = -O3 -fpp -traceback -warn unused -module $(moddir) -qmkl \
        -assume byterecl -assume buffered_io -qopenmp -heap-arrays \
        -gen-interfaces -xHost -Dfd8 \
	#	-O0 -g -check bounds

# compile
$(objdir)/%.o : $(PWD)/%.f90
	$(fc) -o $@ -c $(fflags) $(inc) $<

# link
$(exec) : $(obj)
	$(fc) -o $@ $^ $(lflags) $(inc)

# make directory
makedir:
	-@mkdir -p $(bindir)
	-@mkdir -p $(objdir)
	-@mkdir -p $(moddir)

# clean
clean:
	-@rm -rf $(objdir)/*.o $(moddir)/*.mod $(exec)

