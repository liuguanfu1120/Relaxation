OBJECTS	=	nrtype.o\
		nr.o\
		nrutil.o\
		main.o\
		difeq.o\
		solvde.o\
		
		#plgndr.o\



MAIN	=	inf1	


# put your fortran 90/95 compiler command here
#
#FC	=	f90
FC	=	gfortran
#FC	=	f77
#
# put your compile and load options (e.g. optimizations) here
#
#FFLAGS	=	-r8 -O3
#LDFLAGS	=	-r8 -O3
FFLAGS	=	-O3
LDFLAGS	=	-O3


#.SUFFIXES: .for .o
.SUFFIXES: .f90 .o

#
# if your compiler doesn't recognize the .f90 suffix
# you may need to add the following to copy .f90 to .f
#         cat $< > $*.f
#
.for.o:
	$(FC) -c $(FFLAGS)  $<
.f90.o:
	$(FC) -c $(FFLAGS)  $<


$(MAIN): $(OBJECTS) 
	$(FC) $(LDFLAGS) -o $(MAIN)  $(OBJECTS) 
	rm -f $(OBJECTS) $(PROB) *.mod
	./inf1
clean:
	rm -f $(OBJECTS) $(PROB) *.mod

clobber:
	rm -f $(OBJECTS) $(MAIN) 

#nrtype.o : nrtype.f90 nrutil.f90 nr.f90
#nrutil.o : nrtype.f90
#nr.o : nrtype.f90
#ludcmp.o : nrtype.f90 nrutil.f90
#xludcmp.o : nrtype.f90 nrutil.f90 nr.f90
#bessj0.o : nrtype.f90 nrutil.f90
#bessj1.o : nrtype.f90 nrutil.f90
#bessj.o : nrtype.f90 nrutil.f90 nr.f90
#rk4.o : nrtype.f90 nrutil.f90
#xrk4.o : nrtype.f90 nr.f90
#fourrow.o : nrtype.f90 nrutil.f90
#four1.o : nrtype.f90 nrutil.f90 nr.f90
#twofft.o : nrtype.f90 nrutil.f90 nr.f90 
#xtwofft.o : nrtype.f90 nrutil.f90 nr.f90 
#xfour1.o : nrtype.f90 nrutil.f90 nr.f90
#main.o : const.f90
#first_half_step.o : const.f90
#left_right_weno.o : const.f90
#recons_left.o : const.f90
#recons_right.o : const.f90
