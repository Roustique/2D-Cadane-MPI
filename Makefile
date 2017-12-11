G=gfortran
mp=-fopenmp
main.out: Homework.o main.o
	$(G) $^ $(mp) -o $@
main.o: main.f90 matrixsize.mod
	$(G) $(mp) -c $<
Homework.o: Homework.f90
	$(G) $(mp) -c $<
matrixsize.mod: MSize.f90
	$(G) -c $<
debug: Homeworkdebug maindebug
	$(G) Homework.o main.o -o maintest.out $(mp) -g
Homeworkdebug: Homework.f90
	$(G) -c $< $(mp) -g
maindebug: main.f90 matrixsize.mod
	$(G) -c $< $(mp) -g
