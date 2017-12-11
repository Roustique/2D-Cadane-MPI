M=mpiifort
I=ifort
main.out: Homework.o main.o
	$(M) $^ -o $@
main.o: main.f90 matrixsize.mod
	$(I) -c $<
Homework.o: Homework.f90
	$(M) -c $<
matrixsize.mod: MSize.f90
	$(I) -c $<
debug: Homeworkdebug maindebug
	$(M) Homework.o main.o -o maintest.out -g
Homeworkdebug: Homework.f90
	$(M) -c $< -g
maindebug: main.f90 matrixsize.mod
	$(I) -c $< -g
exe: main.out
	mpiexec -np 4 ./main.out < vvod
