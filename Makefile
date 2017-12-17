M=mpiifort
I=ifort
main.out: TASK.o Tester.o
	$(M) $^ -o $@
Tester.o: Tester.f90
	$(I) -c $<
TASK.o: TASK.f90
	$(M) -c $<
debug: Homeworkdebug maindebug
	$(M) TASK.o Tester.o -o maintest.out -g
Homeworkdebug: TASK.f90
	$(M) -c $< -g
maindebug: Tester.f90
	$(I) -c $< -g
exe: main.out
	mpiexec -np 4 ./main.out
