M=mpiifort
main.out: TASK.o Tester.o
	$(M) $^ -o $@
Tester.o: Tester.f90
	$(M) -c $<
TASK.o: TASK.f90
	$(M) -c $<
debug: Homeworkdebug maindebug
	$(M) TASK.o Tester.o -o maintest.out -debug
Homeworkdebug: TASK.f90
	$(M) -c $< -debug
maindebug: Tester.f90
	$(M) -c $< -debug
exe: main.out
	mpiexec -np 4 ./main.out
