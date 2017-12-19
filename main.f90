Program main
use Homework
use MatrixSize
implicit none
include "mpif.h"
real(8), dimension(:,:), allocatable :: A
integer(4) x1, y1, x2, y2, i
allocate(A(height,length))
open(unit=1, file="vvodBig", status='old', form='formatted')
do i=1,height
 read(1)A(i,:)
enddo
close(1)
call mpi_init(mpiErr)
call FindMaxCoordinates(A, x1, y1, x2, y2)
call mpi_finalize(mpiErr)
write(*,*)x1, y1, x2, y2
deallocate(A)
end
