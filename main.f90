Program main
use Homework
use MatrixSize
implicit none
!include "mpif.h"
real(8), dimension(:,:), allocatable :: A
integer(4) x1, y1, x2, y2, i
allocate(A(height,length))
do i=1,height
 read(*,*)A(i,:)
enddo
call FindMaxCoordinates(A, x1, y1, x2, y2)
write(*,*)x1, y1, x2, y2
deallocate(A)
end
