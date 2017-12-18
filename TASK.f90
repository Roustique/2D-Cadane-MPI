module Homework
contains
subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
include "mpif.h"
real(8), intent(in), dimension(:,:) :: A
real(8), dimension(size(A(:,1))) :: B
integer(4), intent(out) :: x1, y1, x2, y2
integer(4) Alength, Aheight, i, j, k, bottom_border, upper_border, x, y, thread_x1, thread_x2, thread_y1, thread_y2
integer(4) mpiErr, mpiSize, mpiRank
real(8) previous_Summ, Summ, maxSumm, thread_maxSumm

thread_x1=1; thread_x2=1; thread_y1=1; thread_y2=1; thread_maxSumm=A(1,1); maxSumm=A(1,1)
Aheight=size(A(:,1))
Alength=size(A(1,:))

call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

do i=(mpiRank+1),Alength,mpiSize

 do k=1,Aheight
  B(k)=0
 enddo

 do j=i,Alength
  B=B+A(:,j)
  previous_Summ=B(1); x=1; y=1
  Summ=previous_Summ; bottom_border=x; upper_border=y
  do k=2,Aheight
   if (B(k)>(B(k)+previous_Summ)) then
    x=k
    y=k
    previous_Summ=B(k)
   else
    y=k
    previous_Summ=B(k)+previous_Summ
   endif
   if (previous_Summ>Summ) then
    Summ=previous_Summ
    bottom_border=x
    upper_border=y
   endif

  enddo
  if (Summ>thread_maxSumm) then
   thread_maxSumm=Summ
   thread_x1=bottom_border
   thread_x2=upper_border
   thread_y1=i
   thread_y2=j
  endif
 enddo
enddo


  do k=0,(mpiSize-1)
   if (mpiRank==0) then
    maxSumm=thread_maxSumm
    x1=thread_x1
    x2=thread_x2
    y1=thread_y1
    y2=thread_y2
   elseif ((mpiRank==k).AND.(mpiRank/=0)) then
    call mpi_send(thread_x1, 1, MPI_INTEGER4, 0, 5*(k-1), MPI_COMM_WORLD, mpiErr)
    call mpi_send(thread_x2, 1, MPI_INTEGER4, 0, 5*(k-1)+1, MPI_COMM_WORLD, mpiErr)
    call mpi_send(thread_y1, 1, MPI_INTEGER4, 0, 5*(k-1)+2, MPI_COMM_WORLD, mpiErr)
    call mpi_send(thread_y2, 1, MPI_INTEGER4, 0, 5*(k-1)+3, MPI_COMM_WORLD, mpiErr)
    call mpi_send(thread_maxSumm, 1, MPI_REAL8, 0, 5*(k-1)+4, MPI_COMM_WORLD, mpiErr)
   endif
  enddo
if (mpiRank == 0) then
   do k=1,(mpiSize-1)
    call mpi_recv(thread_x1, 1, MPI_INTEGER4, MPI_ANY_SOURCE, 5*(k-1), MPI_COMM_WORLD, status, mpiErr)
    call mpi_recv(thread_x2, 1, MPI_INTEGER4, MPI_ANY_SOURCE, 5*(k-1)+1, MPI_COMM_WORLD, status, mpiErr)
    call mpi_recv(thread_y1, 1, MPI_INTEGER4, MPI_ANY_SOURCE, 5*(k-1)+2, MPI_COMM_WORLD, status, mpiErr)
    call mpi_recv(thread_y2, 1, MPI_INTEGER4, MPI_ANY_SOURCE, 5*(k-1)+3, MPI_COMM_WORLD, status, mpiErr)
    call mpi_recv(thread_maxSumm, 1, MPI_REAL8, MPI_ANY_SOURCE, 5*(k-1)+4, MPI_COMM_WORLD, status, mpiErr)
    if (thread_maxSumm>=maxSumm) then
     maxSumm=thread_maxSumm
     x1=thread_x1
     x2=thread_x2
     y1=thread_y1
     y2=thread_y2    
    endif
   enddo
endif

!call mpi_barrier(MPI_COMM_WORLD, mpiErr)

call mpi_bcast(x1, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(x2, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(y1, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(y2, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
end subroutine
end module
