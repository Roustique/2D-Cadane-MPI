module Homework
contains
subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
include "mpif.h"
real(8), intent(in), dimension(:,:) :: A
real(8), dimension(size(A(:,1))) :: B
integer(4), intent(out) :: x1, y1, x2, y2
integer(4) n, m, i, j, k, minn, maxx, x, y
integer(4) mpiErr, mpiSize, mpiRank
integer(4), dimension(4) :: t
real(8) pr, S, maxS, tmaxS

X=0; tx1=1; tx2=1; ty1=1; ty2=1; maxS=A(1,1)
m=size(A(:,1))
n=size(A(1,:))

call mpi_init(mpiErr)
call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

do i=mpiRank,n,mpiSize

 do k=1,m
  B(k)=0
 enddo

 do j=i,n
  B=B+A(:,j)
  pr=B(1); x=1; y=1
  S=pr; minn=x; maxx=y
  do k=2,m
   if (B(k)>(B(k)+pr)) then
    x=k
    y=k
    pr=B(k)
   else
    y=k
    pr=B(k)+pr
   endif
   if (pr>S) then
    S=pr
    minn=x
    maxx=y
   endif
   !write(*,*)B,S
  enddo
  if (S>tmaxS) then
   tmaxS=S
   t(1)=i
   t(2)=j
   t(3)=minn
   t(4)=maxx
  endif
 enddo
enddo

if (mpiRank == 0) then
   maxS=tmaxS
   x1=t(1)
   x2=t(2)
   y1=t(3)
   y2=t(4)
   do k=1,(mpiSize-1)
    call mpi_recv(t, 4, MPI_INTEGER4, MPI_ANY_SOURCE, k, MPI_COMM_WORLD, status, mpiErr)
    call mpi_recv(t, 4, MPI_INTEGER4, MPI_ANY_SOURCE, (k+4), MPI_COMM_WORLD, status, mpiErr)
    if (tmaxS>maxS) then
     maxS=tmaxS
     x1=t(1)
     x2=t(2)
     y1=t(3)
     y2=t(4)    
    endif
   enddo
  else
   call mpi_send(t, 4, MPI_INTEGER4, 0, mpiRank, MPI_COMM_WORLD, mpiErr)
   call mpi_send(tmaxS, 1, MPI_REAL8, 0, (mpiRank+4), MPI_COMM_WORLD, mpiErr)
endif

call mpi_bcast(x1, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(x2, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(y1, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(y2, 4, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
call mpi_finalize(mpiErr)
!write(*,*)maxS
end subroutine
end module
