module Homework
contains
subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
real(8), intent(in), allocatable, dimension(:,:) :: A
real(8), dimension(size(A(:,1))) :: B
integer(4), intent(out) :: x1, y1, x2, y2
integer(4) n, m, i, j, k, minn, maxx, x, y
real(8) pr, S, maxS

X=0; x1=1; x2=1; y1=1; y2=1; maxS=A(1,1)
m=size(A(:,1))
n=size(A(1,:))

do i=1,n

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
  if (S>maxS) then
   maxS=S
   x1=i
   x2=j
   y1=minn
   y2=maxx
  endif
 enddo
 
enddo
!write(*,*)maxS
end subroutine
end module