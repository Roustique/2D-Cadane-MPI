program Tester
use :: Homework
implicit none
    integer(4) :: sizeX, sizeY, i, j
    integer(4) :: referenceX1,referenceX2,referenceY1,referenceY2,x1,y1,x2,y2
    real(8), allocatable, dimension(:,:) :: matrix
    real(8) :: summ, referenceSumm

    open(unit=1, file="matrix.dat", status='old', form='unformatted')
    read(1) sizeX, sizeY
    read(1) referenceX1, referenceY1, referenceX2, referenceY2
    
    allocate(matrix(sizeX, sizeY))
    read(1) matrix
    close(1)

    call FindMaxCoordinates(matrix, x1, y1, x2, y2)

    referenceSumm = 0d0
    do i=referenceX1, referenceX2
        do j=referenceY1, referenceY2
            referenceSumm = referenceSumm + matrix(i, j)
        enddo
    enddo   
    
    summ = 0d0
    do i=x1, x2
        do j=y1, y2
            summ = summ + matrix(i, j)
        enddo
    enddo    
    
    if(dabs(summ - referenceSumm) > 1d0) then
        write(*, *) "Wrong solution!"
        write(*, *) "Matrix", sizeX,"x", sizeY
        write(*, *) "Expected", referenceSumm, referenceX1, referenceY1, referenceX2, referenceY2
        write(*, *) "Got     ", summ, x1, y1, x2, y2
    else
        write(*,*) "It seems the answer is correct"
    endif
    deallocate(matrix)
end program
