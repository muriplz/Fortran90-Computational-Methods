module Utils
implicit none
contains

subroutine PrintMatrix(A)
real*8 :: A(:,:)
integer :: asize(2),i,j

asize=shape(A)

do i=1,asize(1)
  write(*,'(*(F8.2,2X))')(/(A(i,j),j=1,asize(2))/)
end do
end subroutine PrintMatrix
end module