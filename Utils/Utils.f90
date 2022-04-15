module Utils
implicit none
contains

! Prints a matrix to the screen
subroutine PrintMatrix(A)
real*8 :: A(:,:)
integer :: asize(2),i,j

asize=shape(A)

do i=1,asize(1)
  write(*,'(*(F8.2,2X))')(/(A(i,j),j=1,asize(2))/)
end do
end subroutine PrintMatrix

! Merges a matrix with a vector
! Input: matrix A, vector b
! Output: Amplified matrix Ab
function AmplifiedMatrix(A,b)result(Ab)
real*8 :: A(:,:),b(:)
real*8,allocatable :: Ab(:,:)
integer :: asize(2),i,j

asize=shape(A)
if(size(b).ne.asize(1))then
  print*,"Amplified Matrix cannot be created"
  STOP
end if
allocate(Ab(asize(1),asize(2)+1))
do i=1,asize(1)
  do j=1,asize(2)+1
    if(j>asize(2))then
      Ab(i,j)=b(i)
    else
      Ab(i,j)=A(i,j)
    end if
  end do
end do
end function AmplifiedMatrix

! Swaps two rows of a matrix
subroutine SwapRows(A,i,j)
real*8 :: A(:,:)
real*8,allocatable :: aux(:)
integer :: i,j,asize(2)
asize=shape(A)
allocate(aux(asize(2)))
aux=A(i,:)
A(i,:)=A(j,:)
A(j,:)=aux
deallocate(aux)
end subroutine SwapRows

end module