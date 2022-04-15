module LinearSystems
use Utils
implicit none
contains

! Solves a linear system of equations having a triangular matrix
! and a vector attached to it.
! Input: Amplified matrix Ab
! Output: Solution vector x
function SystemSolution(Ab)result(x)
real*8 :: Ab(:,:),sum
real*8,allocatable :: x(:)
integer :: asize(2),i,j
asize=shape(Ab)
if(asize(1)+1.ne.asize(2))then
  print*,"This system cannot be solved, system's matrix is: "
  call PrintMatrix(Ab)
  STOP
end if
allocate(x(asize(1)))
x(asize(1))=Ab(asize(1),asize(2))/Ab(asize(1),asize(1))
do i=asize(1)-1,1,-1
  sum=0
  do j=i+1,asize(1)
    sum=sum+Ab(i,j)*x(j)
  end do
  x(i)=(Ab(i,asize(2))-sum)/Ab(i,i)
end do
end function SystemSolution
end module