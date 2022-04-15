module Gauss
use Utils
use LinearSystems
implicit none
contains

! Gauss elimination with pivoting
! Input: matrix auxA
! Output: triangular matrix A
function GaussElimination(auxA)result(A)
real*8 :: auxA(:,:),highest
real*8,allocatable :: A(:,:)
integer :: asize(2),i,j,index
asize=shape(auxA)
allocate(A(asize(1),asize(2)))
A=auxA
do i=1,asize(1)-1
  highest=0
  do j=i,asize(1)
    if(highest<abs(A(j,i)))then
      highest=A(j,i)
      index=j
    end if
  end do
  call SwapRows(A,i,index)
  do j=asize(1),i+1,-1
    A(j,:)=A(j,:)-(A(i,:)/A(i,i))*A(j,i)
  end do
end do
end function GaussElimination

! Linear system solver
! Input: matrix A and vector b
! Output: vector x (solution)
function LinearSystem(A,b)result(x)
real*8 :: A(:,:),b(:)
real*8, allocatable :: x(:)
integer :: asize(2)
asize=shape(A)
allocate(x(asize(1)))
x=SystemSolution(GaussElimination(AmplifiedMatrix(A,b)))
end function LinearSystem

end module