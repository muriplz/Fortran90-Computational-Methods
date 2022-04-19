module Utils
implicit none
contains

subroutine BolzanoTheorem(a,b,f)
real*8 :: a,b
external :: f
if(f(a)*f(b)>0)then
  print*, "There is no root in the interval"
  STOP
end if
end subroutine BolzanoTheorem

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

real*8 function Factorial(n)
integer :: n,i
Factorial=1
if(n<0)then
  print*, "Factorial of negative number is undefined"
  STOP
else if(n==0)then
  Factorial=1
else
  do i=n,1,-1
    Factorial=Factorial*i
  end do
end if
end function Factorial

end module