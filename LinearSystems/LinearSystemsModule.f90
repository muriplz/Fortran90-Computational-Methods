module LinearSystems
use Utils
implicit none

contains

subroutine SwitchRows(A,i,j)
real*8 :: A(:,:)
real*8,allocatable :: aux(:)
integer :: i,j,asize(2)
asize=shape(A)
allocate(aux(asize(2)))
aux=A(i,:)
A(i,:)=A(j,:)
A(j,:)=aux
deallocate(aux)
end subroutine SwitchRows

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
  call SwitchRows(A,i,index)
  do j=asize(1),i+1,-1
    A(j,:)=A(j,:)-(A(i,:)/A(i,i))*A(j,i)
  end do
end do
end function GaussElimination

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

function LinearSystem(A,b)result(x)
real*8 :: A(:,:),b(:)
real*8, allocatable :: x(:)
integer :: asize(2)
asize=shape(A)
allocate(x(asize(1)))
x=SystemSolution(GaussElimination(AmplifiedMatrix(A,b)))
end function

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