module NewtonGregoryInterpolationMethod
use Gauss
implicit none
contains

real*8 function NewtonGregoryInterpolation(x,y,xRequerida)
real*8 :: x(:),y(:),xRequerida,s,h,p
real*8,allocatable :: yDifference(:),f0(:)
integer :: i,j,k,n

NewtonGregoryInterpolation=y(1)

call BubbleSortData(x,y)

if(isEquidistant(x).eqv..true.)then
  print*, "Nodes must be equidistant on Newton-Gregory interpolation"
  STOP
end if

h=x(2)-x(1)
s=(xRequerida-x(1))/h
n=size(x)

allocate(yDifference(n),f0(n-1))
k=1

do i=n-1,1,-1
  do j=1,i
    if(i.eq.n-1)then
      yDifference(j)=(y(j+1)-y(j))
    else
      yDifference(j)=(yDifference(j+1)-yDifference(j))
    end if
  end do

  f0(k)=yDifference(1)
  k=k+1
end do

do i=1,n
  p=1
  do j=0,i-1
    p=p*(s-j)
  end do
  NewtonGregoryInterpolation=NewtonGregoryInterpolation+f0(i)*(p/Factorial(i))
end do


deallocate(yDifference,f0)
end function NewtonGregoryInterpolation

real*8 function Factorial(n)
integer :: n,i
Factorial=1
do i=n,1,-1
  Factorial=Factorial*i
end do
end function Factorial

logical function isEquidistant(x)
real*8 :: x(:),h
integer :: i,n

n=size(x)
h=x(2)-x(1)

isEquidistant=.true.

do i=2,n-1
  if(h.ne.x(i+1)-x(i))then
    isEquidistant=.false.
    exit
  end if
end do

end function isEquidistant

subroutine BubbleSortData(x,y)
real*8 :: x(:),y(:),temp
integer :: i,j,n
n = size(x)
if(n.ne.size(y))then
  print*,"x and y must have the same size"
  STOP
end if

do i=1,n-1
  do j=1,n-i
    if(x(j)>x(j+1))then
      temp=x(j)
      x(j)=x(j+1)
      x(j+1)=temp
      temp=y(j)
      y(j)=y(j+1)
      y(j+1)=temp
    end if
  end do
end do

end subroutine BubbleSortData
end module NewtonGregoryInterpolationMethod
