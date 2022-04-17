module BisectionMethod
implicit none
contains

real*8 function Bisection(a,b,f,errorX,errorY,maxIter)
real*8 :: a,b,errorX,errorY,m
integer :: maxIter
external :: f

if(f(a)*f(b)>0)then
  print*, "There is no root in the interval"
  STOP
end if

do i=1,maxIter
  m=(a+b)/2
  if(abs(b-a)<errorX.or.abs(f(m))<errorY)then
    exit
  end if

  if(f(m)==0)then
    exit
  else if(f(m)*f(a)<0)then
    b=m
  else
    a=m
  end if
end do

Bisection=m


end function Bisection
end module BisectionMethod