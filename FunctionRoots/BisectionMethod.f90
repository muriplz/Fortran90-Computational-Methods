module BisectionMethod
use Utils
implicit none
contains

real*8 function f(x)
real*8 :: x
f=x**2-exp(x)
end function f
 
integer function MinIterations(a,b,f,errorX)
real*8 :: a,b,errorX,aux
real*8,external :: f
aux=(log(abs(b-a))-log(errorX))/log(2.0)
MinIterations=ceiling(aux)
end function MinIterations

real*8 function Bisection(a,b,f,errorX,errorY,maxIter)
real*8 :: a,b,errorX,errorY,m
integer :: maxIter,i
real*8,external :: f

call BolzanoTheorem(a,b,f)

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