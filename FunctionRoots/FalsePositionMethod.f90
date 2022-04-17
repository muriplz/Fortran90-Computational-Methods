module FalsePositionMethod
implicit none
contains

real*8 function FalsePosition(a,b,f,errorX,errorY,maxIter)
real*8 :: a,b,errorX,errorY,slope,m
integer :: maxIter
external :: f

call BolzanoTheorem(a,b,f)

do i=1,maxIter
  slope=(f(b)-f(a))/(b-a)
  m=a-(f(a)/slope)

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

end function FalsePosition


end module FalsePositionMethod