module LagrangeInterpolationMethod
implicit none
contains

! Lagrange interpolation
! Input: x(n), y(n), xi
! Output: yi
real*8 function LagrangeInterpolation(x,y,xRequired)
real*8 :: x(:),y(:),xRequired,yRequired,p
integer :: i,j,n
LagrangeInterpolation=0
n=size(x)
if(n.ne.size(y))then
  print*,"dim(X) not equals to dim(Y). Stopping program"
  STOP
end if
do i=1,n
  p=1
  do j=1,n
    if(j.ne.i)then
      p=p*((xRequired-x(j))/(x(i)-x(j)))
    end if
  enddo
  LagrangeInterpolation=LagrangeInterpolation+(p*y(i))
enddo

end function LagrangeInterpolation
end module