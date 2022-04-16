module DirectInterpolationMethod
use Gauss
implicit none
contains

! Direct interpolation
! Input: x(n), y(n), xi
! Output: yi
real*8 function DirectInterpolation(x,y,xRequired)
real*8 :: x(:),y(:),xRequired
real*8,allocatable :: Vandermonde(:,:),Pa(:)
integer :: i,j,n

n=size(x)
if(n.ne.size(y))then
  print*,"dim(X) not equals to dim(Y). Stopping program"
  STOP
end if

allocate(Vandermonde(n,n))
allocate(Pa(n))

do i=1,n
  do j=1,n
    Vandermonde(i,j)=x(i)**(j-1)
  end do
end do

Pa=LinearSystem(Vandermonde,y)

do i=1,n
  DirectInterpolation=DirectInterpolation+Pa(i)*xRequired**(i-1)
end do

deallocate(Vandermonde,Pa)
end function DirectInterpolation

end module