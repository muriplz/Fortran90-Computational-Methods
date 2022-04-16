module NewtonInterpolationMethod
use Gauss
implicit none
contains

! Newton Interpolation Method
! Input: x(n), y(n), xRequired
! Output: yRequired
real*8 function NewtonInterpolation(x,y,xRequired)
real*8 :: xRequired,x(:),y(:),p
real*8,allocatable :: A(:,:),c(:)
integer :: i,j,k,n
NewtonInterpolation=0
n=size(x)
if(n.ne.size(y))then
  print*,"x and y must be the same size"
  STOP
end if
allocate(A(n,n))
allocate(c(n))
do i=1,n
  do j=1,n
    if(j==1)then
      A(i,j)=1
    else
      p=1
      do k=1,j-1
        p=p*(x(i)-x(k))
      end do
      A(i,j)=p
    end if
  end do
end do
c=LinearSystem(A,y)
do i=1,n
  p=1
  if(i>1)then
    do j=1,i-1
      p=p*(xRequired-x(j))
    end do
  end if
  NewtonInterpolation=NewtonInterpolation+c(i)*p
end do
deallocate(A,c)
end function NewtonInterpolation

end module