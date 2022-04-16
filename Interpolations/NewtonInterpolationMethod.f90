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
integer :: i,j,k
NewtonInterpolation=0
if(size(x).ne.size(y))then
  print*,"x and y must be the same size"
  STOP
end if
allocate(A(size(x),size(x)))
allocate(c(size(x)))
do i=1,n
  do j=1,n
    if(i<j)then
      A(i,j)=0
    else if(i==1)then
      A(i,j)=1
    else
      p=1
      do k=1,j
        p=p*(xRequired-x(k))
      end do
      A(i,j)=p
    end if
  end do
end do
c=LinearSystem(A,y)
do i=1,n
  p=1
  do j=1,i
    p=p*(xRequired-x(j))
  end do
  NewtonInterpolation=NewtonInterpolation+c(i)*p
end do
deallocate(A,c)
end function NewtonInterpolation

end module