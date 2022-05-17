module RichardsonExtrapolation
implicit none
contains

real*8 function Richardson(f,x,h,n)
real*8, external :: f
real*8 :: x,h,temp
real*8, allocatable :: D(:,:)
integer :: n,i,k

allocate(D(n+1,n+1))

do k=0,n
  temp=h/2**k
  D(k+1,1)=(f(x+temp)-f(x-temp))/(2*temp)
end do

do i=2,n+1
  do k=i,n+1
    temp = 4**(k-1)
    D(k,i)=temp*D(k,i-1)/(temp-1)-D(k-1,i-1)/(temp-1)
  end do
end do
Richardson=D(n+1,n+1)
deallocate(D)

end function


end module RichardsonExtrapolation