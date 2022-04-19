module SecantMethod
implicit none
contains

real*8 function Secant(x0,x1,f,errorY)
real*8 :: x0,x1,slope,errorY
external :: f

do while (abs(f(x1))>errorY)
  slope=(f(x1)-f(x0))/(x1-x0)
  x1=-(f(x1)-slope*x1)/slope
end do
Secant=x1
end function Secant

end module SecantMethod