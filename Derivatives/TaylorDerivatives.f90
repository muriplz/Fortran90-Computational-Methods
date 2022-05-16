module TaylorDerivatives
implicit none
contains

real*8 function DerivativeStepForward(f,x,h)
  real*8 :: h,x
  real*8,external :: f
  DerivativeStepForward = (f(x+h)-f(x))/h
end function

real*8 function DerivativeStepBack(f,x,h)
  real*8 :: h,x
  real*8,external :: f
  DerivativeStepBack=(f(x)-f(x-h))/h
end function

real*8 function DerivativeCentered(f,x,h)
  real*8 :: h,x
  real*8,external :: f
  DerivativeCentered = (f(x+h)-f(x-h))/2*h
end function

real*8 function SecondDerivative(f,x,h)
  real*8 :: h,x
  real*8,external :: f
  SecondDerivative = (f(x+h)-2f(x)+f(x-h))/h**2
end function

end module