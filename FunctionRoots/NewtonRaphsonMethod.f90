module NewtonRaphsonMethod
implicit none

contains

real*8 function NewtonRaphson(x0,f,df,errorY)
real*8 :: x0,errorY,slope
external :: f,df

slope=df(x0)
NewtonRaphson=x0

do while (abs(f(NewtonRaphson))>errorY)
    NewtonRaphson=NewtonRaphson-f(NewtonRaphson)/slope
    slope=df(NewtonRaphson)
end do

end function NewtonRaphson

end module