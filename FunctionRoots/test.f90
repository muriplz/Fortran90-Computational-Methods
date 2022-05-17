program testing
use BisectionMethod
implicit none
real*8 :: a,b,errorX,errorY
integer :: maxIter

print*, "Enter a"
read(*,*) a
print*, "Enter b"
read(*,*) b
print*, "Enter errorX"
read(*,*) errorX
print*, "Enter errorY"
read(*,*) errorY
print*, "Enter maxIter"
read(*,*) maxIter

print*, Bisection(a,b,f,errorX,errorY,maxIter)

end program testing

