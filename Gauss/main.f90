program prueba2
use Gaussy
use DeterminanteYadjunto
implicit none
real*8,allocatable:: A(:,:),b(:)
integer::n,i,j
print*,"Dime n"
read*,n
allocate(A(n,n))
print*,"Dame A"
read*,A
if(Determinante(A)==0.0)then
  print*,"Determinante es 0, prueba otra matriz"
  STOP
endif
allocate(b(n))
print*,"Dame b"
read*,b

call SolucionGauss(A,b)

do i=1,n
  write (*,*)(A(i,j),j=1,n)
enddo
print*,"----------------"
print*,b

deallocate(A,b)
end program
