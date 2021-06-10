real function F(input)result(output)
  real :: input
  output=2*input**3-exp(input)
  return
end function F



program hoja1ej1
real a,b,error
integer N
print *,"Dame a (primer numero del intervalo [a,b])"
read(*,*)a
print*,"Dame b (segundo numero del intervalo [a,b])"
read(*,*)b
print*,"Con que error quieres la raiz?"
read(*,*)error
if(sign(1.0,F(a)).EQ.sign(1.0,F(b)))then
  print*,"No hay raíz en ese intervalo, prueba con otro intervalo"
  exit
endif

N=int((log(b-a)-log(error_deseado))/log(2.00))
do i=0, N
  centro=(b-a)/2
  if (F(centro)==0) then
    exit
  endif
  if (sign(1.0,F(centro)).EQ.sign(1.0,F(a))) then
    a=centro
  endif
  if (sign(1.0,F(centro)).EQ.sign(1.00,F(b))) then
    b=centro
  endif
end do 
print*,"",centro
end program

