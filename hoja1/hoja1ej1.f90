real function F(input)
  real input,output
  output=2*input*input*input-exp(input)
  return
end function



program hoja1ej1
real a,b,error
integer N
print *,"Dame a (primer numero del intervalo [a,b])"
read(*,*)a
print*,"Dame b (segundo numero del intervalo [a,b])"
read(*,*)b
print*,"Con que error quieres la raiz?"
read(*,*)error
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

