real function F(input)result(output)
  real :: input
  output=2*input**3-exp(input)
  return
end function


program hoja1ej2
real a,b,m
logical p

p=.true.
do while (p.eqv..true.)
  print *,"Dame a (primer numero del intervalo [a,b])"
  read(*,*)a
  print*,"Dame b (segundo numero del intervalo [a,b])"
  read(*,*)b
  p=.false.
  if(sign(1.0,F(a)).EQ.sign(1.0,F(b)))then
    print*,"No hay raiz en ese intervalo, prueba con otro intervalo"
    p=.true.
  endif
enddo

p=.true.
do while(p.eqv..true.)
  m=(F(b)-F(a))/(b-a)
  centro=a-(b-a)*(F(a)/(F(b)-F(a)))
  if(F(centro)==0)then 
    p=.false.
  endif
  if(sign(1.0,F(centro))==sign(1.0,F(a))) then
    a=centro
  else
    b=centro
  endif
enddo

print*,"La raiz es ",centro
end program
  




