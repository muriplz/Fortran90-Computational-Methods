program hoja0ej5
real:: vector(5),vector2(5)
!Primero el vector necesitará tener valores, así  que los leo por pantalla
!Primero uso la función "cuadrado", que sólo eleva al cuadrado un número real, por eso va en un ciclo
do i=1,5
  write(*,*)"Dame el valor de la ",i," posicion del vector"
  read*,vector(i)
  vector(i)=cuadrado(vector(i))
enddo
!Imprimo por pantalla el vector
print*,vector((/(i,i=1,5)/))
end program hoja0ej5

!Función para elevar al cuadrado un número real
real function cuadrado(valor)result(resultado)
  real,intent(in)::valor
  resultado=valor**2
  return
end function cuadrado
  
  
