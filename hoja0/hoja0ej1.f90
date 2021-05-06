program hoja0ej1
integer:: vecA(15),vecB(11),aux
integer:: matriz(5,3)

! Apartado b)
vecA=(/(i,i=1,15)/)
! Apartado a)
vecB=(/(i,i=-5,5)/)
! Apartado c)

do i=1,11
  if (vecB(i)>=-3.and.vecB(i)<=3) then
    print*,vecB(i)
  endif
enddo

! Apartado d)
print*,"              Matriz A dim=5x3"
aux=1
do i=1,3
  do j=1,5
    matriz(j,i)=vecA(aux)
    aux=aux+1
  enddo
enddo

do i=1,5
  print*,matriz(i,1),matriz(i,2),matriz(i,3)
enddo


    
end program 
