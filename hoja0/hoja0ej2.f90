program hoja0ej2
real::matrizD(4,5),matrizE(4,5)
integer::vecA(20),matrizB(4,5),matrizC(4,5),aux
! Apartado a)
vecA=(/(i,i=1,20)/)
! Apartado b)
aux=1
do i=1,4
  do j=1,5
    matrizB(i,j)=vecA(aux)
    aux=aux+1
  enddo
enddo
! Apartado c)
do i=1,4
  do j=1,5
    matrizC(i,j)=2*matrizB(i,j)
  enddo
enddo
! Apartado d)
do i=1,4
  do j=1,5
    matrizD(i,j)=matrizB(i,j)/2.
  enddo
enddo
! Apartado e)
do i=1,4
  do j=1,5
    matrizE(i,j)=sqrt(matrizC(i,j)*matrizD(i,j))*2.
  enddo
enddo
print*,"                 Matriz B"
do j=1,5
    print*,matrizB((/(i,i=1,4)/),j)
enddo
print*,"                 Matriz C"
do j=1,5
    print*,matrizC((/(i,i=1,4)/),j)
enddo
print*,"                 Matriz D"
do j=1,5
    print*,matrizD((/(i,i=1,4)/),j)
enddo
print*,"                 Matriz E"
do j=1,5
    print*,matrizE((/(i,i=1,4)/),j)
enddo

end program
