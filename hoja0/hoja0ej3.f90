program hoja0ej3
integer :: tam_log, dimension_maxima,vector(100)
integer :: matriz(100,100),tam_log_filas,tam_log_columnas
! Leo la dimensión del vector
print*,"Dame el tamanno del vector"
read*,tam_log
! Pido por pantalla el valor que lleva cada posición del vector
do i=1,tam_log
  print*,"Dame la posicion ",i
  read*,vector(i)
enddo
! Excribo por pantalla el vector
do i=1,tam_log
  print*,"La posicion ",i," del vector tiene el valor: ",vector(i)
enddo

! Leo toda la dimension de la matriz
print*,"Dame el tamanno de filas que tiene la matriz"
read*,tam_log_filas
print*,"Dame el tamanno de columnas que tiene la matriz"
read*,tam_log_columnas
! Pido por pantalla un valor por cada posición de la matriz
do i=1,tam_log_filas
  do j=1,tam_log_columnas
    print*,"Dame la posicion de la matriz :(",i,",",j,")."
    read*,matriz(i,j)
  enddo
enddo
! Ahora escribo por pantalla la matriz, haciéndolo por columnas en vez de filas para poder escribirla al completo y bien estructurada (las columnas son un do explícito)
print*,"Esta es la matriz:"
do i=1,tam_log_filas
  print*,matriz(i,(/(j,j=1,tam_log_columnas)/))
enddo

end program
