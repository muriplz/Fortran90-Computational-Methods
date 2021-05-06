


program hoja0ej4
real::input,output
read(*,*),input
call Subrutina(F(input),output)
write(*,*),output
end program



real FUNCTION F(input) result(output)
  REAL, INTENT(IN):: input
  output=0.5*(exp(input)-exp(-input))
  return
END FUNCTION F


subroutine Subrutina(F,output)
  REAL,INTENT(IN):: F
  REAL:: output
  output=F**2+(1/F)
end subroutine Subrutina
  






