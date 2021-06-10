module Gaussy
implicit none
contains
  subroutine SolucionGauss(A,b)
    real*8:: A(:,:),b(:),c
    integer:: n,k,i,j
    n=size(A,1)
    call PivoteParcial(A)
    do j=1,n
      do i=1,n
        if(i>j)then
          c=A(i,j)/A(j,j)
          do k=1,n+1
            A(i,k)=A(i,k)-c*A(j,k)
          enddo
          b(i)=b(i)-c*b(1)
        endif
      enddo
     enddo
      
  end subroutine SolucionGauss
  subroutine PivoteParcial(A)
    real*8::A(:,:),mayor,mayorC
    real*8,allocatable::temp(:)
    integer::n,i,j,mayorFila
    
    n=size(A,1)
    do i=1,n
      mayorC=0
      do j=1,n
        if(mayorC<abs(A(i,j)))then
          mayorC=A(i,j)
        endif
      enddo
      if(mayor<abs(A(i,1)/mayorC))then
        mayor=A(i,1)/mayorC
        mayorFila=i
      endif
    enddo

    allocate(temp(n))
    temp=A(mayorFila,:)
    A(mayorFila,:)=A(1,:)
    A(1,:)=temp
    deallocate(temp)
  end subroutine
end module
