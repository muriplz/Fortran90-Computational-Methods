module DeterminanteYAdjunto
implicit none
contains
  recursive function Determinante(A)result(det)
    real*8::A(:,:),det
    integer::i,j,asize(2),n
    real*8,allocatable:: adj(:,:)
    det=0.0
    asize=shape(A)
    if(asize(1)==asize(2))then
      n=asize(1)
    else
      print*,"No se puede hacer un determinante de una matriz que no es cuadrada"
      STOP
    endif
    if(n.eq.1)then
      det=A(1,1)
    elseif (n.eq.2)then
      det=A(1,1)*A(2,2)-(A(1,2)*A(2,1))
    else
      allocate(adj(n-1,n-1))
      do i=1,n
        adj=Adjunto(A,i,1)
        det = det + A(i,1)*(-1)**(1+i)*Determinante(adj)
      enddo
      deallocate(adj)
    endif
  end function Determinante
  
  function Adjunto(A,mI,mJ)result(adj)
    real*8,allocatable :: adj(:,:)
    real*8:: A(:,:)
    integer::mI,mJ,i,j,o,p,n,msize(2),m
    msize=shape(A)
    n=msize(1)
    m=msize(2)
    o=1
    p=0
    allocate(adj(n-1,m-1))
    do i=1,n
      if(i.ne.mI)then
        p=1
        do j=1,m
          if(j.ne.mJ)then
            adj(o,p)=A(i,j)
            p=p+1
          endif
        enddo
        o=o+1
      endif
    enddo
    end function Adjunto
end module
    
