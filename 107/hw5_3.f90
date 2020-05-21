      program hw5_2
      implicit none

      integer :: i,j,k,l,m,n=3,irow
      real*8 :: big,aa(n,n),bb(n,n),dum,b(n),x(n)

      aa(1,1)=15
      aa(1,2)=-3
      aa(1,3)=-1

      aa(2,1)=-3
      aa(2,2)=18
      aa(2,3)=-6

      aa(3,1)=-4
      aa(3,2)=-1
      aa(3,3)=12

      b(1)=3300
      b(2)=1200
      b(3)=2400

      if(n==1)then
        bb(1,1)=1.0/aa(1,1)
        go to 900
      endif

      do i=1,n
        do j=1,n
          bb(i,j)=0.0
        enddo
        bb(i,i)=1.0
      enddo


      do i=1,n
        big=aa(i,i)
        do j=i,n
          if(aa(j,i)>big)then
            big=aa(j,i)
            irow=j
          endif
        enddo

        if(big>aa(i,i))then
          do k=1,n
            dum=aa(i,k)
            aa(i,k)=aa(irow,k)
            aa(irow,k)=dum
            dum=bb(i,k)
            bb(i,k)=bb(irow,k)
            bb(irow,k)=dum
          enddo
        endif

        dum=aa(i,i)
        do j=1,n
          aa(i,j)=aa(i,j)/dum
          bb(i,j)=bb(i,j)/dum
        enddo

        do j=i+1,n
          dum=aa(j,i)
          do k=1,n
            aa(j,k)=aa(j,k)-dum*aa(i,k)
            bb(j,k)=bb(j,k)-dum*bb(i,k)
          enddo
        enddo

      enddo

      do i=1,n-1
        do j=i+1,n
          dum=aa(i,j)
          do l=1,n
            aa(i,l)=aa(i,l)-dum*aa(j,l)
            bb(i,l)=bb(i,l)-dum*bb(j,l)
          enddo
        enddo
      enddo

      900 continue
print*,"(A):"
print*,"A^-1:"
do i=1,n
print*,bb(i,1),bb(i,2),bb(i,3)
enddo

      do i=1,n
        do j=1,n
          x(i)=x(i)+bb(i,j)*b(j)
        enddo
      enddo
print*,"(B):"
print*,"X:"
print*,x(1),x(2),x(3)

      do i=1,n
        b(i)=0
      enddo

      x(1)=x(1)+10

      aa(1,1)=15
      aa(1,2)=-3
      aa(1,3)=-1

      aa(2,1)=-3
      aa(2,2)=18
      aa(2,3)=-6

      aa(3,1)=-4
      aa(3,2)=-1
      aa(3,3)=12

      do i=1,n
        do j=1,n
          b(i)=b(i)+aa(i,j)*x(j)
        enddo
      enddo
print*,"(C):"
print*,"B:"
print*,b(1),b(2),b(3)

      b(1)=3300
      b(2)=1200
      b(3)=2400

      do i=1,n
        x(i)=0
      enddo

print*,"(D):"
      b(1)=b(1)-700
      b(2)=b(2)-350

      do i=1,n
        do j=1,n
          x(i)=x(i)+bb(i,j)*b(j)
        enddo
      enddo

print*,"X:"
print*,x(1),x(2),x(3)


      stop
      end
