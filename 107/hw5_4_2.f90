      program hw5_4
      implicit none

      integer :: i,j,k,l,m,n=5,irow
      real :: big,aa(n,n),bb(n,n),dum,normaa=0,normbb=0,boss,billy

      aa(1,1)=1
      aa(1,2)=4
      aa(1,3)=9
      aa(1,4)=16
      aa(1,5)=25

      aa(2,1)=4
      aa(2,2)=9
      aa(2,3)=16
      aa(2,4)=25
      aa(2,5)=36

      aa(3,1)=9
      aa(3,2)=16
      aa(3,3)=25
      aa(3,4)=36
      aa(3,5)=49

      aa(4,1)=16
      aa(4,2)=25
      aa(4,3)=36
      aa(4,4)=49
      aa(4,5)=64

      aa(5,1)=25
      aa(5,2)=36
      aa(5,3)=49
      aa(5,4)=64
      aa(5,5)=81

print*,"A:"
do i=1,n
write(*,"(5f14.3)"),aa(i,1),aa(i,2),aa(i,3),aa(i,4),aa(i,5)
enddo

      do i=1,n
        billy=0
        do j=1,n
          if(aa(i,j)>billy)then
            billy=aa(i,j)
          endif
        enddo
        do j=1,n
          aa(i,j)=aa(i,j)/billy
        enddo
      enddo

print*,"after scale,A:"
do i=1,n
write(*,"(5f14.3)"),aa(i,1),aa(i,2),aa(i,3),aa(i,4),aa(i,5)
enddo

      do i=1,n
        boss=0
        do j=1,n
          boss=boss+aa(i,j)
        enddo
        if(boss>normaa)then
          normaa=boss
        endif
      enddo

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

      do i=1,n
        boss=0
        do j=1,n
          boss=boss+abs(bb(i,j))
        enddo
        if(boss>normbb)then
          normbb=boss
        endif
      enddo

print*,"A^-1:"
do i=1,n
write(*,"(5f15.3)"),bb(i,1),bb(i,2),bb(i,3),bb(i,4),bb(i,5)
enddo

print*,"normA=",normaa
print*,"normA^-1=",normbb
print*,"cond A:",normaa*normbb

      stop
      end

