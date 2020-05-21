      program homework1
      implicit none
!
!     program to illustrate read/write of a direct access file
!
      integer :: i,j,k,nrt,nslp,navgslp
      integer,parameter :: ni=71,nj=51,nk=10,nday=31,ntime=4
      real ::  RH(ni,nj),t3(ni,nj,nk),p(nk),th(ni,nj),th3(ni,nj,nk),temp(ni,nj),slp(ni,nj),total1,slp2(ni,nj,4),avgslp(ni,nj,2)
      character*80  ::input
      data p/1000.,850.,700.,500.,400.,300.,250.,200.,150.,100./
!
!
      input='/homework/106/GrADS/may2005.dat'
      open(10,file=input,status='unknown',form='unformatted', &
     &        access='direct',recl=4*ni*nj)

      open(20,file='106601520_aveSLP.dat',status='unknown',form='unform&
     &atted',access='direct',recl=4*ni*nj)

      total1= 0
      nslp  = 0
      nrt   = 51*4*(16-1) + 51*(3-1) + 10*(5) + 1
 
      read(10,rec=nrt) ((slp(i,j),i=1,ni),j=1,nj)
      do i=1,ni
        do j=1,nj
          if(slp(i,j)/=9.999E+20) then
            total1=total1+slp(i,j)
            nslp=nslp+1
          end if
        end do
      end do
      print*,'Average slp=',total1/float(nslp)
        
      nrt = 51*4*(16-1) + 51*(1-1) + 10*(5) + 1

      do k=1,4
        read(10,rec=nrt+(k-1)*51) ((slp2(i,j,k),i=1,ni),j=1,nj)
      end do

      navgslp=0

      do i=1,ni
        do j=1,nj
          do k=1,4
            if(slp2(i,j,k)/=9.999E+20) then
              avgslp(i,j,1)=avgslp(i,j,1)+slp2(i,j,k)
              navgslp=navgslp+1
            end if
          end do
          avgslp(i,j,1)=avgslp(i,j,1)/float(navgslp)
          navgslp=0
        end do
      end do

      write(20,rec=1)((avgslp(i,j,1),i=1,ni),j=1,nj)

        
      close (10)
      close (20)

      stop
      end

