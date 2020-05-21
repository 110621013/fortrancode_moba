      program diracc
      implicit none
!
!     program to illustrate read/write of a direct access file

      integer :: ni,nj,nk,nday,ntime,nrt,i,j,k
      parameter(ni=71,nj=51,nk=10,nday=31,ntime=4)
      real ::  RH(ni,nj),t3(ni,nj,nk),p(nk),th(ni,nj),th3(ni,nj,nk),temp(ni,nj)
      character*80  input
      data p/1000.,850.,700.,500.,400.,300.,250.,200.,150.,100./
!
!
      input='/homework/106/GrADS/may2005.dat'
      open(10,file=input,status='unknown',form='unformatted', &
     &        access='direct',recl=4*ni*nj)

      open(20,file='result.dat',status='unknown',form='unformatted',&
     &        access='direct',recl=4*ni*nj)
!
!     read RH for 12z 5/14/2005 (2-d array)
!
!
         nrt=51*4*(14-1) + 51*(3-1) + 10*(1) + 1
!                day         time     variable level
!
      do k=1,10
        read(10,rec=nrt) ((temp(i,j),i=1,ni),j=1,nj)
        print*,'temp=',temp(1,1)
        write(20,rec=k) ((temp(i,j),i=1,ni),j=1,nj)
        nrt=nrt+1
      enddo


      do k=1,10
        read(10,rec=nrt) ((RH(i,j),i=1,ni),j=1,nj)
        print*,'RH=',RH(1,1)
        write(20,rec=k+10) ((RH(i,j),i=1,ni),j=1,nj)
        nrt=nrt+1
      enddo

!
      close (10)
      close (20)

      stop
      end
