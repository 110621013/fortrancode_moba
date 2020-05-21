      program exam2_1
      implicit none

      integer :: i,j,nrt
      integer,parameter :: ni=71,nj=51
      real ::  temp(ni,nj),maxtemp,mintemp
      character*80  ::input

      input='/homework/106/GrADS/may2005.dat'
      open(10,file=input,status='unknown',form='unformatted', &
     &        access='direct',recl=4*ni*nj)

      nrt   = 51*4*(20-1) + 51*(1-1) + 10*(1) + 1

      read(10,rec=nrt) ((temp(i,j),i=1,ni),j=1,nj)
      maxtemp=temp(1,1)
      do i=1,ni
        do j=1,nj
          if(temp(i,j)/=9.999E+20) then
          if(maxtemp<temp(i,j))then
            maxtemp=temp(i,j)
          endif
          endif
        enddo
      enddo

      read(10,rec=nrt) ((temp(i,j),i=1,ni),j=1,nj)
      mintemp=temp(1,1)
      do i=1,ni
        do j=1,nj
          if(temp(i,j)/=9.999E+20) then
          if(mintemp>temp(i,j))then
            mintemp=temp(i,j)
          endif
          endif
        enddo
      enddo

      write(*,*) "max temperature at 00Z on 20 May 2005 is:",maxtemp
      write(*,*) "min temperature at 00Z on 20 May 2005 is:",mintemp

      close (10)

      stop
      end

