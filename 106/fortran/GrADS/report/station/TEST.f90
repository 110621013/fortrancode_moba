      program  HE

      implicit none
      integer  ::  i
      real     ::  p(210)
      character  :: A(210)
!---------------------------------
      open(99,file='banqiao201411.dat',form='Unformatted',access='direct',recl=4*1*1)
     
      do i = 1,210
      read(99,rec=i) p(i)
      print*,i,p(i)
      enddo
!-------------------------------
      close(99)
      stop 
      end

