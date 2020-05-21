      program  HE

      implicit none
      integer  ::  i
      real     ::  p(693)
      character  :: A(693)
!---------------------------------
      open(99,file='2.dat',form='Unformatted',access='direct',recl=4*1*1)
     
      do i = 1,693
      read(99,rec=i) p(i)
      print*,i,p(i)
      enddo
!-------------------------------
      close(99)
      stop 
      end

