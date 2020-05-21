      program person
      implicit none
!作業
      integer :: i,j=0
      character*25 :: path
      real :: nox=0.0,ch=0.0,ga=0
!------------------讀麥寮測站資料--------------------------------------------------------------------------------
      open(10,file='/home/106601015/data2.txt')
      read(10,*) 
      read(10,*) nox,path,ch
      close(10)
      
      write(*,*) nox,ch


      stop
      end
      !do i=1,76
      !  read(10,*) stn(i),lat(i),lon(i)
      !end do


