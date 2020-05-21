      program midexam1
      implicit none

      real,dimension(24) :: temp
      integer :: i,j=0
      real :: k=0.0,avg=0.0,pk=0,king
!-------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------
      open(11,file='/homework/106/temp.txt')
      do i=1,24
      read(11,*)temp(i)
      enddo

      do i=1,24
        if(temp(i)/=-999.0)then
        j=j+1
        k=k+temp(i)
        endif
      enddo

      avg=k/j
      write(*,*)avg
      close(11)

      call bubble(temp)
      call maxtemp()
      stop
      end program midexam1

      subroutine maxtemp()                !副程式練習
      implicit none
      real,dimension(24) :: temp
      integer :: i,j=0
      real :: k=0.0,avg=0.0,pk=0,king

      open(11,file='/homework/106/temp.txt')
      do i=1,24
      read(11,*)temp(i)
      enddo

      do i=1,24
      pk=king-temp(i)
        if(pk<0.0)then
        king=temp(i)
        else            
        king=king
        endif
      enddo
      write(*,*)king
      return
      end

      subroutine bubble(temp)                !副程式練習
      implicit none

      real,dimension(24) :: temp
      integer :: i,j=0
      real :: k=0.0,avg=0.0,pk=0,king

      do i=24,1,-1
        do j=1,i
         if(temp(j)<temp(j+1))then
          pk=temp(j+1)
          temp(j+1)=temp(j)
          temp(j)=pk
         endif
        enddo
      enddo

      print*,(temp(i),i=1,25)
      return
      end

