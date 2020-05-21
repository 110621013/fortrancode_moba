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

      do i=1,23
      pk=temp(i+1)-temp(i)
        if(pk>0.0)then
        king=temp(i+1)
        else
        king=temp(i)
        endif
      enddo
      write(*,*)king


      stop
      end
!--------------------------------

