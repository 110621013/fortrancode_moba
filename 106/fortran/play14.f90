      program ArrayTest1
      implicit none
      integer,parameter :: max=5
      integer ::score(max)
      integer :: stu_number
      integer :: i

      write(*,*)"How many students in your class:"
      read(*,*)stu_number

      do i=1,stu_number
      print*,"Please input number",i,"student's grads:"
      read(*,*)score(i)
      end do
      
      stop
      end
