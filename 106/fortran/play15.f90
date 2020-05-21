      program ArrayTest2
      implicit none
      integer :: stu_number
      integer,allocatable :: score(:)
      integer :: i

      write(*,*)"How many students inyour class:"
      read(*,*)stu_number
      allocate(score(stu_number))

      do i=1,stu_number
      print*,"Please input number",i,"student's grads:"
      read(*,*) score(i)
      end do

      deallocate(score)
      stop
      end
