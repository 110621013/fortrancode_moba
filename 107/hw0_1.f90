      program hw0_1
      implicit none

      integer :: input,i,num
      character :: w*15
!      data :: num(//)

      print*, "Please input an five-digit numbers."
      read(*,*) input

      do i=1,5
        if(i==1)then
          w=" ten thousands "
        elseif(i==2)then
          w="     thousands "
        elseif(i==3)then
          w="      hundreds "
        elseif(i==4)then
          w="          tens "
        elseif(i==5)then
          w="         units "
        endif

        num=mod(input/(10**(5-i)),10)
        print*,"The",w,"digit of ",input,"is",num

      enddo

!write(*,"(A4,A13,A10,I5,A4,I1)")"The ",trim(num(x))......

      stop
      end
