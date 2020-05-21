      program hw1_1
      implicit none

      integer :: order,i,time

      print*, "Now you can expand cos(X)."
      print*, "How many order would you want??."
      read(*,*) order
      print*, "There is what you want!!"

      print*, "1"
      do i=1,order
        if(i<10)then
          time=mod(i,4)
          select case (time)
            case(0)
              write(*,"(A3,I1,A1,I1,A1)") "+X^",i,"/",i,"!"
            case(2)
              write(*,"(A3,I1,A1,I1,A1)") "-X^",i,"/",i,"!"
          end select

        elseif(i>=10 .and. i<100)then
          time=mod(i,4)
          select case (time)
            case(0)
              write(*,"(A3,I2,A1,I2,A1)") "+X^",i,"/",i,"!"
            case(2)
              write(*,"(A3,I2,A1,I2,A1)") "-X^",i,"/",i,"!"
          end select

        elseif(i>=100 .and. i<1000)then
          time=mod(i,4)
          select case (time)
            case(0)
              write(*,"(A3,I3,A1,I3,A1)") "+X^",i,"/",i,"!"
            case(2)
              write(*,"(A3,I3,A1,I3,A1)") "-X^",i,"/",i,"!"
          end select
        else
          time=mod(i,4)
          select case (time)
            case(0)
              write(*,"(A3,I4,A1,I4,A1)") "+X^",i,"/",i,"!"
            case(2)
              write(*,"(A3,I4,A1,I4,A1)") "-X^",i,"/",i,"!"
          end select
        endif
      enddo

!write(*,"(A4,A13,A10,I5,A4,I1)")"The ",trim(num(x))......

      stop
      end

