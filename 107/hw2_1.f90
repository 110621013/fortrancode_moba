      program hw2_1 !hw1-2 Q3.3
      implicit none

      real :: epsilon=1.0


      do
        print*,epsilon 
        if(epsilon+1<=1)exit
        epsilon=epsilon/2
      enddo
      epsilon=epsilon*2

      print*,"my computer's machine epsilon is:",epsilon

      stop
      end

