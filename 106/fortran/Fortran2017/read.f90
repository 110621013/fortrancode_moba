      program read
      implicit none
     
      character(len=15) :: text
!-----------------------------
      open(unit=10,file="try_read.txt")
      open(unit=99,file="output.txt")

      read(10,*)text       !input
      print*,text          !output
      write(99,*),text     !output

      close(10)
      close(99)
!-----------------------------
      stop
      end
