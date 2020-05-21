      program finalexam3
      implicit none
      integer :: i,j,record
      integer,dimension(24) :: num
      real,dimension(24) :: rh
!      character :: outfile
!------------------------------------------------------------------------------
      open(11,file='/homework/106/EXAM/exam3.txt')
      do i=1,24
        read(11,*) num(i),rh(i)
      enddo
      close(11)
!------------------------------------------------------------------------------
      open(13,file='106601015_exam3.dat',form='unformatted',access='direct',recl=4*1*1)

      record=0 
      do j=1,24
        record=record+1
        write(13,rec=record)rh(j)
      enddo

      stop
      end

