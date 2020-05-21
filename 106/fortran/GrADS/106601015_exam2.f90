      program exam2
      implicit none
      integer :: i
      real,dimension(76) :: lat,lon
      character,dimension(76) :: stn*10
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      open(11,file='/homework/106/FORTRAN/EPA_location.txt')
      do i=1,76
        read(11,*) stn(i),lat(i),lon(i)
      enddo
      close(11)

!-------------------------------------------------------------------------------------
        do i = 1,76
           open(13,file='/home/106601015/fortran/GrADS/output/'//TRIM(stn(i))//'_2015.ctl')
        
        write(13,*) "dset ",trim(stn(i)),"_2015.dat"
        write(13,*) "undef  9.999E+20"
        write(13,*) "title ",trim(stn(i)),"_2015"
        write(13,*) "xdef 1 linear   1 1"
        write(13,*) "ydef 1 linear   1 1"
        write(13,*) "zdef 1 levels  1"
        write(13,*) "tdef 8760 linear  01Z01JAN2015 1hr"
        write(13,*) "vars  2"
        write(13,*) "ws       0  99   wind speed(m/s)"
        write(13,*) "temp     0  99   temperature"
        write(13,*) "endvars"
   
        close(13)
           
        enddo

      stop
      end

