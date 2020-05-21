      program dazer
      implicit none

      real, dimension(122) :: pres,z,temp,td,wd,ws,rh
      integer :: sta,date,x,i,nrec

      open (1, file='4669902014081900.txt')
        do i=1,122
          read(1,*) sta,date,x,pres(i),z(i),temp(i),td(i),wd(i),ws(i),rh(i)
        enddo
      open (2, file='dazer.dat', status='unknown', form='unformatted',access='direct',&
                recl=4*1)
      nrec = 0
        do i =1,122
          nrec=nrec+1
          write(2,rec=nrec) wd(i)
        enddo
        do i=1,122
          nrec=nrec+1
          write(2,rec=nrec) ws(i)
        enddo
        do i=1,122
          nrec=nrec+1
          write(2,rec=nrec) rh(i)
        enddo
      close(1)  ; close(2)

      open(3,file='dazer.ctl')
      write(3,*) 'dset dazer.dat'
      write(3,*) 'undef -999.0'
      write(3,*) 'title auto-station observations'
      write(3,*) 'xdef    1    linear 1.0 1.0'
      write(3,*) 'ydef    1    linear 1.0 1.0'
      write(3,*) 'tdef    1    linear 00:00Z19AUG2014 1hr'
      write(3,*) 'zdef    122  levels '
        do i=1,122
          write(3,*)  z(i)
        enddo
      write(3,*) 'vars 3'
      write(3,*) 'wd 122 99 wind direction'
      write(3,*) 'ws 122 99 wind speed'
      write(3,*) 'rh   122 99 relative humidity'
      write(3,*) 'endvars'
      close(3) 

      stop
      end

