      implicit none
      integer :: i,j,k,timechange,conslevnum,levnum,record
      real,dimension(99,59) :: num,wtf,h,rh,wd,p,t,td,ws
      character*100 :: trash,outputfile,levnum2,m,n,time,conslevnum2,outputfile2,outputfile3
      !character*16  :: time

      open (12,file='input.txt',form='formatted')

      do timechange = 1,59
!        open (12,file='input.txt',form='formatted')
        if(timechange<10)then
          write(outputfile,'(i1)')timechange
        else
          write(outputfile,'(i2)')timechange
        endif
        open(14,file=trim(outputfile)//'.txt')
        open(15,file=trim(outputfile)//'.dat',form='unformatted',access='direct',recl=4*1*1)
        do i = 1,2
          read (12,*)
        enddo
        read (12,'(a37,i3)')trash,conslevnum
        !print*,conslevnum
        do i = 1,5
          read (12,*)
        enddo

        levnum=conslevnum
        if(levnum>99) then
          levnum=99
        endif

        do j = 1,levnum
          read (12,'(f8.0,f8.0,f8.0,f8.0,f8.0,f8.0,f8.0,f8.0,f8.0)')num(j,timechange),wtf(j,timechange),p(j,timechange),h(j,timechange),t(j,timechange),&
          rh(j,timechange),td(j,timechange),wd(j,timechange),ws(j,timechange)
          write(14,'(f8.0,f8.0,f8.0,f8.0,f8.0,f8.0,f8.0))')p(j,timechange),h(j,timechange),t(j,timechange),&
          rh(j,timechange),td(j,timechange),wd(j,timechange),ws(j,timechange)
        enddo

        record=0
        do j = 1,levnum
          record=record+1
          write(15,rec=record)p(j,timechange)
        enddo
        record=levnum
        do j = 1,levnum
          record=record+1
          write(15,rec=record)h(j,timechange)
        enddo
        record=2*levnum
        do j = 1,levnum
          record=record+1
          write(15,rec=record)t(j,timechange)
        enddo
        record=3*levnum
        do j = 1,levnum
          record=record+1
          write(15,rec=record)rh(j,timechange)
        enddo
        record=4*levnum
        do j = 1,levnum
          record=record+1
          write(15,rec=record)td(j,timechange)
        enddo
        record=5*levnum
        do j = 1,levnum
          record=record+1
          write(15,rec=record)wd(j,timechange)
        enddo
        record=6*levnum
        do j = 1,levnum
          record=record+1
          write(15,rec=record)ws(j,timechange)
        enddo


        do k = 1,conslevnum-levnum+2
          read (12,*)
        enddo
        i=0
        j=0
        k=0

        close (14)
        close (15)
!        write(outputfile,'(i2)') timechange
        open(16,file=TRIM(outputfile)//'.ctl')
        if(timechange<10)then
            write(outputfile3,'(i1)')timechange
          else
            write(outputfile3,'(i2)')timechange
          endif

        write(outputfile2,*)"dset   ^/home/106601015/fortran/GrADS/report/"//trim(outputfile3)//".dat"

        write(16,*) trim(outputfile2)
        write(16,*) "undef  9.999E+20"
        write(16,*) "title making fxxking data"
        write(16,*) "xdef 1 linear   1 1"
        write(16,*) "ydef 1 linear   1 1"

        write(levnum2,'(i2)') levnum
        write(16,*) "zdef ",trim(levnum2)," levels "
        do i=1,levnum
          write(n,*)h(i,timechange)
          m=trim(n)//"."
          write(16,*)m
        enddo

        j=MOD(timechange,2)
        if(j==1)then
          k=timechange/2+1
          if(k<10)then
            write(time,'(i1)')k
            m="tdef 1 linear 00z0"//trim(time)//"nov2014 12hr"
            write(16,*) trim(m)
          else
            write(time,'(i2)')k
            m="tdef 1 linear 00z"//trim(time)//"nov2014 12hr"
            write(16,*) trim(m)
          endif
        else
          k=timechange/2+1
          if(k<10)then
            write(time,'(i1)')k
            m="tdef 1 linear 12z0"//trim(time)//"nov2014 12hr"
            write(16,*) trim(m)
          else
            write(time,'(i2)')k
            m="tdef 1 linear 12z"//trim(time)//"nov2014 12hr"
            write(16,*) trim(m)
          endif
        endif

        write(conslevnum2,*)levnum
        
        write(16,*)"vars  7"
        write(16,*)"p     ",trim(conslevnum2)," 00     air pressure"
        write(16,*)"h     ",trim(conslevnum2)," 00     geoheight"
        write(16,*)"temp  ",trim(conslevnum2)," 00     temperature"
        write(16,*)"rh    ",trim(conslevnum2)," 00     relative humility"
        write(16,*)"td    ",trim(conslevnum2)," 00     dewpoint temperature"
        write(16,*)"wd    ",trim(conslevnum2)," 00     wind direct"
        write(16,*)"ws    ",trim(conslevnum2)," 00     wind speed"
        write(16,*) "endvars"

        close(16)

      enddo
      close(12)
   
      print*,'susses'
      stop
      end

