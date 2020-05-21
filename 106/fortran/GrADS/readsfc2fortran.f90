      implicit none
      integer :: i,j,k,nlev,nflag
      integer :: yy,mm,dd,hh,sd,ed,sm,em,days,record
      real :: pi,time
      real,dimension(76) :: lat,lon,pm25sum,pm25ave,num
      real,dimension(24*365,76) ::temp,ch4,co,nmhc,no,no2,nox,o3,pm10,&
                                 pm25,rain,rh,so2,thc,wd,ws,u,v
      character,dimension(76) :: stn*10,stn11*8,nn*6
      character*51 :: path
      character*100 :: infile(76),outfile
!------------------------------------------------------------------------------
      path='/homework/106/data'
!------------------------------------------------------------------------------
      sm=1
      em=1
      sd=1
      ed=31
!------------------------------------------------------------------------------
      open(11,file='/homework/106/data/EPA_location.txt')
      do i=1,76
        read(11,*) stn(i),lat(i),lon(i)
        infile(i)=trim(path)//'/'//trim(stn(i))//'_2015.txt'
!      print*,stn(i),lat(i),lon(i)
      enddo
      close(11)

      do j=1,76
      open (12,file=trim(infile(j)),form='formatted')
!         print*,infile(j)

      read (12,*)


         PM25sum(j)=0.

         do i = 1,24*365
        read (12,'(i4,i2,i2,i2,18(f8.2))')yy,mm,dd,hh,temp(i,j),ch4(i,j),co(i,j),nmhc(i,j),no(i,j),&
               no2(i,j),nox(i,j),o3(i,j),pm10(i,j),pm25(i,j),rain(i,j),&
               rh(i,j),so2(i,j),thc(i,j),wd(i,j),ws(i,j),u(i,j),v(i,j)


         IF ((mm.ge.sm).and.(mm.le.em)) then
             IF(PM25(i,j).ne.-999.00)then
                PM25sum(j)=pm25(i,j)+PM25sum(j)
                num(j)=num(j)+1
             endif
         endif

         enddo

         close(12)
         enddo
!-------------------------------------------------------------------------------------
      do j=1,76
         outfile=trim(stn(j))//'_2015.dat'
         open(13,file='./output/'//trim(outfile), &
&         form='unformatted',access='direct',recl=4*1*1)

      record=0
      do i=1,24*365
      record=record+1
      write(13,rec=record) ws(i,j)
      record=record+1
      write(13,rec=record) temp(i,j)
      enddo

      close(13)

      enddo

      stop
      end

