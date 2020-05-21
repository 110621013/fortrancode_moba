      program station
      implicit none
      integer :: i,j,k,nlev,nflag,yy,mm,dd,hh,st,et,days,record
      real :: pi
      integer,dimension(76,8760) :: time
      real,dimension(76) :: lat,lon
      real,dimension(76,8760) :: temp,ch4,co,nmhc,no1,no2,nox,o3,pm10,pm25,rain,rh,so2,thc,wd,ws,u,v
      character,dimension(76) :: stn*10,nn*6
      character*51 :: path
      character*100,dimension(76) :: infile
!--------------------------------------------------------------------------------------------------
      open(10,file='/homework/106/data/EPA_location.txt')
      do i=1,76
        read(10,*) stn(i),lat(i),lon(i)
        print*,stn(i)
      end do
!-------------------------------------------------------------------------------------------------
      do i=1,76
        path='/homework/106/data/'
        infile(i)=TRIM(path)//trim(stn(i))//'_2015.txt'
        print*,infile(i)
      end do
!-------------------------------------------------------------------------------------------------
      do i=1,76
        open(11,file=infile(i),form='formatted')
        read(11,*)
           do j=1,8760
              read(11,'(i10,18(f8.2))')time(j,i),temp(j,i),ch4(j,i),co(j,i),nmhc(j,i),no1(j,i),&
                 no2(j,i),nox(j,i),o3(j,i),pm10(j,i),pm25(j,i),rain(j,i),rh(j,i),so2(j,i),thc(j,i),wd(j,i),ws(j,i),u(j,i),v(j,i)
           enddo
      close(11)
      enddo

      do i=1,76
        j=8760
        write(*,'(A10,i10,18(f8.2))')stn(i),time(j,i),temp(j,i),ch4(j,i),co(j,i),nmhc(j,i),no1(j,i),&
                 no2(j,i),nox(j,i),o3(j,i),pm10(j,i),pm25(j,i),rain(j,i),rh(j,i),so2(j,i),thc(j,i),wd(j,i),ws(j,i),u(j,i),v(j,i)

      enddo

      stop
      end
