      program stationformgif!用來寫測站形式的dat

      implicit none
      integer :: i,j,k,nlev,nflag
      !integer :: yy,mm,dd,hh,sd,ed,sm,em,days,record
      real :: time
      real,dimension(76) :: lat,lon,bug
      real,dimension(30) :: num,stp,sfp,maxstp,minstp,temp,maxtemp,mintemp,td,ws,wd
      character,dimension(76) :: stn*10,stn11*8
      !character*51 :: path
      character*100 :: input,js,trash!infile(76),outfile
!------------------------------------------------------------------------------
      open(11,file='./EPA_location.txt')!路徑季的改
      do i=1,76
        read(11,*) stn(i),bug(i),lat(i),lon(i)
      enddo
      write(*,*)stn(i),bug(i),lat(i),lon(i)
      close(11)
!------------------------------------------------------------------------------
      num=0.
      do j = 1,1
         write(js,*)j
         input="10"//trim(js)//".txt"
         open (12,file=trim(input),form='formatted')
!板橋(2)
         do i = 1,30
           read (12,'(f7.1,f7.1,a1,f6.1,a1,f6.1,a9,f6.1,a9,f4.1,a3,f4.1,a11,f4.1,a11,f4.1)')num(i),stp(i),trash,sfp(i),trash,maxstp(i),&
trash,minstp(i),trash,temp(i),trash,maxtemp(i),trash,mintemp(i),trash,td(i)
!           read (12,'(f7.1,f7.1,a1,f6.1,a1,f6.1,a9,f6.1,a9,f4.1,a3,f4.1,a11,f4.1,a11,f4.1)')num(i,j),stp(i,j),trash,sfp(i,j),trash,maxstp(i,j),&
!trash,minstp(i,j),trash,temp(i,j),trash,maxtemp(i,j),trash,mintemp(i,j),trash,td(i,j)

         print*,num(i),stp(i),trash,sfp(i),trash,maxstp(i),&
trash,minstp(i),trash,temp(i),trash,maxtemp(i),trash,mintemp(i),trash,td(i)

!print*,num(i,j),stp(i,j),trash,sfp(i,j),trash,maxstp(i,j),&
!trash,minstp(i,j),trash,temp(i,j),trash,maxtemp(i,j),trash,mintemp(i,j),trash,td(i,j)

         enddo
         close(12)
      enddo
!!-------------------------------------------------------------------------------------
!      open(31,file='station.dat',form='unformatted')!測站都在一張資料裡,一檔案
!      do i=24*90+1,24*90+25                !!時間,天數
!        time=0.0
!        nlev=1
!        nflag=1
!        do j=1,76
!          write(stn11(j),'(i8)') j!在強制將測站名稱改成數字
!          print*,'00000000=',stn11(j)
!          write(31) stn11(j),lat(j),lon(j),time,nlev,nflag!一定要降寫
!          write(31) pm25(i,j),u(i,j),v(i,j),o3(i,j)
!          write(31) temp(i,j),ch4(i,j),co(i,j),nmhc(i,j),no(i,j),&
!               no2(i,j),nox(i,j),o3(i,j),pm10(i,j),pm25(i,j),rain(i,j),&
!               rh(i,j),so2(i,j),thc(i,j),wd(i,j),ws(i,j),u(i,j),v(i,j)
!        enddo
!        nlev=0!宣告這時間結束了
!        write(31) stn11(76),lat(76),lon(76),time,nlev,nflag!return nlev=0,這時間結束
!      enddo
!      write(31) stn11(76),lat(76),lon(76),time,nlev,nflag!回會有連續兩個nlev=0,資料結束
!      print*,'00000000=',stn11(76)
!      close(31)
!-------------------------------------------------------------


      stop
      end

