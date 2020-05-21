      program finalexam1
      implicit none

      integer :: i!,j!,l,z,m,nlev,nflag,st,et,days,record
      real,dimension(15) :: score
!      integer,dimension(76,8760) :: time
!      real,dimension(76) :: lat,lon
!      real,dimension(76,8760) :: temp,ch4,co,nmhc,no1,no2,nox,o3,pm10,pm25,rain,rh,so2,thc,wd,ws,u,v
!      character,dimension(76) :: stn*10,nn*6
      character,dimension(15) :: n*3,id*8,trash*2,namela*6,sinla*2
!      character*100,dimension(76) :: infile
!      integer,dimension(24*365) :: yy,mm,dd,hh
!      real :: k=0.0,n=0.0,avg=0.0,aven=0.0
!-------------------------------------------------------------------------------------------------
      open(10,file='/homework/106/EXAM/exam1.dat')
      read(10,*)
      do i=1,15
        read(10,*) n(i),id(i),namela(i),sinla(i),score(i)
!print*,n(i),"////",id(i),"////",namela(i),"////",score(i)

      end do
      close(10)
!print*,id(i),namela(i),score(i)
!-------------------------------------------------------------------------------------------------
      do i=1,15
        if(score(i).lt.60)then
          print*,id(i),"   ",trim(namela(i)),", ",sinla(i),score(i)
        endif
      enddo

      stop
      end

