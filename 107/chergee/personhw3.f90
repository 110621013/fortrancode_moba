      program personhw3
      implicit none
      integer :: i,j=0
      character*10 :: month
      character*51 :: path
      character*100,dimension(12) :: infile

      real :: totala=0,totalb=0,totalc=0
      integer :: SunShinemaxm=0,GloblRadmaxm=0
      real,dimension(12) :: SunShinemax,GloblRadmax
      character*8 :: a,SunShinea,SunShineRatea,GloblRada
      real,dimension(31,12) :: SunShine,SunShineRate,GloblRad
!-------------------------------------------------------------------------------------------------
      path='/home/106601015/107/chergee/' !助教~path需更改為自己的位址喔!!
      infile(1)=TRIM(path)//"467440-2017-11.prn"
      infile(2)=TRIM(path)//"467440-2017-12.prn"

      do i=3,12
        if(i-2<10)then
          write(month,"(a1,i1)")"0",i-2
        else
          write(month,"(i2)")i-2
        endif
        infile(i)=TRIM(path)//"467440-2018-"//trim(month)//".prn"
      end do
!do i=1,12
!print*,infile(i)
!enddo
!-------------------------------------------------------------------------------------------------

      do i=1,12
        open(11,file=infile(i),form='formatted')
        read(11,*)

!print*,i
          if(i==4)then
            do j=1,28
              read(11,"(30(a8))")a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,SunShinea,SunShineRatea,GloblRada
              read(SunShinea,"(f8.2)")SunShine(j,i)
              read(SunShineRatea,"(f8.2)")SunShineRate(j,i)
              read(GloblRada,"(f8.2)")GloblRad(j,i)
            enddo
          elseif(i==1 .or. i==6 .or. i==8 .or. i==11)then
            do j=1,30
              read(11,"(30(a8))")a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,SunShinea,SunShineRatea,GloblRada
              read(SunShinea,"(f8.2)")SunShine(j,i)
              read(SunShineRatea,"(f8.2)")SunShineRate(j,i)
              read(GloblRada,"(f8.2)")GloblRad(j,i)
            enddo
          else
            do j=1,31
              read(11,"(30(a8))")a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,SunShinea,SunShineRatea,GloblRada
              read(SunShinea,"(f8.2)")SunShine(j,i)
              read(SunShineRatea,"(f8.2)")SunShineRate(j,i)
              read(GloblRada,"(f8.2)")GloblRad(j,i)
            enddo
          endif
        close(11)
      enddo

      do i=1,12
        do j=1,31
          if(SunShinemax(i)<SunShine(j,i))then
            SunShinemax(i)=SunShine(j,i)
          endif
          if(GloblRadmax(i)<GloblRad(j,i))then
            GloblRadmax(i)=GloblRad(j,i)
          endif
          if(i>=6 .and. i<=9)then
            totala=totala+SunShine(j,i)
          else
            totalb=totalb+SunShine(j,i)
          endif
          totalc=totalc+GloblRad(j,i)
        enddo
      enddo



do i=1,12
print*,"----------"
print*,"日照時數(hr)       日照率(%)       全天空日射量(MJ/㎡)"
do j=1,31
print*,SunShine(j,i),SunShineRate(j,i),GloblRad(j,i)
enddo
enddo

do i=1,12
print*,SunShinemax(i)
enddo
print*,"---"
do i=1,12
print*,GloblRadmax(i)
enddo
print*,"total 夏季日照時數:",totala
print*,"total 非夏季日照時數:",totalb
print*,"total 日射量:",totalc




      stop
      end
