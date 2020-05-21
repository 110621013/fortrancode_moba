      program hw0_2
      implicit none

      integer :: i,total=0
      character :: first
      integer :: num,last

      print*,"Please input a ID"
      read(*,'(a1,i9)')first,last
!print*,num/1000
!------------原本想要進行長度除錯,但字串長度問題很難處理
      !do while(len(input)/=10)
      !  print*,"Your ID length is not correct"
      !  print*,"Please re-enter a ID"
      !  read(*,*)input
      !enddo
!----------------------------------------------------------

!!! read(number,"(I5)")x   轉換字串number=>整數x

!print*,ichar(first)
      if(ichar(first)>=65 .and. ichar(first)<=90)then
        num=ichar(first)-55
        total=(num/10)*1+(mod(num,10))*9
!print*,num,total     !14 37
        do i=8,1,-1
          total=total+mod((last/10**i),10)*i
!print*,total
        enddo
        total=total+mod(last,10)
!print*,total

        if(mod(total,10)==0)then
          print*,"pass"
        else
          print*,"fail"
        endif
      else if(ichar(first)>=97 .and. ichar(first)<=122)then
        num=ichar(first)-87
        total=(num/10)*1+(mod(num,10))*9
        do i=8,1,-1
          total=total+mod((last/10**i),10)*i
        enddo
        total=total+mod(last,10)

        if(mod(total,10)==0)then
          print*,"pass"
        else
          print*,"fail"
        endif

      else
        print*,"your id's first word is wrong!!"
      endif

      stop
      end
