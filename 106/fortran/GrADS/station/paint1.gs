'reinit'
'open stationtest.ctl'
'set display color white'
'c'

tt=1
while(tt<25)
'enable print fig_'tt'.gmf'
'set t 'tt
tt=tt+1
endwhile


'set lon 119.3 122.2'
'set lat 21.5 25.5'
'set vpage 0 11 0 8.5'
'set parea 0.5 10.5 0.5 8'

'set gxout stnmark'
'set cmark 3'
'set digsize 0.25'
'set mpdset twncounty'
'd pm25'

'draw title gg'
'set gxout vector'
'set arrscl 0.5 10'
'd u;v'


