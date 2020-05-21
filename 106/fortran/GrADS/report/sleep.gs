'reinit'
'set display color white'
'c'
qq=0
while(qq<59)
qq=qq+1
#'c'
'open 'qq'.ctl'
#'set grads on'
#'set vpage 0 11 0 8.5'

'set lev 0 30000'
#'set yaxis 0 30000'
'set vrange 0 360'
#'set ylint 2500'
#'set ylevs 0 5000 10000 15000 20000 25000'
'd wd'
'close 1'
#'printim wd_'qq'.gif'
endwhile
'printim qq.gif'
