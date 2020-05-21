>>x=[0:0.25*pi:2*pi]
>>y=sin(x).^2
>>xx = 0:0.05*pi:2*pi
>>yy = spline(x,y,xx);
>>plot(x,y, 'o' ,xx,yy)

