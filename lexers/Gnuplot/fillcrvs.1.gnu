# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'fillcrvs.1.png'
set key outside right top vertical Right noreverse enhanced autotitle nobox
set style increment default
set title "plot with filledcurve [options]" 
set xrange [ * : * ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set yrange [ * : * ] noreverse writeback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
plot [-10:10] [-5:3] 	1.5+sin(x)/x with filledcurve x2, 	sin(x)/x with filledcurve, 	1+sin(x)/x with lines, 	-1+sin(x)/x with filledcurve y1=-2, 	-2.5+sin(x)/x with filledcurve xy=-5,-4., 	-4.3+sin(x)/x with filledcurve x1, 	(x>3.5 ? x/3-3 : 1/0) with filledcurve y2
