# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'fillcrvs.5.png'
set grid nopolar
set grid xtics nomxtics ytics nomytics noztics nomztics nortics nomrtics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid front   lt 0 linecolor 0 linewidth 0.500,  lt 0 linecolor 0 linewidth 0.500
set key outside right top vertical Right noreverse enhanced autotitle nobox
set style increment default
set title "Some sqrt stripes on filled graph background" 
set xrange [ * : * ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set yrange [ * : * ] noreverse writeback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
plot [0:10] [-8:6] 	-8 with filledcurve x2 lt 15, 	sqrt(x) with filledcurves y1=-0.5, 	sqrt(10-x)-4.5 with filledcurves y1=-5.5
