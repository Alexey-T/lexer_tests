# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'fillcrvs.6.png'
unset border
set dummy t, y
set grid nopolar
set grid xtics nomxtics ytics nomytics noztics nomztics nortics nomrtics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault   lt 0 linecolor 0 linewidth 0.500,  lt 0 linecolor 0 linewidth 0.500
unset key
set label 1 "gnuplot" at 0.00000, 1.20000, 0.00000 center norotate front nopoint
set label 2 "gnuplot" at 0.0200000, -0.600000, 0.00000 center norotate front nopoint
set arrow 1 from -0.100000, 0.260000, 0.00000 to 0.180000, -0.170000, 0.00000 head front nofilled linecolor rgb "#f0e442"  linewidth 4.000 dashtype solid size first  0.100,40.000,90.000
set style increment default
set parametric
set size ratio 1 1,1
unset xtics
unset ytics
set title "Let's smile with parametric filled curves" 
set xrange [ -1.00000 : 1.00000 ] noreverse nowriteback
set x2range [ * : * ] noreverse writeback
set yrange [ -1.00000 : 1.60000 ] noreverse nowriteback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
plot [t=-pi:pi] 	sin(t),cos(t) with filledcurve xy=0,0 lt 15,		sin(t)/8-0.5,cos(t)/8+0.4 with filledcurve lt 3,		sin(t)/8+0.5,cos(t)/8+0.4 with filledcurve lt 3,		t/5,abs(t/5)-0.8 with filledcurve xy=0.1,-0.5 lt 1, 	t/3,1.52-abs(t/pi) with filledcurve xy=0,1.8 lt -1
