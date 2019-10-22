# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'fillcrvs.7.png'
set format x "" 
set format y "" 
set grid nopolar
set grid xtics nomxtics ytics nomytics noztics nomztics nortics nomrtics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault   lt 0 linecolor 0 linewidth 0.500,  lt 0 linecolor 0 linewidth 0.500
set style increment default
set object  1 rect from graph 0, 0 to graph 1, 1
set object  1 behind clip lw 1.0  dashtype solid fc  rgb "#afffff"  fillstyle   solid 1.00 border lt -1
set title "world.dat plotted with filledcurves" 
set xrange [ -180.000 : 180.000 ] noreverse nowriteback
set x2range [ * : * ] noreverse writeback
set yrange [ -70.0000 : 80.0000 ] noreverse nowriteback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
set lmargin  1
## Last datafile plotted: "world.dat"
plot 'world.dat' with filledcurve notitle fs solid 1.0 lc rgb 'dark-goldenrod'
