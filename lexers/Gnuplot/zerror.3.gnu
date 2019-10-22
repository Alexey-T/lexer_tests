# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'zerror.3.png'
set style fill  transparent solid 0.75 noborder
set format z "%.1f" 
unset key
set style increment default
set view 66, 200, 1, 1
unset xtics
unset ytics
set title "fence plot constructed with zerrorfill" 
set xlabel "X axis" 
set xlabel  font "" textcolor lt -1 rotate parallel
set xrange [ * : * ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set ylabel "Y axis" 
set ylabel  font "" textcolor lt -1 rotate parallel
set yrange [ * : * ] noreverse writeback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
set pm3d depthorder 
sinc(u,v) = sin(sqrt(u**2+v**2)) / sqrt(u**2+v**2)
## Last datafile plotted: "+"
splot for [i=-5:4][y=-50:50:5] '+' using (i):($1/100.):(-1):(-1):(sinc($1/10., 1.+i)) with zerrorfill
