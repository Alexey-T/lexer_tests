# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'fillbetween.3.png'
set style fill   solid 1.00 noborder
set style increment default
set style data lines
set title "Fill area between two curves (above/below)" 
set xrange [ 250.000 : 500.000 ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set yrange [ * : * ] noreverse writeback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
## Last datafile plotted: "silver.dat"
plot 'silver.dat' u 1:2:($3+$1/50.) w filledcurves above title 'Above',                '' u 1:2:($3+$1/50.) w filledcurves below title 'Below', 	       '' u 1:2 lt -1 lw 2 title 'curve 1', 	       '' u 1:($3+$1/50.) lt 3 lw 2 title 'curve 2'
