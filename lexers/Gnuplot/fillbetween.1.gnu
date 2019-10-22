# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'fillbetween.1.png'
set style increment default
set style data lines
set title "Fill area between two curves" 
set xrange [ 10.0000 : * ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set yrange [ 0.00000 : 175.000 ] noreverse nowriteback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
## Last datafile plotted: "silver.dat"
plot 'silver.dat' u 1:2:3 "%lf %lf %lf" w filledcu,       '' u 1:2 lt -1 notitle, '' u 1:3 lt -1 notitle
