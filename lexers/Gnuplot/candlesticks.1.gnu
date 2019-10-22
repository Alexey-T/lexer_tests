# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'candlesticks.1.png'
set style increment default
set title "candlesticks with open boxes (default)" 
set xrange [ 0.00000 : 11.0000 ] noreverse nowriteback
set x2range [ * : * ] noreverse writeback
set yrange [ 0.00000 : 10.0000 ] noreverse nowriteback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
## Last datafile plotted: "candlesticks.dat"
plot 'candlesticks.dat' using 1:3:2:6:5 with candlesticks
