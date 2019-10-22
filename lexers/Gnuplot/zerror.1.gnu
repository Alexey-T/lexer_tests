# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'zerror.1.png'
set border 127 front lt black linewidth 1.000 dashtype solid
set grid nopolar
set grid noxtics nomxtics noytics nomytics ztics nomztics nortics nomrtics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault   lt 0 linecolor 0 linewidth 0.500,  lt 0 linecolor 0 linewidth 0.500
set key at screen 0.9, 0.8 right top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid
set key noinvert samplen 4 spacing 1 width 0 height 1 
set key opaque
set style increment default
set style data lines
set xyplane at 1
set ztics  norangelimit logscale autofreq 
set title "splot with zerrorfill (no depth sorting)" 
set xrange [ * : * ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set yrange [ 0.500000 : 5.50000 ] noreverse nowriteback
set y2range [ * : * ] noreverse writeback
set zrange [ 1.00000 : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
set logscale z 10
## Last datafile plotted: "silver.dat"
splot for [k=1:5] 'silver.dat' using 1:(k):2:3 with zerror lt black fc lt k title "k = ".k
