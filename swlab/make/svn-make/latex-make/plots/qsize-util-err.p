set terminal postscript eps enhanced color
set output 'qsize-util-err.eps' 
set autoscale
unset log
unset label
set xtic auto 
set y2tic auto nomirror tc lt -1
set grid
set xr[0:20]
set y2r[60:100]
set yrange[0:20]
set ytics auto nomirror tc lt -1
   set key at 18,8.8
set xlabel "Queue Size"
set ylabel "Queue Overflow(%)"
set y2label "Utilization(%)" 
   plot  "01.dat" using 1:($3*100) axes x1y1 title 'QO:Err 01%' with linespoints lt 12 lw 3 pt 5 ps 1.2,\
"05.dat" using 1:($3*100) axes x1y1 title 'QO:Err 05%' with linespoints lt 12 lw 3 pt 9 ps 1.3,\
	 "10.dat"using 1:($3*100) axes x1y1 title 'QO:Err 10%' with linespoints lt 12 lw 3 pt 7 ps 1.2,\
"20.dat" using 1:($3*100) axes x1y1 title 'QO:Err 20%' with linespoints lt 12 lw 3 pt 3 ps 1.2,\
"01.dat"  using 1:($2*100) axes x1y2 title 'Util:Err 01%' with linespoints lt 1 lw 3 pt 4 ps 1.2,\
"05.dat" using 1:($2*100) axes x1y2 title 'Util:Err 05%' with linespoints lt 1 lw 3 pt 9 ps 1.6,\
"10.dat" using 1:($2*100) axes x1y2 title 'Util:Err 10%' with linespoints lt 1 lw 3 pt 6 ps 1.3,\
"20.dat" using 1:($2*100) axes x1y2 title 'Util:Err 20%' with linespoints lt 1 lw 3 pt 2 ps 1.2
	 
	  
