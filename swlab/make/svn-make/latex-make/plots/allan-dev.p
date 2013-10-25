set terminal postscript eps enhanced color
set output 'allan-dev.eps' 
set autoscale
set log x 2
set log y 10
set mytics 9
unset label
set xtic (0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,60)
set ytic (0.001,0.01,0.1,1)
set grid
set xr[0.01:60]
set yr[0.001:1]
   set key at 50,0.9
set xlabel "Averaging Interval(sec)"
set ylabel "Allan Deviation" 
   plot "allan-dev.dat" using 1:2 title 'Measured' with linespoints lt -1 lw 2 pt 9,\
        "allan-dev.dat" using 1:4 title 'Uniform' with linespoints lt 12 lw 3 lc -1  pt 5
	
	  
