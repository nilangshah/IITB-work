all:allan-dev.eps qsize-util-err.eps

allan-dev.eps: allan-dev.p allan-dev.dat
	gnuplot allan-dev.p

qsize-util-err.eps: qsize-util-err.p 01.dat 20.dat 05.dat 10.dat
	gnuplot qsize-util-err.p

