all: hello

hello: c1.o c2.o c3.o
	cc c1.o c2.o c3.o -lm -o hello

c1.o: c1.c 
	cc -c c1.c -o c1.o

c2.o: c2.c
	cc -c c2.c -o c2.o

c3.o: c3.c

.PHONY: clean

clean:
	rm -r *.o hello
