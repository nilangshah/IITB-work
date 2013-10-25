LIBS=-lm
CC=cc
OBJ= c1.o c2.o c3.o
DEPS = c2.h

all: hello

hello: $(OBJ)
	$(CC) c1.o c2.o c3.o $(LIBS) -o hello

c1.o: c1.c $(DEPS)
	$(CC) -c c1.c -o c1.o

c2.o: c2.c $(DEPS)
	$(CC) -c c2.c -o c2.o

c3.o: c3.c $(DEPS)

.PHONY : clean

clean:
	rm -r *.o hello
