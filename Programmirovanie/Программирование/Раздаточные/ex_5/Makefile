CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/ex_5 src/ex_5.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./ex_5
	cat ./bin/output.txt
