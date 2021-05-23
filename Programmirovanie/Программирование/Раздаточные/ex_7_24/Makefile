CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/ex_7_24 src/ex_7_24.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./ex_7_24
	cat ./bin/output.txt
