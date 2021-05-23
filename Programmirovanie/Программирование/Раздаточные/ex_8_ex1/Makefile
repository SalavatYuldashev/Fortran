CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/ex_8_ex1 src/ex_8_ex1.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./ex_8_ex1
	cat ./bin/output.txt
