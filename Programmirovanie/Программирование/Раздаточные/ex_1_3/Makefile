CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/ex_1_3 src/ex_1_3.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./ex_1_3
	cat ./bin/output.txt
