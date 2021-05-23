CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/ex_4_2a src/ex_4_2a.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./ex_4_2a
	cat ./bin/output.txt
