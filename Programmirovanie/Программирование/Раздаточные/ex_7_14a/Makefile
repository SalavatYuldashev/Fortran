CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/ex_7_14a src/ex_7_14a.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./ex_7_14a
	cat ./bin/output.txt
