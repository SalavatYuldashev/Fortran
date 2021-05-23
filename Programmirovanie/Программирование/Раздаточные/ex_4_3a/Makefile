CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/ex_4_3a src/ex_4_3a.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./ex_4_3a
	cat ./bin/output.txt
