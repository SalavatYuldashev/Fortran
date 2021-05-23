CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/first src/first.f90

clean:
	rm -f obj/*
	rm -rf bin/*

run:
	cd ./bin; ./first;
