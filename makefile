# File: makefile

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test

all: compile check

compile:
	@cd src/backend; make ARCH32=1 test_prim

check:
	@cd src/frontend; make check
	@cd src/backend; make check

clean:
	@cd src/backend; make clean
