# File: makefile

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test

all: compile

compile:
	@cd src/backend; make

check:
	@cd src/frontend; make check
	@cd src/backend; make check

clean:
	@cd src/backend; make clean
