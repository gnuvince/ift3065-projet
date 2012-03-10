# File: makefile

CWD = `pwd`

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test

all: check

check:
	@gsi ${CWD}/tests/test_simplify.scm -e '(main)'
