# File: makefile

CWD = `pwd`

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test

all: check

check:
	@gsi ${CWD}/tests/test_lexer.scm -e '(main)'
	@gsi ${CWD}/tests/test_reader.scm -e '(main)'
	@gsi ${CWD}/tests/test_parser.scm -e '(main)'
