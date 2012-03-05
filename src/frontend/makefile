# File: makefile

CWD = `pwd`

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test

MINISCM_SOURCE = scanner.scm parser.scm miniscm.scm

all: miniscm.exe check

check:
	@gsi ${CWD}/tests/test_lexer.scm
	@gsi ${CWD}/tests/test_reader.scm
