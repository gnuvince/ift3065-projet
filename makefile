# File: makefile

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test

MINISCM_SOURCE = scanner.scm parser.scm miniscm.scm

all: miniscm.exe check

check:
	@cd src/frontend; make check
#	@cd src/ir; make check
#	@cd src/backend; make check


#	gsc -exe -o miniscm.exe -e '(load "lalr-scm/lalr.scm")' $(MINISCM_SOURCE)


# miniscm-interpreted:
# 	rm -f miniscm.exe
# 	echo "#! /bin/sh"                           > miniscm.exe
# 	echo "gsi -:dar $(MINISCM_SOURCE) \$$*"    >> miniscm.exe
# 	chmod +x miniscm.exe

# scanner.scm: scanner.l
# 	gsi silex/silex.scm -e '(lex "$*.l" "$*.scm")'

# .scm.s: miniscm.exe
# 	./miniscm.exe $*.scm

# .scm.test: miniscm.exe
# 	@echo "******************************************* TESTING $*.scm"
# 	./miniscm.exe $*.scm
# 	gcc -m32 -o $*.exe $*.s
# 	(./$*.exe || echo "exit status = $$?")

clean:
	@cd src/frontend; make clean
	@cd src/ir; make clean
	@cd src/backend; make clean

