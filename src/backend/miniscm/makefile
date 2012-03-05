# File: makefile

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test

MINISCM_SOURCE = scanner.scm parser.scm miniscm.scm

all: miniscm.exe check

check:
	for t in test1 test2 test3; do \
	  $(MAKE) tests/$$t.test; \
	done

miniscm.exe: $(MINISCM_SOURCE)
	#$(MAKE) miniscm-compiled
	$(MAKE) miniscm-interpreted

miniscm-compiled:
	gsc -exe -o miniscm.exe -e '(load "lalr-scm/lalr.scm")' $(MINISCM_SOURCE)

miniscm-interpreted:
	rm -f miniscm.exe
	echo "#! /bin/sh"                           > miniscm.exe
	echo "gsi -:dar $(MINISCM_SOURCE) \$$*"    >> miniscm.exe
	chmod +x miniscm.exe

scanner.scm: scanner.l
	gsi silex/silex.scm -e '(lex "$*.l" "$*.scm")'

.scm.s: miniscm.exe
	./miniscm.exe $*.scm

.scm.test: miniscm.exe
	@echo "******************************************* TESTING $*.scm"
	./miniscm.exe $*.scm
	gcc -m32 -o $*.exe $*.s
	(./$*.exe || echo "exit status = $$?")

clean:
	rm -f scanner.scm *.exe *~ *.s scanner.c parser.c miniscm.c tests/*.s tests/*.exe
