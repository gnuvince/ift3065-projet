# File: makefile

# WARNINGS := -Wall -Wextra -pedantic -Wshadow -Wpointer-arith -Wcast-align \
#             -Wwrite-strings -Wmissing-prototypes -Wmissing-declarations \
#             -Wredundant-decls -Wnested-externs -Winline -Wno-long-long \
#             -Wuninitialized -Wconversion -Wstrict-prototypes
SHELL = /bin/sh

CFLAGS := -g -std=c99 $(WARNINGS)

CC := gcc

CWD := `pwd`

BUILDDIR := tests
OBJDIR := tests
OBJFILES := bytefield.o gc_utils.o box.o primitives.o primitives_utils.o gc.o

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test .c .o

all: check

devtest: $(TARGS)

test_primitives: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_primitives.exe test_primitives.c
	$(CHMOD) +x $(CWD)/test_primitives.exe

test_gc: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_gc.exe test_gc.c
	$(CHMOD) +x $(CWD)/test_gc.exe

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

run:
	$(CWD)/test_gc.exe
	$(CWD)/test_primitives.exe

check:
	python run_backend_tests.py

clean:
	-rm $(CWD)/*.o
	-rm -rf $(CWD)/*dSYM

cleanall:
	-rm $(CWD)/*.exe

.PHONY: clean check run cleanall
