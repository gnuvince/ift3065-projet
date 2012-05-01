# File: makefile

# WARNINGS := -Wall -Wextra -pedantic -Wshadow -Wpointer-arith -Wcast-align \
#             -Wwrite-strings -Wmissing-prototypes -Wmissing-declarations \
#             -Wredundant-decls -Wnested-externs -Winline -Wno-long-long \
#             -Wuninitialized -Wconversion -Wstrict-prototypes

ARCH32 ?= 1
ifeq ($(ARCH32), 1)
    CFLAGS = -O0 -DARCH32 -m32 -g -std=c99 $(WARNINGS)
else
    CFLAGS = -O0 -g -std=c99 $(WARNINGS)
endif

SHELL = /bin/sh

#CFLAGS := -g -std=c99 $(WARNINGS)

CC := gcc

CWD := `pwd`

BUILDDIR := tests
OBJDIR := tests
OBJFILES := bytefield.o bytefield_utils.o box.o primitives.o primitives_utils.o gc.o

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test .c .o

all: check

test_primitives32: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_primitives.exe test_primitives.c
	chmod +x $(CWD)/test_primitives.exe

test_gc32: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_gc.exe test_gc.c
	chmod +x $(CWD)/test_gc.exe

test_primitives: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_primitives.exe test_primitives.c
	chmod +x $(CWD)/test_primitives.exe

test_gc: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_gc.exe test_gc.c
	chmod +x $(CWD)/test_gc.exe

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

run_gc:
	$(CWD)/test_gc.exe

run_prim:
	$(CWD)/test_primitives.exe

check:
	python run_backend_tests.py

clean:
	-rm $(CWD)/*.o
	-rm -rf $(CWD)/*dSYM

cleanall: clean
	-rm $(CWD)/*.exe

.PHONY: clean check run cleanall
