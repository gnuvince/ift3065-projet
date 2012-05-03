# File: makefile

# WARNINGS := -Wall -Wextra -pedantic -Wshadow -Wpointer-arith -Wcast-align \
#             -Wwrite-strings -Wmissing-prototypes -Wmissing-declarations \
#             -Wredundant-decls -Wnested-externs -Winline -Wno-long-long \
#             -Wuninitialized -Wconversion -Wstrict-prototypes

ARCH32 ?= 1
ifeq ($(ARCH32), 1)
    CFLAGS =  -O1 -mpreferred-stack-boundary=2 -DARCH32 -m32 -g -std=c99 $(WARNINGS)
else
    CFLAGS =  -O1 -mpreferred-stack-boundary=2 -g -std=c99 $(WARNINGS)
endif

SHELL = /bin/sh

#CFLAGS := -g -std=c99 $(WARNINGS)

CC := gcc

CWD := `pwd`

BUILDDIR := tests
OBJDIR := tests
OBJFILES := bytefield.o bytefield_utils.o box.o gc.o primitives.o primitives_utils.o stack.o

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test .c .o

all: check

test_prim: test_primitives

test_primitives: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_primitives.exe test_primitives.c
	chmod +x $(CWD)/test_primitives.exe

test_gc: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -o test_gc.exe test_gc.c
	chmod +x $(CWD)/test_gc.exe

test_stack: $(OBJFILES)
	$(CC) $(CFLAGS) $(OBJFILES) -fPIE -Wl,-pie -o test_stack.exe test_stack.c
	chmod +x $(CWD)/test_stack.exe

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

run_gc:
	$(CWD)/test_gc.exe

run_prim:
	$(CWD)/test_primitives.exe

run_stack:
	$(CWD)/testStack.exe

check:
	python run_backend_tests.py

clean:
	-rm $(CWD)/*.o
	-rm $(CWD)/*.s
	-rm $(CWD)/*.S
	-rm -rf $(CWD)/*dSYM

cleanall: clean
	-rm $(CWD)/*.exe

.PHONY: clean check run cleanall run_gc run_prim run_stack test_prim test_primitives test_gc test_stack
