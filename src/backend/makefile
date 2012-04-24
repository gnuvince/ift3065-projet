# File: makefile

# WARNINGS := -Wall -Wextra -pedantic -Wshadow -Wpointer-arith -Wcast-align \
#             -Wwrite-strings -Wmissing-prototypes -Wmissing-declarations \
#             -Wredundant-decls -Wnested-externs -Winline -Wno-long-long \
#             -Wuninitialized -Wconversion -Wstrict-prototypes
CFLAGS := -m32 -O0 -g -std=c99 $(WARNINGS)

CC := gcc

CWD := `pwd`

BUILDDIR := tests
OBJDIR := tests
OBJFILES := box.o primitives.o primitives_utils.o

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test .c .o

all: check

devtest: $(OBJFILES) test_primitives
	chmod +x ${CWD}/test_primitives
	${CWD}/test_primitives

test_primitives:
	$(CC) $(CFLAGS) $(OBJFILES) -o test_primitives test_primitives.c

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

# check: $(OBJFILES) clean
# 	-@echo "Nothing for now"

check:
	python run_backend_tests.py

clean:
	-@rm ${CWD}/*.o

.PHONY: clean
