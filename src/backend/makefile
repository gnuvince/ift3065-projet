# File: makefile
WARNINGS := -Wall -Wextra -pedantic -Wshadow -Wpointer-arith -Wcast-align \
            -Wwrite-strings -Wmissing-prototypes -Wmissing-declarations \
            -Wredundant-decls -Wnested-externs -Winline -Wno-long-long \
            -Wuninitialized -Wconversion -Wstrict-prototypes
CFLAGS := -g -std=c99 $(WARNINGS)

CC := gcc

CWD := `pwd`
BUILDDIR := tests
OBJDIR := tests

.SUFFIXES:
.SUFFIXES: .scm .s .exe .test .c .o

all: check

check:	primitives.o primitives.h primitives_utils.h sins_const.h \
		sins_types.h box.h
		@gcc -c ${CWD}/*.c
		@gcc -o ${CWD}/test_primitives.exe test_primitives.c
		chmod +x ${CWD}/test_primitives.exe
		${CWD}/test_primitives.exe

%.o: %.c Makefile
		@$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
		-@rm ${CWD}/*.o
		-@rm ${CWD}/*.d
		-@rm ${CWD}/a.out

