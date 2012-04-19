#include <stdio.h>
#include "primitives_utils.h"
#include "sins_types.h"
#include "sins_const.h"
#include "primitives.h"
#include "box.h"

void print_bword(__BWORD__ v) {
    printf("%llu", (bword_t)v);
}

void print_pair(__BWORD__ p) {
    if (__null_p(p) == __TRUE__)
        printf("()");
    else {
        printf("(");
        printf("%llu . %llu", __getCar(p), __getCdr(p));
        printf(")");
    }
}

void print_byteField(__bytefield__ *f) {
    if (f == NULL)
        printf("NULL byteField\n");
    else {
        printf("Byte Field\n");
        printf("*field: %llu, fieldsize: %llu, next:%llu\n", (bword_t)f->field, (bword_t)f->fieldsize, (bword_t)f->next);
    }
}

void print_vector(__BWORD__ v) {
    if (__vector_p(v) == __FALSE__)
        printf("Illegal vector\n");
    else {
        printf("Vector\n");
        printf("size: %llu, addr: %llu\n", __unboxint(__vectorLength(v)), __unboxptd(v));        
    }
}

