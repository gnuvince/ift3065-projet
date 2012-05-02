#include <stdio.h>
#include "primitives_utils.h"
#include "sins_types.h"
#include "sins_const.h"
#include "primitives.h"
#include "box.h"

void print_bword(__BWORD__ v) {
    printf(wordFormat, (bword_t)v);
}

void print_pair(__BWORD__ p) {
    if (__null_p(_A1_, p) == __TRUE__)
        printf("()");
    else {
        printf("(");
        printf(wordFormat, __getCar(_A_(1), p));
        printf(" . ");
        printf(wordFormat, __getCdr(_A_(1), p));
        printf(")");
    }
}

/* void print_byteField(__bytefield__ *f) { */
/*     if (f == NULL) */
/*         printf("NULL byteField\n"); */
/*     else { */
/*         printf("Byte Field\n"); */
/*         printf("*field: %llu, fieldsize: %llu, next:%llu\n", (bword_t)f->field, (bword_t)f->fieldsize, (bword_t)f->next); */
/*     } */
/* } */

/* void print_vector(__BWORD__ v) { */
/*     if (__vector_p(_A1_, v) == __FALSE__) */
/*         printf("Illegal vector\n"); */
/*     else { */
/*         printf("Vector\n"); */
/*         printf("size: %llu, addr: %llu\n", __unboxint(_A1_, __vectorLength(_A1_, v)), __unboxptd(_A1_, v));         */
/*     } */
/* } */

/* void setBit_word( __WORD__ *w, __WORD__ mask ) { */
/*     *w = (*w | mask); */
/* } */

/* void setBit_byte( char *b, char mask ) { */
/*     *b = (*b | mask); */
/* } */
