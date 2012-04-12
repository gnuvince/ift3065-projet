#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "primitives.h"
#include "sins_types.h"
#include "sins_const.h"
#include "primitives_utils.h"
#include "box.h"

void allocByteField( size_t num, size_t size ) {
    f.field = (void*)calloc(num, size);
    f.fieldsize = num*size;
    f.next = 0;
}

void freeByteField( ) {
    free(f.field);
}

void* allocBlock( __WORD__ size ) {
    void *b;
    __WORD__ misalign;

    if ((f.next + size) > f.fieldsize) {
        return NULL;
    }
    else {
        b = (void*)(f.field + f.next);
        
        f.next = f.next + size;
        misalign = (f.next % __WORDSIZE__);
        if (misalign != 0)
            f.next = f.next + (__WORDSIZE__ - misalign);

        return b;
    }
}

__BWORD__ __getCar( __pair__ *p ) {
    return p->car;
}

__BWORD__ __getCdr( __pair__ *p ) {
    return p->cdr;
}

void __setCar( __pair__ *p, __BWORD__ newcar ) {
    p->car = newcar;
}

void __setCdr( __pair__ *p, __BWORD__ newcdr ) {
    p->cdr = newcdr;
}

__pair__*  __cons( __BWORD__ car, __BWORD__ cdr ) {
    __pair__ *newpair = NULL;
    
    newpair = (__pair__*)allocBlock(__PAIRSIZE__);
    
    if (newpair == NULL)
        return newpair;
    else {
        __setCar(newpair, car);
        __setCdr(newpair, cdr);
        return newpair;
    }
}

__vector__* __vector( __WORD__ size) {
    __vector__ *newvector = NULL;
    newvector = (__vector__*)allocBlock(__WORDSIZE__*(size + 1));

    if (newvector == NULL)
        return NULL;
    else {
        newvector->size = size;
        return newvector;
    }
}

__BWORD__ __vectorRef( __vector__* v, __WORD__ ref) {
    if (v == NULL) {
        printf("Invalid vector");
        exit(-1);
    }
        
    if (ref >= 0 && ref < v->size)
        return (__BWORD__)*(((__BWORD__*)v) + ref + 1);
    else {
        printf("Error: Invalid vector index: %llu\n", (bword_t)ref);
        exit(-1);        
    }
}

void __vectorSet( __vector__* v, __WORD__ ref, __BWORD__ val) {
    __BWORD__ *pos;
    if (v == NULL) {
        printf("Invalid vector");
        exit(-1);
    }

    if (ref >= 0 && ref < v->size)
        *(((__BWORD__*)v) + ref + 1) = val;
    else {
        printf("Error: Invalid vector index: %llu\n", (bword_t)ref);
        exit(-1);
    }
}

__WORD__ __vectorLength( __BWORD__ v ) {
    return (*(__unboxptd(v)) >> __VEC_LEN_SHFT__);
}

__BWORD__ __add( __BWORD__ a, __BWORD__ b ) {
    return a + b;
}

__BWORD__ __sub( __BWORD__ a, __BWORD__ b ) {
    return a - b;
}

__BWORD__ __mul( __BWORD__ a, __BWORD__ b ) {
    return __box(__unbox(a) * __unbox(b));
}

__BWORD__ __quotient( __BWORD__ a, __BWORD__ b ) {
    return __box(__unbox(a) / __unbox(b));
}

__BWORD__ __remainder( __BWORD__ a, __BWORD__ b ) {
    return __box(__unbox(a) % __unbox(b));
}

int __number_p( __BWORD__ n ) {
    return __boxint_p(n);
}

int __pair_p( __BWORD__ p ) {
    return __boxpair_p(p);
}

int __lt( __BWORD__ a, __BWORD__ b ) {
    return a < b;
}

int __gt( __BWORD__ a, __BWORD__ b ) {
    return a > b;
}

int __ge( __BWORD__ a, __BWORD__ b ) {
    return a >= b;
}

int __le( __BWORD__ a, __BWORD__ b ) {
    return a <= b;
}

int __equalPtd( __BWORD__ a, __BWORD__ b ) {
    switch(__boxsubtype(a)) {
    case __VEC_TYPE__:
        break;
    case __STR_TYPE__:
        return 0;
    default:
        return 0;
    }
}

int __equal( __BWORD__ a, __BWORD__ b ) {
    if(__boxtype(a) != __boxtype(b))
        return 0;

    if(__boxtype(a) == __PTD_TYPE__)
        return __equalPtd(a, b);
    else
        return a == b;
}

int __eq( __BWORD__ a, __BWORD__ b ) {
    return a == b;
}

