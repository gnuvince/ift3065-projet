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
