#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "primitives.h"
#include "sins_types.h"
#include "sins_const.h"
#include "primitives_utils.h"
#include "box.h"


void allocByteField( __bytefield__ *f, size_t num, size_t size ) {
    f->field = (void*)calloc(num, size);
    f->fieldsize = num*size;
    f->next = 0;
}

void* allocBlock( __bytefield__ *f, __WORD__ size ) {
    void *b;
    __WORD__ misalign;

    if ((f->next + size) > f->fieldsize) {
        return NULL;
    }
    else {
        b = (void*)(f->field + f->next);

        f->next = f->next + size;
        misalign = (f->next % __WORDSIZE__);
        if (misalign != 0)
            f->next = f->next + (__WORDSIZE__ - misalign);

        return b;
    }
}

__BWORD__ __getcar( __pair__ *p ) {
    return p->car;
}

__BWORD__ __getcdr( __pair__ *p ) {
    return p->cdr;
}

void __setcar( __pair__ *p, __BWORD__ newcar ) {
    p->car = newcar;
}

void __setcdr( __pair__ *p, __BWORD__ newcdr ) {
    p->cdr = newcdr;
}

__pair__*  __cons( __bytefield__ *f, __BWORD__ car, __BWORD__ cdr ) {
    __pair__ *newpair = NULL;

    newpair = (__pair__*)allocBlock(f, __PAIRSIZE__);

    if (newpair == NULL)
        return newpair;
    else {
        __setcar(newpair, car);
        __setcdr(newpair, cdr);
        return newpair;
    }
}

__vector__* __vector( __bytefield__ *f, __WORD__ size) {
    __vector__ *newvector = NULL;
    newvector = (__vector__*)allocBlock(f, __WORDSIZE__*(size + 1));

    if (newvector == NULL)
        return NULL;
    else {
        newvector->size = size;
        return newvector;
    }
}

__BWORD__ __vector_ref( __vector__* v, __WORD__ ref) {
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

void __vector_set( __vector__* v, __WORD__ ref, __BWORD__ val) {
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


__BWORD__ __add(__BWORD__ a, __BWORD__ b) {
    return a+b;
}
