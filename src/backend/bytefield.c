#include <stdlib.h>
#include <stdio.h>
#include "bytefield.h"
#include "gc.h"
#include "sins_types.h"

__bytefield__ bf_A;
__bytefield__ bf_B;
__bytefield__ *heap = &bf_A;
__bytefield__ *newheap = &bf_B;

__BWORD__ varroots[__VARNUM__];
__VAR__ varnext = 0;

__bytefield__* getHeap() {
    return heap;
}

__bytefield__* getNewHeap() {
    return newheap;
}

__VAR__ getVarNext() {
    return varnext;
}

void swapHeap() {
    if (heap == &bf_A) {
        heap = &bf_B;
        newheap = &bf_A;
    }
    else {
        heap = &bf_A;
        newheap = &bf_B;
    }
}

void allocByteField( __bytefield__ *f, size_t wordsize ) {
    f->field = (void*)calloc((size_t)__FIELDSIZE__, wordsize);
    if (f->field == NULL) {
        printf("Out of memory\n");
        exit(1);
    }
    f->fieldsize = __FIELDSIZE__ * wordsize;
    f->next = 0;
}

void freeByteField( __bytefield__ *f ) {
    free(f->field);
}

void* allocBlock( __bytefield__ *f, __WORD__ size ) {
    void *b;
    __WORD__ misalign;

    if ((f->next + size) > f->fieldsize) {
        /* __gc(f, &newf); */
        if ((f->next + size) > f->fieldsize) {
            return NULL;
        } else {
            b = (void*)(f->field + f->next);
            
            f->next = f->next + size;
            misalign = (f->next % __WORDSIZE__);
            if (misalign != 0)
                f->next = f->next + (__WORDSIZE__ - misalign);
            
            return b;
        }
    } else {
        b = (void*)(f->field + f->next);
        
        f->next = f->next + size;
        misalign = (f->next % __WORDSIZE__);
        if (misalign != 0)
            f->next = f->next + (__WORDSIZE__ - misalign);

        return b;
    }
}

__VAR__ allocVar( ) {
    __VAR__ res;
    
    if (varnext >= __VARNUM__) {
        printf("Max variable number reached.");
        exit(1);
    } else {
        res = varnext;
        varnext += 1;
    }
    return res;
}

void setVar( __VAR__ var, __BWORD__ val ) {
    if ((int)var < 0 || (int)var >= varnext) {
        printf("Variable index %d does not exist.", (int)var);
        exit(1);
    } else {
        varroots[(int)var] = val;
    }
}

__BWORD__ getVar( __VAR__ var ) {
    if (var < 0 || var >= varnext) {
        printf("Variable index %d does not exist.", (int)var);
        exit(1);
    } else {
        return varroots[(int)var];
    }
}
