#ifndef BYTEFIELD_H
#define BYTEFIELD_H

#include <stdlib.h>
#include <stdio.h>
#include "sins_types.h"

#define __FIELDSIZE__ 1024
#define __VARNUM__ (__FIELDSIZE__ >> 1)

typedef int __VAR__;

typedef struct {
    void     *field;
    __WORD__ fieldsize;
    __WORD__ next;
} __bytefield__;

__bytefield__* getHeap();

__bytefield__* getNewHeap();

__VAR__ getVarNext();

void swapHeap();

void allocByteField( __bytefield__ *f, size_t wordsize );

void freeByteField( __bytefield__ *f );

void* allocBlock( __bytefield__ *f, __WORD__ size );

__VAR__ allocVar( );

void setVar( __VAR__ var, __BWORD__ val );

__BWORD__ getVar( __VAR__ var );

#endif
