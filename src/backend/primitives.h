#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "sins_types.h"
#include "sins_const.h"
#include "primitives_utils.h"
#include "box.h"

typedef struct {
    __WORD__ size;
} __vector__;

typedef struct {
    __BWORD__ car;
    __BWORD__ cdr;
} __pair__;

#define __PAIRSIZE__ sizeof(__pair__)

typedef struct {
    void *field;
    __WORD__ fieldsize;
    __WORD__ next;
} __bytefield__;

void allocByteField( __bytefield__ *f, size_t num, size_t size );

void* allocBlock( __bytefield__ *f, __WORD__ size );

__BWORD__ __getcar( __pair__ *p );

__BWORD__ __getcdr( __pair__ *p );

void __setcar( __pair__ *p, __BWORD__ newcar );

void __setcdr( __pair__ *p, __BWORD__ newcdr );

__pair__* __cons( __bytefield__ *f, __BWORD__ car, __BWORD__ cdr );

__vector__* __vector( __bytefield__ *f, __WORD__ size);

__BWORD__ __vector_ref( __vector__* v, __WORD__ ref);

void __vector_set( __vector__* v, __WORD__ ref, __BWORD__ val);

__BWORD__ __box( __WORD__ v, __WORD__ type );

__WORD__ __unbox( __BWORD__ v );

#endif

