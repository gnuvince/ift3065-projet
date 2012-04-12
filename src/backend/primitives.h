#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include <stddef.h>
#include "sins_types.h"
#include "sins_const.h"
#include "primitives_utils.h"
#include "box.h"

__bytefield__ f;

void allocByteField( size_t num, size_t size );

void freeByteField();

void* allocBlock( __WORD__ size );

__BWORD__ __getCar( __pair__ *p );

__BWORD__ __getCdr( __pair__ *p );

void __setCar( __pair__ *p, __BWORD__ newcar );

void __setCdr( __pair__ *p, __BWORD__ newcdr );

__pair__* __cons( __BWORD__ car, __BWORD__ cdr );

__vector__* __vector( __WORD__ size);

__BWORD__ __vectorRef( __vector__* v, __WORD__ ref);

void __vectorSet( __vector__* v, __WORD__ ref, __BWORD__ val);

__BWORD__ __add( __BWORD__ a, __BWORD__ b );

__BWORD__ __sub( __BWORD__ a, __BWORD__ b );

__BWORD__ __mul( __BWORD__ a, __BWORD__ b );

__BWORD__ __quotient( __BWORD__ a, __BWORD__ b );

__BWORD__ __remainder( __BWORD__ a, __BWORD__ b );

int __number_p( __BWORD__ n );

int __pair_p( __BWORD__ p );

int __lt( __BWORD__ a, __BWORD__ b );

int __gt( __BWORD__ a, __BWORD__ b );

int __ge( __BWORD__ a, __BWORD__ b );

int __le( __BWORD__ a, __BWORD__ b );

int __equal( __BWORD__ a, __BWORD__ b );

int __eq( __BWORD__ a, __BWORD__ b );

#endif
