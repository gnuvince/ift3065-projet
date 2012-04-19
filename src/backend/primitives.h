#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include <stddef.h>
#include "sins_types.h"
#include "sins_const.h"
#include "primitives_utils.h"
#include "box.h"

__bytefield__ f;
__bytefield__ oldf;             /* For stop-and-copy GC */

void allocByteField( size_t num, size_t size );

void freeByteField();

void* allocBlock( __WORD__ size );

__BWORD__ __getCar( __BWORD__ p );

__BWORD__ __getCdr( __BWORD__ p );

void __setCar( __BWORD__ p, __BWORD__ newcar );

void __setCdr( __BWORD__ p, __BWORD__ newcdr );

__BWORD__ __cons( __BWORD__ car, __BWORD__ cdr );

__BWORD__ __vector( __WORD__ size);

__BWORD__ __vectorRef( __BWORD__ v, __WORD__ ref);

void __vectorSet( __BWORD__ v, __WORD__ ref, __BWORD__ val);

__BWORD__ __vectorLength( __BWORD__ v );

__BWORD__ __vectorEqual( __BWORD__ v1, __BWORD__ v2);

__BWORD__ __vector_p( __BWORD__ v );

__BWORD__ __add( __BWORD__ a, __BWORD__ b );

__BWORD__ __sub( __BWORD__ a, __BWORD__ b );

__BWORD__ __mul( __BWORD__ a, __BWORD__ b );

__BWORD__ __quotient( __BWORD__ a, __BWORD__ b );

__BWORD__ __remainder( __BWORD__ a, __BWORD__ b );

__BWORD__  __number_p( __BWORD__ n );

__BWORD__  __null_p( __BWORD__ p );

__BWORD__ __pair_p( __BWORD__ p );

__BWORD__ __list_p( __BWORD__ p );

__BWORD__ __lt( __BWORD__ a, __BWORD__ b );

__BWORD__ __gt( __BWORD__ a, __BWORD__ b );

__BWORD__ __ge( __BWORD__ a, __BWORD__ b );

__BWORD__ __le( __BWORD__ a, __BWORD__ b );

__BWORD__ __equalPtd( __BWORD__ b1, __BWORD__ b2 );

__BWORD__ __equal( __BWORD__ b1, __BWORD__ b2 );

__BWORD__ __eq( __BWORD__ b1, __BWORD__ b2 );

__BWORD__ __string( char *s );

__BWORD__ __string_p( __BWORD__ s );

__BWORD__ __stringLength( __BWORD__ s );

__BWORD__ __stringRef( __BWORD__ s, __WORD__ ref);

__BWORD__ __stringEqual( __BWORD__ s1, __BWORD__ s2 );

void __display( __BWORD__ s );

void __newline( );

__BWORD__ __char( char ch );

__BWORD__ __char_p( __BWORD__ c );

void __writeChar( __BWORD__ c );

#endif
