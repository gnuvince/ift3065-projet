#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include <stddef.h>
#include "sins_types.h"
#include "sins_const.h"
#include "primitives_utils.h"
#include "box.h"
#include "bytefield.h"

void __initStack( _S_ );

void __initHeap ( _S_ );

__BWORD__ __getCar ( _S_, __BWORD__ p );

__BWORD__ __getCdr ( _S_, __BWORD__ p );

void __setCar ( _S_, __BWORD__ p, __BWORD__ newcar );

void __setCdr ( _S_, __BWORD__ p, __BWORD__ newcdr );

__BWORD__ __cons ( _S_, __BWORD__ car, __BWORD__ cdr );

__BWORD__ __createLambda ( _S_, __BWORD__ size );

__BWORD__ __stringToSymbol ( _S_, __BWORD__ s );

__BWORD__ __symbolLength ( _S_, __BWORD__ sym );

__BWORD__ __symbolToString ( _S_, __BWORD__ sym );

void  __lambdaSet ( _S_, __BWORD__ p, __BWORD__ ref, __BWORD__ val );

__BWORD__ __lambdaRef ( _S_, __BWORD__ v, __BWORD__ ref );

void __lambdaSetCode ( _S_, __BWORD__ p, __WORD__ code );

__WORD__ __lambdaGetCode ( _S_, __BWORD__ p );

__BWORD__ __createVector ( _S_, __BWORD__ size );

__BWORD__ __vectorRef ( _S_, __BWORD__ v, __BWORD__ ref );

void __vectorSet ( _S_, __BWORD__ v, __BWORD__ ref, __BWORD__ val );

__BWORD__ __vectorLength ( _S_, __BWORD__ v );

__BWORD__ __vectorEqual ( _S_, __BWORD__ v1, __BWORD__ v2);

__BWORD__ __vector_p ( _S_, __BWORD__ v );

__BWORD__ __add ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __inc ( _S_, __BWORD__ a );

__BWORD__ __sub ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __mul ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __quotient ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __remainder ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__  __number_p ( _S_, __BWORD__ n );

__BWORD__  __null_p ( _S_, __BWORD__ p );

__BWORD__ __lambda_p ( _S_, __BWORD__ p );

__BWORD__ __pair_p ( _S_, __BWORD__ p );

__BWORD__ __list_p ( _S_, __BWORD__ p );

__BWORD__ __lt ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __gt ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __ge ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __le ( _S_, __BWORD__ a, __BWORD__ b );

__BWORD__ __equalPtd ( _S_, __BWORD__ b1, __BWORD__ b2 );

__BWORD__ __equal ( _S_, __BWORD__ b1, __BWORD__ b2 );

__BWORD__ __eq ( _S_, __BWORD__ b1, __BWORD__ b2 );

__BWORD__ __createString ( char *s );

__BWORD__ __string_p ( _S_, __BWORD__ s );

__BWORD__ __stringLength ( _S_, __BWORD__ s );

__BWORD__ __stringRef ( _S_, __BWORD__ s, __BWORD__ ref);

__BWORD__ __stringEqual ( _S_, __BWORD__ s1, __BWORD__ s2 );

__BWORD__ __stringToList ( _S_, __BWORD__ s );

void __stringSet ( _S_, __BWORD__ s, __BWORD__ ref, __BWORD__ ch );

void __display ( _S_, __BWORD__ s );

void __newline ( _S_ );

__BWORD__ __charToInteger ( _S_, __BWORD__ ch );

__BWORD__ __integerToChar ( _S_, __BWORD__ ch );

__BWORD__ __char_p ( _S_, __BWORD__ c );

void __writeChar ( _S_, __BWORD__ c );

void __gc ( _S_ );

void __dumpHeap ( _S_ );

#endif
