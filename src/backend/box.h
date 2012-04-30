#ifndef BOX_H
#define BOX_H
#include "sins_types.h"
#include "sins_const.h"

/*            */
/* Boxing fns */
/*            */
__BWORD__ __box( _S_, __WORD__ v, __WORD__ type );

__BWORD__ __boxint( _S_, __WORD__ v );

__BWORD__ __boxptd( _S_, __WORD__ v );

__BWORD__ __boxpair( _S_, __WORD__ v );

__BWORD__ __boxlambda( _S_, __WORD__ v );

__WORD__ __boxtype( _S_, __BWORD__ v );

__WORD__ __boxsubtype( _S_, __BWORD__ v );

/*              */
/* Unboxing fns */
/*              */
__WORD__ __unboxint( _S_, __BWORD__ v );

__WORD__ __unboxptd( _S_, __BWORD__ v );

__WORD__ __unboxpair( _S_, __BWORD__ v );

__WORD__ __unboxlambda( _S_, __BWORD__ v );

__WORD__ __unbox( _S_, __BWORD__ v );

__BWORD__ __unboxchar( _S_, __BWORD__ ch );

/*                        */
/* Boxed types predicates */
/*                        */
int __boxint_p( _S_, __BWORD__ v );

int __boxptd_p( _S_, __BWORD__ v );

int __boxpair_p( _S_, __BWORD__ v );

int __boxlambda_p( _S_, __BWORD__ v );

int __boxvector_p( _S_, __BWORD__ v );

int __boxstring_p( _S_, __BWORD__ v );

int __boxchar_p( _S_, __BWORD__ v );

/*                */
/* Boxed obj size */
/*                */
__WORD__ __boxptdsize( _S_, __BWORD__ v );

__WORD__ __boxsize( _S_, __BWORD__ v );

#endif
