#ifndef BOX_H
#define BOX_H
#include "sins_types.h"

/*            */
/* Boxing fns */
/*            */
__BWORD__ __box( __WORD__ v, __WORD__ type );

__BWORD__ __boxint( __WORD__ v );

__BWORD__ __boxptd( __WORD__ v );

__BWORD__ __boxpair( __WORD__ v );

__BWORD__ __boxlambda( __WORD__ v );

__WORD__ __boxtype( __BWORD__ v );

__WORD__ __boxsubtype( __BWORD__ v );

/*              */
/* Unboxing fns */
/*              */
__WORD__ __unboxint( __BWORD__ v );

__WORD__ __unboxptd( __BWORD__ v );

__WORD__ __unboxpair( __BWORD__ v );

__WORD__ __unboxlambda( __BWORD__ v );

__WORD__ __unbox( __BWORD__ v );

char __unboxchar( __BWORD__ ch );

/*                        */
/* Boxed types predicates */
/*                        */
int __boxint_p( __BWORD__ v );

int __boxptd_p( __BWORD__ v );

int __boxpair_p( __BWORD__ v );

int __boxlambda_p( __BWORD__ v );

int __boxvector_p( __BWORD__ v );

int __boxstring_p( __BWORD__ v );

int __boxchar_p( __BWORD__ v );

/*                */
/* Boxed obj size */
/*                */
__WORD__ __boxptdsize( __BWORD__ v );

__WORD__ __boxsize( __BWORD__ v );

#endif
