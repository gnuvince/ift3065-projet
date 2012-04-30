#include <stdio.h>
#include <stdlib.h>
#include "box.h"
#include "sins_types.h"
#include "sins_const.h"
#include "primitives.h"
#include "primitives_utils.h"
#include "bytefield.h"

/*            */
/* Boxing fns */
/*            */
__BWORD__ __box( _S_, __WORD__ v, __WORD__ type ) {
    switch (type) {
    case __INT_TYPE__:
        return (v << __INT_SHFT__) + __INT_TYPE__;

    case __PTD_TYPE__:
        return (v << __PTD_SHFT__) + __PTD_TYPE__;

    case __PAIR_TYPE__:
        return (v << __PAIR_SHFT__) + __PAIR_TYPE__;

    case __LAMBDA_TYPE__:
        return (v << __LAMBDA_SHFT__) + __LAMBDA_TYPE__;
        
    default:
        printf("Unrecognized box type\n");
        exit(__FAIL__);
    }
}

__BWORD__ __boxint( _S_, __WORD__ v ) {
    return __box(_A2_, v, __INT_TYPE__);
}

__BWORD__ __boxptd( _S_, __WORD__ v ) {
    return __box(_A2_, v, __PTD_TYPE__);
}

__BWORD__ __boxpair( _S_, __WORD__ v ) {
    return __box(_A2_, v, __PAIR_TYPE__);
}

__BWORD__ __boxlambda( _S_, __WORD__ v ) {
    return __box(_A2_, v, __LAMBDA_TYPE__);
}

__WORD__ __boxtype( _S_, __BWORD__ v ) {
    return (__WORD__)(v & __BOX_MASK__);
}

__WORD__ __boxsubtype( _S_, __BWORD__ v ) {
    return (((__ptd_hdr__*)__unboxptd(_A1_, v))->hdr & __SUB_MASK__); 
}


/*              */
/* Unboxing fns */
/*              */
__WORD__ __unboxint( _S_, __BWORD__ v ) {
    return ((v - __INT_TYPE__) >> __INT_SHFT__);
}

__WORD__ __unboxptd( _S_, __BWORD__ v ) {
    return ((v - __PTD_TYPE__) >> __PTD_SHFT__);
}

__WORD__  __unboxpair( _S_, __BWORD__ v ) {
    return ((v - __PAIR_TYPE__) >> __PAIR_SHFT__);
}

__WORD__ __unboxlambda( _S_, __BWORD__ v ) {
    return ((v - __LAMBDA_TYPE__) >> __LAMBDA_SHFT__);
}

__WORD__ __unbox( _S_, __BWORD__ v ) {
    switch (__boxtype(_A1_, v)) {
    case __INT_TYPE__:
        return __unboxint(_A1_, v);
    case __PTD_TYPE__:
        return __unboxptd(_A1_, v);
    case __PAIR_TYPE__:
        return __unboxpair(_A1_, v);
    case __LAMBDA_TYPE__:
    default:
        printf("Unrecognized or unsupported box type\n");
        exit(__FAIL__);        
    }
}

__BWORD__ __unboxchar( _S_, __BWORD__ ch ) {
    return __boxint(_A1_, ((((__char__*)__unboxptd(_A1_, ch))->hdr >> __CHAR_VAL_SHFT__) & __CHAR_MASK__));
}

/*                        */
/* Boxed types predicates */
/*                        */
int __boxint_p( _S_, __BWORD__ v ) {
    return (__boxtype(_A1_, v) == __INT_TYPE__);
}

int __boxptd_p( _S_, __BWORD__ v ) {
    return (__boxtype(_A1_, v) == __PTD_TYPE__);
}

int __boxpair_p( _S_, __BWORD__ v ) {
    return (__boxtype(_A1_, v) == __PAIR_TYPE__);
}

int __boxlambda_p( _S_, __BWORD__ v ) {
    return (__boxtype(_A1_, v) == __LAMBDA_TYPE__);    
}

int __boxvector_p( _S_, __BWORD__ v ) {
    return (__boxptd_p(_A1_, v) && __boxsubtype(_A1_, v) == __VEC_TYPE__);
}

int __boxstring_p( _S_, __BWORD__ v ) {
    return (__boxptd_p(_A1_, v) && __boxsubtype(_A1_, v) == __STR_TYPE__);
}

int __boxchar_p( _S_, __BWORD__ v ) {
    return (__boxptd_p(_A1_, v) && __boxsubtype(_A1_, v) == __CHAR_TYPE__);
}

/*                */
/* Boxed obj size */
/*                */
/* __WORD__ __boxptdsize( _S_, __BWORD__ v ) { return 1; } */
__WORD__ __boxptdsize( _S_, __BWORD__ v ) {
    switch (__boxsubtype(_A1_, v)) {
    case __VEC_TYPE__:
        return (sizeof(__ptd_hdr__) + (__unboxint(_A1_, __vectorLength(_A1_, v)) * __BWORDSIZE__));
    
    case __STR_TYPE__:
        return (sizeof(__ptd_hdr__) + (__unboxint(_A1_, __stringLength(_A1_, v)) + 1));
    
    case __CHAR_TYPE__:
        return (sizeof(__ptd_hdr__));
    
    default:
        printf("Unrecognized or unsupported box type\n");
        exit(__FAIL__);
    }
}

__WORD__ __boxsize( _S_, __BWORD__ v ) {
    if (v == __NULL__)
        return __BWORDSIZE__;
    
    switch (__boxtype(_A1_, v)) {
    case __INT_TYPE__:
        return (__BWORDSIZE__);

    case __PTD_TYPE__:
        return (__boxptdsize(_A1_, v));

    case __PAIR_TYPE__:
        return (__PAIRSIZE__);

    case __LAMBDA_TYPE__:
    default:
        printf("Unrecognized or unsupported box type\n");
        exit(__FAIL__);
    }
}
