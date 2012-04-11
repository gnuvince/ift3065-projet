#include "box.h"
#include "sins_types.h"
#include "sins_const.h"
#include "primitives.h"
#include "primitives_utils.h"

/*            */
/* Boxing fns */
/*            */
__BWORD__ __box( __WORD__ v, __WORD__ type ) {
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
        break;
    }
}

__BWORD__ __boxint( __WORD__ v ) {
    return __box(v, __INT_TYPE__);
}

__BWORD__ __boxptd( __WORD__ v ) {
    return __box(v, __PTD_TYPE__);
}

__BWORD__ __boxpair( __WORD__ v ) {
    return __box(v, __PAIR_TYPE__);
}

__BWORD__ __boxlambda( __WORD__ v ) {
    return __box(v, __LAMBDA_TYPE__);
}

/*              */
/* Unboxing fns */
/*              */
__WORD__ __unboxint( __BWORD__ v ) {
    return ((v - __INT_TYPE__) >> __INT_SHFT__);
}

__WORD__ __unboxptd( __BWORD__ v ) {
    return ((v - __PTD_TYPE__) >> __PTD_SHFT__);
}

char  __unboxpair( __BWORD__ v ) {
    return ((v - __PAIR_TYPE__) >> __PAIR_SHFT__);
}

__WORD__ __unboxlambda( __BWORD__ v ) {
    return ((v - __LAMBDA_TYPE__) >> __LAMBDA_SHFT__);
}

/*                        */
/* Boxed types predicates */
/*                        */
int __boxint_p( __BWORD__ v ) {
    return ((v & __BOX_MASK__) == __INT_TYPE__);
}

int __boxptd_p( __BWORD__ v ) {
    return ((v & __BOX_MASK__) == __PTD_TYPE__);
}

int __boxpair_p( __BWORD__ v ) {
    return ((v & __BOX_MASK__) == __PAIR_TYPE__);
}

int __boxlambda_p( __BWORD__ v ) {
    return ((v != __FALSE__) && (v & __BOX_MASK__) == __LAMBDA_TYPE__);
}

int __boxvector_p( __BWORD__ v ) {
    return (__boxptd_p(v) && (*((__WORD__*)__unboxptd(v)) & __SUB_MASK__ == __VEC_TYPE__));
}

int __boxstring_p( __BWORD__ v ) {
    return 0; /* not yet */
    /* return (__boxptd_p(v) && (*((void*)__unboxptd(v)) & __SUB_MASK__ == __STR_TYPE__)); */
}
