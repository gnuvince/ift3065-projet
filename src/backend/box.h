#include "primitives.h"

/****************************************************/
/*                                                  */
/* Boxed types Pattern                              */
/* ------------------------------------------------ */
/* int         all box bits set to 0                */
/* pair        last 2 bits 11                       */
/* pointed     last 2 bits 01                       */
/* ascii       last 2 bits 10                       */
/*                                                  */
/****************************************************/

typedef word_t __BWORD__;
#define __BWORDSIZE__ sizeof(__BWORD__)

#define __BOX_SIZE__  2
#define __BOX_MASK__  3

#define __INT_TYPE__  0
#define __INT_SHFT__  2
#define __PTD_TYPE__  1
#define __PTD_SHFT__  0
#define __PTD_VEC__   255
#define __PTD_STR__   0
#define __CHAR_TYPE__ 2
#define __CHAR_SHFT__ 0
#define __PAIR_TYPE__ 3
#define __PAIR_SHFT__ 0

__BWORD__ __box( __WORD__ v, __WORD__ type ) {
    switch (type) {
    case __INT_TYPE__:
        return (v << __INT_SHFT__) + __INT_TYPE__;

    case __PTD_TYPE__:
        return (v << __PTD_SHFT__) + __PTD_TYPE__;

    case __CHAR_TYPE__:
        return (v << __CHAR_SHFT__) + __CHAR_TYPE__;

    case __PAIR_TYPE__:
        return (v << __PAIR_SHFT__) + __PAIR_TYPE__;
        
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

__BWORD__ __boxchar( char v ) {
    return __box((__WORD__)v, __CHAR_TYPE__);
}

__BWORD__ __boxpair( __WORD__ v ) {
    return __box(v, __PAIR_TYPE__);
}

__WORD__ __unboxint( __BWORD__ v ) {
    return ((v - __INT_TYPE__) >> __INT_SHFT__);
}

__WORD__ __unboxptd( __BWORD__ v ) {
    return ((v - __PTD_TYPE__) >> __PTD_SHFT__);
}

char  __unboxchar( __BWORD__ v ) {
    return ((v - __INT_TYPE__) >> __INT_SHFT__);
}

__WORD__ __unboxpair( __BWORD__ v ) {
    return ((v - __INT_TYPE__) >> __INT_SHFT__);
}

