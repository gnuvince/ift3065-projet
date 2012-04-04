#include "primitives.h"

__BWORD__ __box( __WORD__ v, __WORD__ type ) {
    switch (type) {
    case __PAIR_TYPE__:
        return (v << __PAIR_SHFT__) + __PAIR_TYPE__;
        
    case __INT_TYPE__:
        return (v << __INT_SHFT__) + __INT_TYPE__;

    default:
        break;
    }
}

__WORD__ __unbox( __BWORD__ v ) {
    switch (v & __BOX_MASK__) {
    case __PAIR_TYPE__:
        return (v >> __PAIR_SHFT__);

    case __INT_TYPE__:
        return (v >> __INT_SHFT__);

    default:
        break;
    }
}
