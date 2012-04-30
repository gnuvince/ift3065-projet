#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "primitives.h"
#include "sins_types.h"
#include "sins_const.h"
#include "primitives_utils.h"
#include "box.h"
#include "bytefield.h"

__BWORD__ __getCar( _S_, __BWORD__ p ) {
    if (__pair_p(_A1_, p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    return ((__pair__*)__unboxpair(_A1_, p))->car;
}

__BWORD__ __getCdr( _S_, __BWORD__ p ) {
    if (__pair_p(_A1_, p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    return ((__pair__*)__unboxpair(_A1_, p))->cdr;
}

void __setCar( _S_, __BWORD__ p, __BWORD__ newcar ) {
    if (__pair_p(_A1_, p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    ((__pair__*)__unboxpair(_A1_, p))->car = newcar;
}

void __setCdr( _S_, __BWORD__ p, __BWORD__ newcdr ) {
    if (__pair_p(_A1_, p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    ((__pair__*)__unboxpair(_A1_, p))->cdr = newcdr;
}

__BWORD__  __cons( _S_, __BWORD__ car, __BWORD__ cdr ) {
    __pair__ *newpair = NULL;
    __BWORD__ bpair;

    newpair = (__pair__*)allocBlock(getHeap(), __PAIRSIZE__);
    newpair->hdr = __PAIR_TYPE__;

    if (newpair == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        bpair = __boxpair(_A1_, (__WORD__)newpair);
        __setCar(_A2_, bpair, car);
        __setCdr(_A2_, bpair, cdr);
        return bpair;
    }
}

__BWORD__ __vector( _S_, __BWORD__ size ) {
    __vector__ *newvector = NULL;

    __WORD__ usize = __unboxint(_A1_, size);
    newvector = (__vector__*)allocBlock(getHeap(), sizeof(__vector__) + (__WORDSIZE__ * usize));

    if (newvector == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newvector->hdr = (usize << __VEC_LEN_SHFT__) + __VEC_TYPE__;
        return __boxptd(_A1_, (__WORD__)newvector);
    }
}

__BWORD__ __vectorRef( _S_, __BWORD__ v, __BWORD__ ref ) {
    if (__vector_p(_A1_, v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(_A1_, ref);
    if ((uref < 0) || (uref >= __unboxint(_A1_, __vectorLength(_A1_, v)))) {
        printf("Error: Invalid vector index\n");
        exit(__FAIL__);
    }

    return *((__BWORD__*)__unboxptd(_A1_, v) + uref + 2);
}

void __vectorSet( _S_, __BWORD__ v, __BWORD__ ref, __BWORD__ val ) {
    if (__vector_p(_A1_, v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(_A1_, ref);
    if ((uref < 0) || (uref >= __unboxint(_A1_, __vectorLength(_A1_, v)))) {
        printf("Error: Invalid vector index\n");
        exit(__FAIL__);
    }

    *((__BWORD__*)__unboxptd(_A1_, v) + uref + 2) = val;
}

__BWORD__ __vectorLength( _S_, __BWORD__ v ) {
    if (__vector_p(_A1_, v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A1_, ((__vector__*)__unboxptd(_A1_, v))->hdr >> __VEC_LEN_SHFT__);
}

__BWORD__ __vectorEqual( _S_, __BWORD__ v1, __BWORD__ v2 ) {
    if (__vector_p(_A1_, v1) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    if (__vector_p(_A1_, v2) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ v1Len = __vectorLength(_A1_, v1);

    if (v1Len != __vectorLength(_A1_, v2))
        return __FALSE__;

    for (__WORD__ i = 0; i < v1Len; ++i) {
        if (__vectorRef(_A2_, v1, i) != __vectorRef(_A2_, v2, i))
            return __FALSE__;
    }

    return __TRUE__;
}

__BWORD__ __vector_p( _S_, __BWORD__ v ) {
    if (__boxvector_p(_A1_, v))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __add( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return (a + b);
}

__BWORD__ __sub( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A1_, __unboxint(_A1_, a) - __unboxint(_A1_, b));    
}

__BWORD__ __mul( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A1_, __unboxint(_A1_, a) * __unboxint(_A1_, b));
}

__BWORD__ __quotient( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A1_, __unboxint(_A1_, a) / __unboxint(_A1_, b));
}

__BWORD__ __remainder( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A1_, __unboxint(_A1_, a) % __unboxint(_A1_, b));
}

__BWORD__ __number_p( _S_, __BWORD__ n ) {
    if (__boxint_p(_A1_, n))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__  __null_p( _S_, __BWORD__ p ) {
    if (p == __NULL__)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __pair_p( _S_, __BWORD__ p ) {
    if(__boxpair_p(_A1_, p) && (__null_p(_A1_, p) == __FALSE__))
        return __TRUE__;
    else
        return __FALSE__;
}


__BWORD__ __list_p( _S_, __BWORD__ p ) {
    if (__null_p(_A1_, p) == __TRUE__)
        return __TRUE__;

    if(__boxpair_p(_A1_, p))
        return __list_p(_A1_, __getCdr(_A1_, p));
    else
        return __FALSE__;
}

__BWORD__ __lt( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a < b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __gt( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a > b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __ge( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a >= b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __le( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A1_, a) == __FALSE__) || (__number_p(_A1_, b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a <= b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __equalPtd( _S_, __BWORD__ b1, __BWORD__ b2 ) {
    switch(__boxsubtype(_A1_, b1)) {
    case __VEC_TYPE__:
        return __vectorEqual(_A2_, b1, b2);
    case __STR_TYPE__:
        return __stringEqual(_A2_, b1, b2);
    default:
        return __FALSE__;
    }
}

__BWORD__ __equal( _S_, __BWORD__ a, __BWORD__ b ) {
    if (__boxtype(_A1_, a) != __boxtype(_A1_, b))
        return __FALSE__;

    if (__boxtype(_A1_, a) == __PTD_TYPE__)
        return __equalPtd(_A2_, a, b);
    else
        return __eq(_A2_, a, b);
}

__BWORD__ __eq( _S_, __BWORD__ a, __BWORD__ b ) {
    if (a == b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __string( char *s ) {
    __string__ *newstring = NULL;
    __WORD__ slen = (__WORD__)strlen(s);

    newstring = (__string__*)allocBlock(getHeap(), sizeof(__string__) + slen + 1);

    if (newstring == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newstring->hdr = (slen << __STR_LEN_SHFT__) + __STR_TYPE__;
        strcpy((char*)newstring + sizeof(__string__), s);
        return __boxptd(_A1_, (__WORD__)newstring);
    }
}

__BWORD__ __string_p( _S_, __BWORD__ s ) {
    if ((__boxtype(_A1_, s) == __PTD_TYPE__) && (__boxsubtype(_A1_, s) == __STR_TYPE__))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __stringLength( _S_, __BWORD__ s ) {
    if (__string_p(_A1_, s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A1_, (((__string__*)(__unboxptd(_A1_, s)))->hdr) >> __STR_LEN_SHFT__);
}

__BWORD__ __stringRef( _S_, __BWORD__ s, __BWORD__ ref) {
    if (__string_p(_A1_, s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(_A1_, ref);
    if ((uref < 0) || (uref >= __stringLength(_A1_, s))) {
        printf("Error: Invalid string index\n");
        exit(__FAIL__);
    }

    return *((char*)__unboxptd(_A1_, s) + uref + sizeof(__char__));
}

__BWORD__ __stringEqual( _S_, __BWORD__ s1, __BWORD__ s2 ) {
    if (__string_p(_A1_, s1) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    if (__string_p(_A1_, s2) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    __WORD__ s1Len = __unboxint(_A1_, __stringLength(_A1_, s1));

    if (s1Len != __unboxint(_A1_, __stringLength(_A1_, s2)))
        return __FALSE__;

    for (__WORD__ i = 0; i < s1Len; ++i) {
        if (__stringRef(_A2_, s1, __boxint(_A1_, i)) != __stringRef(_A2_, s2, __boxint(_A1_, i)))
            return __FALSE__;
    }

    return __TRUE__;

}

void __display( _S_, __BWORD__ s ) {
    if (__string_p(_A1_, s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    printf((char*)__unboxptd(_A1_, s) + sizeof(__string__));
}

void __newline( ) {
    __writeChar(_A1_, __char(_A1_, __CH_newline__));
}

__BWORD__ __charToInteger( _S_, __BWORD__ ch ) {
    return __unboxchar(ch);
}

__BWORD__ __integerToChar( _S_, __BWORD__ i ) {
    __char__ *newchar = NULL;

    newchar = (__char__*)allocBlock(getHeap(), sizeof(__char__));

    if (newchar == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newchar->hdr = ((__BWORD__)i << __CHAR_VAL_SHFT__) + __CHAR_TYPE__;
        return __boxptd(_A1_, (__WORD__)newchar);
    }
}

__BWORD__ __char_p( _S_, __BWORD__ ch ) {
    if ((__boxtype(_A1_, ch) == __PTD_TYPE__) && (__boxsubtype(_A1_, ch) == __CHAR_TYPE__))
        return __TRUE__;
    else
        return __FALSE__;
}

void __writeChar( _S_, __BWORD__ ch ) {
    if (__char_p(_A1_, ch) == __FALSE__) {
        printf("CHAR expected\n");
        exit(__FAIL__);
    }

    int c = (int)__unboxchar(_A1_, ch);
    if (putchar(c) != c) {
        printf("Error writing char\n");
        exit(__FAIL__);
    }
}
