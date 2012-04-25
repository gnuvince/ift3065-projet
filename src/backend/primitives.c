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

__BWORD__ __getCar( __BWORD__ p ) {
    if (__pair_p(p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    return ((__pair__*)__unboxpair(p))->car;
}

__BWORD__ __getCdr( __BWORD__ p ) {
    if (__pair_p(p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    return ((__pair__*)__unboxpair(p))->cdr;
}

void __setCar( __BWORD__ p, __BWORD__ newcar ) {
    if (__pair_p(p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    ((__pair__*)__unboxpair(p))->car = newcar;
}

void __setCdr( __BWORD__ p, __BWORD__ newcdr ) {
    if (__pair_p(p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    ((__pair__*)__unboxpair(p))->cdr = newcdr;
}

__BWORD__  __cons( __BWORD__ car, __BWORD__ cdr ) {
    __pair__ *newpair = NULL;
    __BWORD__ bpair;
    
    newpair = (__pair__*)allocBlock(getHeap(), __PAIRSIZE__);
    newpair->hdr = __PAIR_TYPE__;
    
    if (newpair == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        bpair = __box((__WORD__)newpair, __PAIR_TYPE__);
        __setCar(bpair, car);
        __setCdr(bpair, cdr);
        return bpair;
    }
}

__BWORD__ __vector( __BWORD__ size) {
    __vector__ *newvector = NULL;

    __WORD__ usize = __unboxint(size);
    newvector = (__vector__*)allocBlock(getHeap(), sizeof(__vector__) + (__WORDSIZE__ * usize));

    if (newvector == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newvector->hdr = (usize << __VEC_LEN_SHFT__) + __VEC_TYPE__;
        return __box((__WORD__)newvector, __PTD_TYPE__);
    }
}

__BWORD__ __vectorRef( __BWORD__ v, __BWORD__ ref) {
    if (__vector_p(v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(ref);
    if ((uref < 0) || (uref >= __unboxint(__vectorLength(v)))) {
        printf("Error: Invalid vector index\n");
        exit(__FAIL__);        
    }
    
    return *((__BWORD__*)__unboxptd(v) + uref + 2);
}

void __vectorSet( __BWORD__ v, __BWORD__ ref, __BWORD__ val) {
    if (__vector_p(v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(ref);
    if ((uref < 0) || (uref >= __unboxint(__vectorLength(v)))) {
        printf("Error: Invalid vector index\n");
        exit(__FAIL__);        
    }

    *((__BWORD__*)__unboxptd(v) + uref + 2) = val;
}

__BWORD__ __vectorLength( __BWORD__ v ) {
    if (__vector_p(v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    return __box(((__vector__*)__unboxptd(v))->hdr >> __VEC_LEN_SHFT__, __INT_TYPE__);
}

__BWORD__ __vectorEqual( __BWORD__ v1, __BWORD__ v2) {
    if (__vector_p(v1) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    if (__vector_p(v2) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }
    
    __WORD__ v1Len = __vectorLength(v1);
    
    if (v1Len != __vectorLength(v2))
        return __FALSE__;

    for (__WORD__ i = 0; i < v1Len; ++i) {
        if (__vectorRef(v1, i) != __vectorRef(v2, i))
            return __FALSE__;
    }
    
    return __TRUE__;
}

__BWORD__ __vector_p( __BWORD__ v ) {
    if (__boxvector_p(v))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __add( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return (a + b);
}

__BWORD__ __sub( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return (a - b);
}

__BWORD__ __mul( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __box(__unboxint(a) * __unboxint(b), __INT_TYPE__);
}

__BWORD__ __quotient( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __box(__unboxint(a) / __unboxint(b), __INT_TYPE__);
}

__BWORD__ __remainder( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __box(__unboxint(a) % __unboxint(b), __INT_TYPE__);
}

__BWORD__ __number_p( __BWORD__ n ) {
    if (__boxint_p(n))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__  __null_p( __BWORD__ p ) {
    if (p == __NULL__)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __pair_p( __BWORD__ p ) {
    if(__boxpair_p(p) && (__null_p(p) == __FALSE__))
        return __TRUE__;
    else
        return __FALSE__;
}


__BWORD__ __list_p( __BWORD__ p ) {
    if (__null_p(p) == __TRUE__)
        return __TRUE__;
    
    if(__boxpair_p(p))
        return __list_p(__getCdr(p));
    else
        return __FALSE__;
}

__BWORD__ __lt( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a < b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __gt( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a > b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __ge( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a >= b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __le( __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(a) == __FALSE__) || (__number_p(b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a <= b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __equalPtd( __BWORD__ b1, __BWORD__ b2 ) {
    switch(__boxsubtype(b1)) {
    case __VEC_TYPE__:
        return __vectorEqual(b1, b2);
    case __STR_TYPE__:
        return __stringEqual(b1, b2);
    default:
        return __FALSE__;
    }
}

__BWORD__ __equal( __BWORD__ a, __BWORD__ b ) {
    if (__boxtype(a) != __boxtype(b))
        return __FALSE__;

    if (__boxtype(a) == __PTD_TYPE__)
        return __equalPtd(a, b);
    else
        return __eq(a, b);    
}

__BWORD__ __eq( __BWORD__ a, __BWORD__ b ) {
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
        return __box((__WORD__)newstring, __PTD_TYPE__);
    }
}

__BWORD__ __string_p( __BWORD__ s ) {
    if ((__boxtype(s) == __PTD_TYPE__) && (__boxsubtype(s) == __STR_TYPE__))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __stringLength( __BWORD__ s ) {
    if (__string_p(s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    return __box((((__string__*)(__unboxptd(s)))->hdr) >> __STR_LEN_SHFT__, __INT_TYPE__);
}

__BWORD__ __stringRef( __BWORD__ s, __BWORD__ ref) {
    if (__string_p(s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(ref);
    if ((uref < 0) || (uref >= __stringLength(s))) {
        printf("Error: Invalid string index\n");
        exit(__FAIL__);        
    }
    
    return *((char*)__unboxptd(s) + uref + sizeof(__char__));
}

__BWORD__ __stringEqual( __BWORD__ s1, __BWORD__ s2 ) {
    if (__string_p(s1) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    if (__string_p(s2) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }
    
    __WORD__ s1Len = __unboxint(__stringLength(s1));
    
    if (s1Len != __unboxint(__stringLength(s2)))
        return __FALSE__;

    for (__WORD__ i = 0; i < s1Len; ++i) {
        if (__stringRef(s1, __boxint(i)) != __stringRef(s2, __boxint(i)))
            return __FALSE__;
    }
    
    return __TRUE__;
    
}

void __display( __BWORD__ s ) {
    if (__string_p(s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    printf((char*)__unboxptd(s) + sizeof(__string__));
}

void __newline( ) {
    __writeChar(__char(__CH_newline__));
}

__BWORD__ __char( char ch ) {
    __char__ *newchar = NULL;

    newchar = (__char__*)allocBlock(getHeap(), sizeof(__char__));

    if (newchar == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newchar->hdr = ((__WORD__)ch << __CHAR_VAL_SHFT__) + __CHAR_TYPE__;
        return __box((__WORD__)newchar, __PTD_TYPE__);
    }
}

__BWORD__ __char_p( __BWORD__ ch ) {
    if ((__boxtype(ch) == __PTD_TYPE__) && (__boxsubtype(ch) == __CHAR_TYPE__))
        return __TRUE__;
    else
        return __FALSE__;
}
 
void __writeChar( __BWORD__ ch ) {
    if (__char_p(ch) == __FALSE__) {
        printf("CHAR expected\n");
        exit(__FAIL__);
    }

    int c = (int)__unboxchar(ch);
    if (putchar(c) != c) {
        printf("Error writing char\n");
        exit(__FAIL__);
    }
}
