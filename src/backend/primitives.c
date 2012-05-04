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
#include "bytefield_utils.h"
#include "gc.h"
#include "stack.h"

/*                */
/* Pair procedure */
/*                */

__BWORD__ __getCar ( _S_, __BWORD__ p ) {
    if (__pair_p(_A_(1), p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    return ((__pair__*)__unboxpair(_A_(1), p))->car;
}

__BWORD__ __getCdr ( _S_, __BWORD__ p ) {
    if (__pair_p(_A_(1), p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    return ((__pair__*)__unboxpair(_A_(1), p))->cdr;
}

void __setCar ( _S_, __BWORD__ p, __BWORD__ newcar ) {
    if (__pair_p(_A_(1), p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    ((__pair__*)__unboxpair(_A_(1), p))->car = newcar;
}

void __setCdr ( _S_, __BWORD__ p, __BWORD__ newcdr ) {
    if (__pair_p(_A_(1), p) != __TRUE__) {
        printf("PAIR expected\n");
        exit(__FAIL__);
    }

    ((__pair__*)__unboxpair(_A_(1), p))->cdr = newcdr;
}

/* Constructor: __cons */
__BWORD__  __cons ( _S_, __BWORD__ car, __BWORD__ cdr ) {
    __pair__ *newpair = NULL;
    __BWORD__ bpair;

    newpair = (__pair__*)allocBlock(getHeap(), __PAIRSIZE__);
    newpair->hdr = __PAIR_TYPE__;

    if (newpair == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        bpair = __boxpair(_A_(1), (__WORD__)newpair);
        __setCar(_A_(2), bpair, car);
        __setCdr(_A_(2), bpair, cdr);
    }

    return bpair;
}

/*                   */
/* Symbol procedures */
/*                   */

/* Constructor: stringToSymbol */
__BWORD__ __stringToSymbol ( _S_, __BWORD__ s ) {
    __symbol__ *newsymbol = NULL;
    __WORD__ slen = __unboxint(_A_(1), __stringLength(_A_(1), s));

    if (__string_p(_A_(1), s) != __TRUE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    newsymbol = (__symbol__*)allocBlock(getHeap(), sizeof(__symbol__) + slen + 1);

    if (newsymbol == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newsymbol->hdr = (slen << __SYM_LEN_SHFT__) + __SYM_TYPE__;
        strcpy((char*)s + sizeof(__string__), (char*)newsymbol + sizeof(__symbol__));
        return __boxptd(_A_(1), (__WORD__)newsymbol);
    }
}

__BWORD__ __symbolLength ( _S_, __BWORD__ sym ) {
    return __boxint(_A_(1), (__WORD__)(((__symbol__*)__unboxptd(_A_(1), sym))->hdr >> __SYM_LEN_SHFT__));
}

__BWORD__ __symbolToString ( _S_, __BWORD__ sym ) {
    __string__ *newstring = NULL;
    __BWORD__ slen = __symbolLength(_A_(1), sym);

    if (__string_p(_A_(1), sym) != __TRUE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    newstring = (__string__*)allocBlock(getHeap(), sizeof(__string__) + slen + 1);

    if (newstring == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newstring->hdr = (slen << __SYM_LEN_SHFT__) + __STR_TYPE__;
        strcpy((char*)sym + sizeof(__symbol__), (char*)newstring + sizeof(__string__));
        return __boxptd(_A_(1), (__WORD__)newstring);
    }
}

/*                   */
/* Lambda procedures */
/*                   */

/* Constructor: lambda */
__BWORD__ __createLambda ( _S_, __BWORD__ size ) {
    __lambda__ *newlambda = NULL;

    __WORD__ usize = __unboxint(_A_(1), size);
    newlambda = (__lambda__*)allocBlock(getHeap(), sizeof(__lambda__) + (__WORDSIZE__ * (usize + 1)));

    if (newlambda == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newlambda->hdr = (usize << __VEC_LEN_SHFT__) + __VEC_TYPE__; /* !! Vector disguised as a lambda !! */
        return __boxlambda(_A_(1), (__WORD__)newlambda);
    }
}

void  __lambdaSet ( _S_, __BWORD__ p, __BWORD__ ref, __BWORD__ val ) {
    __vectorSet(_A_(3), __boxptd(_A_(1), __unboxlambda(_A_(1), p)), __inc(_A_(1), ref), val);
}

__BWORD__ __lambdaRef ( _S_, __BWORD__ p, __BWORD__ ref ) {
    return __vectorRef(_A_(2), __boxptd(_A_(1), __unboxlambda(_A_(1), p)), __inc(_A_(1), ref));
}

void __lambdaSetCode ( _S_, __BWORD__ p, __WORD__ code ) {
    __vectorSet(_A_(3), __boxptd(_A_(1), __unboxlambda(_A_(1), p)), 0, code);
}

__WORD__ __lambdaGetCode ( _S_, __BWORD__ p ) {
    if (__lambda_p(_A_(1), p) == __FALSE__) {
        printf("Procedure expected\n");
        exit(__FAIL__);
    }

    return __vectorRef(_A_(2), __boxptd(_A_(1), __unboxlambda(_A_(1), p)), 0);
}

/*                   */
/* Vector procedures */
/*                   */

/* constructor: __createVector */
__BWORD__ __createVector ( _S_, __BWORD__ size ) {
    __vector__ *newvector = NULL;

    __WORD__ usize = __unboxint(_A_(1), size);
    newvector = (__vector__*)allocBlock(getHeap(), sizeof(__vector__) + (__WORDSIZE__ * usize));

    if (newvector == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newvector->hdr = (usize << __VEC_LEN_SHFT__) + __VEC_TYPE__;
        return __boxptd(_A_(1), (__WORD__)newvector);
    }
}

__BWORD__ __vectorRef ( _S_, __BWORD__ v, __BWORD__ ref ) {
    if (__vector_p(_A_(1), v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(_A_(1), ref);
    if ((uref < 0) || (uref >= __unboxint(_A_(1), __vectorLength(_A_(1), v)))) {
        printf("Error: Invalid vector index\n");
        exit(__FAIL__);
    }

    return *((__BWORD__*)__unboxptd(_A_(1), v) + uref + 2);
}

void __vectorSet ( _S_, __BWORD__ v, __BWORD__ ref, __BWORD__ val ) {
    if (__vector_p(_A_(1), v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(_A_(1), ref);
    if ((uref < 0) || (uref >= __unboxint(_A_(1), __vectorLength(_A_(1), v)))) {
        printf("Error: Invalid vector index\n");
        exit(__FAIL__);
    }

    *((__BWORD__*)__unboxptd(_A_(1), v) + uref + 2) = val;
}

__BWORD__ __vectorLength ( _S_, __BWORD__ v ) {
    if (__vector_p(_A_(1), v) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A_(1), ((__vector__*)__unboxptd(_A_(1), v))->hdr >> __VEC_LEN_SHFT__);
}

__BWORD__ __vectorEqual ( _S_, __BWORD__ v1, __BWORD__ v2 ) {
    if (__vector_p(_A_(1), v1) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    if (__vector_p(_A_(1), v2) == __FALSE__) {
        printf("VECTOR expected\n");
        exit(__FAIL__);
    }

    __WORD__ v1Len = __vectorLength(_A_(1), v1);

    if (v1Len != __vectorLength(_A_(1), v2))
        return __FALSE__;

    for (__WORD__ i = 0; i < v1Len; ++i) {
        if (__vectorRef(_A_(2), v1, i) != __vectorRef(_A_(2), v2, i))
            return __FALSE__;
    }

    return __TRUE__;
}

__BWORD__ __vector_p ( _S_, __BWORD__ v ) {
    if (__boxvector_p(_A_(1), v))
        return __TRUE__;
    else
        return __FALSE__;
}

/*                   */
/* String procedures */
/*                   */

/* Constructor: __createString */
__BWORD__ __createString ( char *s ) {
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
        return __boxptd(_A_(1), (__WORD__)newstring);
    }
}


__BWORD__ __makeString(_S_, __BWORD__ len) {
   __string__ *newstring = NULL;
   __WORD__ slen = (__WORD__)__unboxint(_A_(1), len);

    newstring = (__string__*)allocBlock(getHeap(), sizeof(__string__) + slen + 1);

    if (newstring == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newstring->hdr = (slen << __STR_LEN_SHFT__) + __STR_TYPE__;
        memset(newstring + sizeof(__string__), 0, slen+1);
        return __boxptd(_A_(1), (__WORD__)newstring);
    }
}


__BWORD__ __stringLength ( _S_, __BWORD__ s ) {
    if (__string_p(_A_(1), s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A_(1), (((__string__*)(__unboxptd(_A_(1), s)))->hdr) >> __STR_LEN_SHFT__);
}

__BWORD__ __stringRef ( _S_, __BWORD__ s, __BWORD__ ref) {
    __char__ *newchar = NULL;

    if (__string_p(_A_(1), s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(_A_(1), ref);
    if ((uref < 0) || (uref >= __stringLength(_A_(1), s))) {
        printf("Error: Invalid string index\n");
        exit(__FAIL__);
    }

    return __integerToChar(_A_(1), __boxint(_A_(1), *((char*)__unboxptd(_A_(1), s) + uref + sizeof(__char__))));
}

__BWORD__ __stringEqual ( _S_, __BWORD__ s1, __BWORD__ s2 ) {
    if (__string_p(_A_(1), s1) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    if (__string_p(_A_(1), s2) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    __WORD__ s1Len = __unboxint(_A_(1), __stringLength(_A_(1), s1));

    if (s1Len != __unboxint(_A_(1), __stringLength(_A_(1), s2)))
        return __FALSE__;

    for (__WORD__ i = 0; i < s1Len; ++i) {
        if (__stringRef(_A_(2), s1, __boxint(_A_(1), i)) != __stringRef(_A_(2), s2, __boxint(_A_(1), i)))
            return __FALSE__;
    }

    return __TRUE__;

}

__BWORD__ __stringToList ( _S_, __BWORD__ s ) {
    __pair__ *newpair = NULL;
    __pair__ *head = NULL;
    __BWORD__ bpair;
    __BWORD__ ch;
    int slen;

    if (__string_p(_A_(1), s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    newpair = (__pair__*)allocBlock(getHeap(), sizeof(__pair__));
    newpair->hdr = __PAIR_TYPE__;

    if (newpair == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }

    slen = __unboxint(_A_(1), __stringLength(_A_(1), s));
    __setCdr(_A_(2), __boxpair(_A_(1), (__WORD__)newpair), __NULL__);

    for (int i = 0; i < slen; i++) {
        head = (__pair__*)allocBlock(getHeap(), sizeof(__pair__));
        head->hdr = __PAIR_TYPE__;

        if (head == NULL) {
            printf("Out of memory\n");
            exit(__FAIL__);
        }

        __setCdr(_A_(2), __boxpair(_A_(1), (__WORD__)head), __boxpair(_A_(1), (__WORD__)newpair));
        newpair = head;
    }

    bpair = __boxpair(_A_(1), (__WORD__)newpair);
    for (int i = 0; i < slen; i++) {
        ch = __stringRef(_A_(2), s, __boxint(_A_(1), (__WORD__)i));

        __setCar(_A_(2), bpair, ch);
        bpair = __getCdr(_A_(1), bpair);
    }

    return __boxpair(_A_(1), (__WORD__)newpair);
}

void __stringSet ( _S_, __BWORD__ s, __BWORD__ ref, __BWORD__ ch ) {
    if (__string_p(_A_(1), s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    if (__number_p(_A_(1), ref) == __FALSE__) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    __WORD__ uref = __unboxint(_A_(1), ref);
    if ((uref < 0) || (uref >= __stringLength(_A_(1), s))) {
        printf("Error: Invalid string index\n");
        exit(__FAIL__);
    }

    if (__char_p(_A_(1), ch) == __FALSE__) {
        printf("CHAR expected\n");
        exit(__FAIL__);
    }

    __WORD__ bsize = __boxsize(_A_(1), s);
    char chint = (char)__unboxint(_A_(1), __charToInteger(_A_(1), ch));
    *((char*)(__unboxptd(_A_(1), s) + sizeof(__string__) + uref)) = chint;
}

/*                 */
/* Char procedures */
/*                 */

__BWORD__ __charToInteger ( _S_, __BWORD__ ch ) {
    return __unboxchar(_A_(1), ch);
}

/* Constructor: __integerToChar */
__BWORD__ __integerToChar ( _S_, __BWORD__ i ) {
    __char__ *newchar = NULL;

    newchar = (__char__*)allocBlock(getHeap(), sizeof(__char__));

    if (newchar == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }
    else {
        newchar->hdr = ((__BWORD__)__unboxint(_A_(1), i) << __CHAR_VAL_SHFT__) + __CHAR_TYPE__;
        return __boxptd(_A_(1), (__WORD__)newchar);
    }
}

/*                       */
/* Arithmetic procedures */
/*                       */

__BWORD__ __add ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return (a + b);
}

__BWORD__ __inc ( _S_, __BWORD__ a ) {
    return __add(_A_(2), a, __boxint(_A_(1), 1));
}

__BWORD__ __sub ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A_(1), __unboxint(_A_(1), a) - __unboxint(_A_(1), b));
}

__BWORD__ __mul ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A_(1), __unboxint(_A_(1), a) * __unboxint(_A_(1), b));
}

__BWORD__ __quotient ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A_(1), __unboxint(_A_(1), a) / __unboxint(_A_(1), b));
}

__BWORD__ __remainder ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    return __boxint(_A_(1), __unboxint(_A_(1), a) % __unboxint(_A_(1), b));
}

/*                      */
/* Predicate procedures */
/*                      */

__BWORD__ __number_p ( _S_, __BWORD__ n ) {
    if (__boxint_p(_A_(1), n))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__  __null_p ( _S_, __BWORD__ p ) {
    if (p == __NULL__)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __lambda_p ( _S_, __BWORD__ p ) {
    if(__boxlambda_p(_A_(1), p))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __pair_p ( _S_, __BWORD__ p ) {
    if(__boxpair_p(_A_(1), p) && (__null_p(_A_(1), p) == __FALSE__))
        return __TRUE__;
    else
        return __FALSE__;
}


__BWORD__ __list_p ( _S_, __BWORD__ p ) {
    if (__null_p(_A_(1), p) == __TRUE__)
        return __TRUE__;

    if(__boxpair_p(_A_(1), p))
        return __list_p(_A_(1), __getCdr(_A_(1), p));
    else
        return __FALSE__;
}

__BWORD__ __string_p ( _S_, __BWORD__ s ) {
    if ((__boxtype(_A_(1), s) == __PTD_TYPE__) && (__boxsubtype(_A_(1), s) == __STR_TYPE__))
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __char_p ( _S_, __BWORD__ ch ) {
    if ((__boxtype(_A_(1), ch) == __PTD_TYPE__) && (__boxsubtype(_A_(1), ch) == __CHAR_TYPE__))
        return __TRUE__;
    else
        return __FALSE__;
}

/*                        */
/* Logical ops procedures */
/*                        */

__BWORD__ __lt ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a < b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __gt ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a > b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __ge ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a >= b)
        return __TRUE__;
    else
        return __FALSE__;
}

__BWORD__ __le ( _S_, __BWORD__ a, __BWORD__ b ) {
    if ((__number_p(_A_(1), a) == __FALSE__) || (__number_p(_A_(1), b) == __FALSE__)) {
        printf("NUMBER expected\n");
        exit(__FAIL__);
    }

    if (a <= b)
        return __TRUE__;
    else
        return __FALSE__;
}

/*                   */
/* Object procedures */
/*                   */

__BWORD__ __equalPtd ( _S_, __BWORD__ b1, __BWORD__ b2 ) {
    switch(__boxsubtype(_A_(1), b1)) {
    case __VEC_TYPE__:
        return __vectorEqual(_A_(2), b1, b2);
    case __STR_TYPE__:
        return __stringEqual(_A_(2), b1, b2);
    default:
        return __FALSE__;
    }
}

__BWORD__ __equal ( _S_, __BWORD__ a, __BWORD__ b ) {
    if (__boxtype(_A_(1), a) != __boxtype(_A_(1), b))
        return __FALSE__;

    if (__boxtype(_A_(1), a) == __PTD_TYPE__)
        return __equalPtd(_A_(2), a, b);
    else
        return __eq(_A_(2), a, b);
}

__BWORD__ __eq ( _S_, __BWORD__ a, __BWORD__ b ) {
    if (a == b)
        return __TRUE__;
    else
        return __FALSE__;
}


/*                   */
/* Output procedures */
/*                   */

void __display ( _S_, __BWORD__ s ) {
    if (__string_p(_A_(1), s) == __FALSE__) {
        printf("STRING expected\n");
        exit(__FAIL__);
    }

    printf((char*)__unboxptd(_A_(1), s) + sizeof(__string__));
}

void __newline ( _S_ ) {
    __BWORD__ i = __integerToChar(_A_(1), __boxint(_A_(1), __CH_newline__));

    __writeChar(_A_(1), i);
}

void __writeChar ( _S_, __BWORD__ ch ) {
    if (__char_p(_A_(1), ch) == __FALSE__) {
        printf("CHAR expected\n");
        exit(__FAIL__);
    }

    int c = (int)__unboxint(_A_(1), __unboxchar(_A_(1), ch));
    if (putchar(c) != c) {
        printf("Error writing char\n");
        exit(__FAIL__);
    }
}

/*                 */
/* Misc procedures */
/*                 */

void __initStack( _S_ ) {
    allocStack();
}

void __initHeap( _S_ ) {
    allocByteField(getHeap(), __PAIRSIZE__);
}

void __gc ( _S_ ) {
    gc_run(getHeap(), getNewHeap());
}

void __dumpHeap ( _S_ ) {
    dumpByteField(getHeap());
}
