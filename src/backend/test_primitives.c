#include <stdio.h>
#include <stdlib.h>
#include "sins_types.h"
#include "sins_const.h"
#include "box.h"
#include "primitives.h"
#include "primitives_utils.h"

int main(void)
{
    allocByteField(1024, __PAIRSIZE__);

    __newline(); __display(__string("** Pair **")); __newline();
    __BWORD__ p1, p2, p3;    
    p1 = __cons(4, __boxint(4));
    p2 = __cons(6, __boxint(6));
    p3 = __cons(8, __boxint(8));
    __display(__string("p1: ")); print_pair(p1); __newline();
    __display(__string("p2: ")); print_pair(p2); __newline();
    __display(__string("p3: ")); print_pair(p3); __newline();
    __BWORD__ a = __getCar(p1);
    __BWORD__ b = __getCdr(p1);
    __display(__string("car(p1): ")); print_bword(a); __newline();
    __display(__string("cdr(p1): ")); print_bword(b); __newline();

    __newline(); __display(__string("** Vector **")); __newline();
    __BWORD__ v = __vector(10);
    __display(__string("vectorLength(v): ")); print_bword(__unboxint(__vectorLength(v))); __newline();
    __display(__string("v[0] = 42")); __newline();
    __vectorSet(v, 0, 42);
    __display(__string("v[0]: ")); print_bword(__vectorRef(v, 0)); __newline();
    __display(__string("v[9] = 84")); __newline();
    __vectorSet(v, 9, 84);
    __display(__string("v[9]: ")); print_bword(__vectorRef(v, 9)); __newline();

    __newline(); __display(__string("** String **")); __newline();
    __BWORD__ s;
    s = __string("Allo les amis");
    __display(s); __newline();
    
    freeByteField();
    return 0;
}
