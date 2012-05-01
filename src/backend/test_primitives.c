#include <stdio.h>
#include <stdlib.h>
#include "sins_types.h"
#include "sins_const.h"
#include "box.h"
#include "primitives.h"
#include "primitives_utils.h"
#include "bytefield.h"
#include "bytefield_utils.h"

int main(void)
{
    __initHeap(_A_(0));
    /* allocByteField(getHeap(), __PAIRSIZE__); */

    __newline(_A_(0)); __display(_A_(1), __string("** Pair **")); __newline(_A_(0));

    /* __BWORD__ p1, p2, p3;     */
    /* p1 = __cons(4, __boxint(4)); */
    /* p2 = __cons(p1, __boxint(6)); */
    /* p3 = __cons(p2, __boxint(8)); */
    __VAR__ p1, p2, p3;    
    p1 = allocVar(); p2 = allocVar(); p3 = allocVar();
    setVar(p1, __cons(_A_(2), __boxint(_A_(1), 3), __boxint(_A_(1), 4)));
    setVar(p2, __cons(_A_(2), getVar(p1), __boxint(_A_(1), 6)));
    setVar(p3, __cons(_A_(2), getVar(p2), __boxint(_A_(1), 8)));

    __display(_A_(1), __string("p1: ")); print_pair(getVar(p1)); __newline(_A_(0));
    __display(_A_(1), __string("p2: ")); print_pair(getVar(p2)); __newline(_A_(0));
    __display(_A_(1), __string("p3: ")); print_pair(getVar(p3)); __newline(_A_(0));

    __BWORD__ a = __getCar(_A_(1), getVar(p1));
    __BWORD__ b = __getCdr(_A_(1), getVar(p1));
    __display(_A_(1), __string("car(p1): ")); print_bword(a); __newline(_A_(0));
    __display(_A_(1), __string("cdr(p1): ")); print_bword(b); __newline(_A_(0));

    __newline(_A_(0)); __display(_A_(1), __string("** Vector **")); __newline(_A_(0));
    __BWORD__ v = __vector(_A_(1), __boxint(_A_(1), 1));
    v = __vector(_A_(1), __boxint(_A_(1), 10));
    __display(_A_(1), __string("vectorLength(v): ")); print_bword(__unboxint(_A_(1), __vectorLength(_A_(1), v))); __newline(_A_(0));
    __display(_A_(1), __string("v[0] = 42")); __newline(_A_(0));
    __vectorSet(_A_(3), v, __boxint(_A_(1), 0), __boxint(_A_(1), 42));
    __display(_A_(1), __string("v[0]: ")); print_bword(__vectorRef(_A_(2), v, __boxint(_A_(1), 0))); __newline(_A_(0));
    __display(_A_(1), __string("v[9] = 84")); __newline(_A_(0));
    printf("***\n");
    __vectorSet(_A_(3), v, __boxint(_A_(1), 9), __boxint(_A_(1), 84));
    printf("***\n");
    __display(_A_(1), __string("v[9]: ")); print_bword(__vectorRef(_A_(2), v, __boxint(_A_(1), 9))); __newline(_A_(0));
    
    __newline(_A_(0)); __display(_A_(1), __string("** String **")); __newline(_A_(0));
    __BWORD__ s;
    s = __string("Allo les amis");
    __display(_A_(1), s); __newline(_A_(0));
    dumpByteField(getHeap());
    
    freeByteField(getHeap());
    return 0;
}
