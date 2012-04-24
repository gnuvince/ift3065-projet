#include <stdio.h>
#include <stdlib.h>
#include "sins_types.h"
#include "sins_const.h"
#include "box.h"
#include "primitives.h"
#include "primitives_utils.h"
#include "gc.h"
#include "gc_utils.h"
#include "bytefield.h"

int main(void)
{
    allocByteField(getHeap(), __PAIRSIZE__); /* f @ bytefield.h */

    __VAR__ p1;
    p1 = allocVar();

    setVar(p1, __cons(__boxint(3), __boxint(4)));
    setVar(p1, __cons(__boxint(5), __boxint(6)));
    setVar(p1, __cons(__boxint(7), __boxint(8)));

    dumpByteField(getHeap());
    __gc(getHeap(), getNewHeap());
    dumpByteField(getHeap());

    freeByteField(getHeap());
    
    return __SUCCESS__;
}
