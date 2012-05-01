#include <stdio.h>
#include <stdlib.h>
#include "sins_types.h"
#include "sins_const.h"
#include "box.h"
#include "primitives.h"
#include "primitives_utils.h"
#include "gc.h"
#include "bytefield.h"
#include "bytefield_utils.h"

char *line = "======================================================================================\n";

int main(void)
{
    __initHeap(_A_(0));
    /* allocByteField(getHeap(), __PAIRSIZE__); */
    /* dumpWord(getHeap(), (__WORD__)__TRUE__); printf("\n"); */
    /* dumpWord(getHeap(), (__WORD__)__FALSE__); printf("\n"); */
    /* dumpWord(getHeap(), (__WORD__)__NULL__); printf("\n"); */

    dumpWord(getHeap(), __add(_A_(2), __boxint(_A_(1), 3), __boxint(_A_(1), 4))); printf("\n");
    
    __VAR__ p1;
    p1 = allocVar();

    printf(line);
    printf("Only 1 boxint var. Heap should be empty before and after gc:\n");
    setVar(p1, __boxint(_A_(1), 42));
    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);

    printf(line);
    printf("Only 1 boxpair var. Heap should contain the pair before and after gc:\n");
    setVar(p1, __cons(_A_(2), __boxint(_A_(1), 3), __boxint(_A_(1), 4)));
    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);

    printf(line);
    printf("Only 1 boxpair var but 2 more pairs created. Heap should contain 3 pairs before and 1 after gc:\n");
    setVar(p1, __cons(_A_(2), __boxint(_A_(1), 5), __boxint(_A_(1), 6)));
    setVar(p1, __cons(_A_(2), __boxint(_A_(1), 7), __boxint(_A_(1), 8)));

    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);

    printf(line);
    printf("2 more boxpair vars, 3 new pairs created. Heap should contain 4 pairs before and 3 after gc:\n");
    __VAR__ p2, p3;
    p2 = allocVar();
    p3 = allocVar();

    setVar(p1, __cons(_A_(2), __boxint(_A_(1), 3), __boxint(_A_(1), 4)));
    setVar(p2, __cons(_A_(2), __boxint(_A_(1), 5), __boxint(_A_(1), 6)));
    setVar(p3, __cons(_A_(2), __boxint(_A_(1), 7), __boxint(_A_(1), 8)));

    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);

    printf(line);
    printf("3 vars. p1 == pair; p2 == vector; p3 == string. Heap should contain 5 pairs before and 3 after gc:\n");
    setVar(p3, __string("**Variable p3**"));
    setVar(p2, __vector(_A_(1), __boxint(_A_(1), 12)));

    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);

    printf(line);
    printf("Set 3 vars to null. Heap should have 3 objects before and 0 after\n");
    setVar(p1, __NULL__);
    setVar(p2, __NULL__);
    setVar(p3, __NULL__);

    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);

    printf(line);
    printf("p1 <= vector(3)\n");
    printf("p1[0] <= string('Allo')\n");
    printf("p2 <= cons(10, 11)\n");
    printf("p1[1] <= p2\n");
    printf("p2 <= cons(5, 6)\n");
    printf("p3 <= vector(5)\n");
    setVar(p1, __vector(_A_(1), __boxint(_A_(1), 3)));
    __vectorSet(_A_(2), getVar(p1), __boxint(_A_(1), 0), __string("Allo"));
    setVar(p2, __cons(_A_(2), __boxint(_A_(1), 10), __boxint(_A_(1), 11)));
    __vectorSet(_A_(2), getVar(p1), __boxint(_A_(1), 1), getVar(p2));
    setVar(p2, __cons(_A_(2), __boxint(_A_(1), 5), __boxint(_A_(1), 6)));
    setVar(p3, __vector(_A_(1), __boxint(_A_(1), 5)));

    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);
    
    freeByteField(getHeap());
    
    return __SUCCESS__;
}
