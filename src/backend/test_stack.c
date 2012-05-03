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
#include "stack.h"

char *line = "======================================================================================\n";

int main(void)
{
    void **esp_c = get_esp_c();
    void **ebp_c = get_esp_c();
    void **esp_s = get_esp_s();
    void **ebp_s = get_esp_s();

    __initHeap(_A_(0));
    __initStack(_A_(0));

    printf("Inits done\n");

    __asm__ ( "pushl   %eax\n\t"
              "movl    _esp_c, %eax\n\t"
              "movl    %esp, (%eax)\n\t"
              "popl    %eax" );
    
    /* __asm__ ( "movl %esp, (_esp_c)\n\t" */
    /*           "movl (_esp_s), %esp\n\t" */
    /*           "movl %ebp, (_ebp_c)\n\t" */
    /*           "movl (_ebp_s), %ebp" ); */
    
    printf("esp/ebp done\n");
    
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
    setVar(p3, __createString("**Variable p3**"));
    setVar(p2, __createVector(_A_(1), __boxint(_A_(1), 12)));

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
    setVar(p1, __createVector(_A_(1), __boxint(_A_(1), 3)));
    __vectorSet(_A_(2), getVar(p1), __boxint(_A_(1), 0), __createString("Allo"));
    setVar(p2, __cons(_A_(2), __boxint(_A_(1), 10), __boxint(_A_(1), 11)));
    __vectorSet(_A_(2), getVar(p1), __boxint(_A_(1), 1), getVar(p2));
    setVar(p2, __cons(_A_(2), __boxint(_A_(1), 5), __boxint(_A_(1), 6)));
    setVar(p3, __createVector(_A_(1), __boxint(_A_(1), 5)));

    dumpByteField(getHeap());
    gc_run(getHeap(), getNewHeap());
    dumpByteField(getHeap());
    printf(line);
    
    freeByteField(getHeap());

    /* __asm__ ( "movl %esp, _esp_s\n\t" */
    /*           "movl _esp_c, %esp\n\t" */
    /*           "movl %ebp, _ebp_s\n\t" */
    /*           "movl _ebp_c, %ebp" ); */

    freeStack();
    
    return __SUCCESS__;
}
