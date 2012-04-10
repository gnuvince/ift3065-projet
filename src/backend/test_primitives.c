#include <stdio.h>
#include <stdlib.h>
#include "sins_types.h"
#include "sins_const.h"
#include "box.h"
#include "primitives.h"
#include "primitives_utils.h"

int main(void)
{
    __bytefield__ f;
    __pair__ *p1, *p2, *p3;

    
    allocByteField(&f, 1024, __PAIRSIZE__);
    if (f.field == NULL)
        return -1;

    p1 = __cons(&f, __boxint(3), __boxint(4));
    p2 = __cons(&f, __boxint(5), __boxint(6));
    p3 = __cons(&f, __boxint(7), __boxint(8));

    if (p1 == NULL || p2 == NULL || p3 == NULL) {
        printf("Out of memory\n");
        return -1;
    }
    else {
        print_pair(p1);
        print_pair(p2);
        print_pair(p3);
    }
    
    __BWORD__ a = __getcar(p1);
    __BWORD__ b = __getcdr(p1);
    printf("a: %llu, b: %llu\n", (bword_t)a, (bword_t)b);

    __vector__ *v = __vector(&f, 10);
    if (v == NULL)
        return -1;
    else {
        print_vector(v);
        __vector_set(v, 0, 42);
        print_bword(__vector_ref(v, 0));
    }

    
    free(f.field);
    return 0;
}
