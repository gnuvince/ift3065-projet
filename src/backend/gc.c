#include <stdlib.h>
#include <string.h>
#include "gc.h"
#include "sins_const.h"
#include "sins_types.h"
#include "primitives.h"
#include "bytefield.h"
#include "bytefield_utils.h"
#include "box.h"


void gc_run ( __bytefield__ *from, __bytefield__ *to ) {
    __BWORD__ obj;
    
    allocByteField(to, __PAIRSIZE__);
    
    for (__VAR__ i = 0; i < getVarNext(); ++i) {
        obj = getVar(i);
        
        if ((obj != __NULL__) && (__boxtype(_A1_, obj) != __INT_TYPE__) && (gc_isAlive(obj))) {
            setVar(i, gc_copyObject(obj, from, to));
        }
    }

    swapHeap();
    freeByteField(from);
}

__BWORD__ gc_copyObject ( __BWORD__ obj, __bytefield__ *from, __bytefield__ *to ) {
    __WORD__ objsize = __boxsize(_A1_, obj);
    __WORD__ objtype = __boxtype(_A1_, obj);
    __BWORD__ newobj;
    void *loc;

    if ((obj != __NULL__) && (objtype != __INT_TYPE__) && (gc_isAlive(obj))) {
        /* Reserve space in new heap */
        loc = allocBlock(to, objsize);        
        if (loc == NULL) {
            printf("Out of memory");
            exit(__FAIL__);
        }

        /* Create new obj */
        newobj = __box(_A2_, (__WORD__)loc, objtype);
        
        /* Copy the object */
        memcpy(loc, (void*)__unbox(_A1_, obj), (size_t)objsize);

        /* Copy the sub-objects */
        if (objtype == __PAIR_TYPE__) {
            __setCar(_A2_, newobj, gc_copyObject(__getCar(_A1_, obj), from, to));
            __setCdr(_A2_, newobj, gc_copyObject(__getCdr(_A1_, obj), from, to));
        }

        else if ((objtype == __PTD_TYPE__) && (__boxsubtype(_A1_, obj) == __VEC_TYPE__)) {
            for (__WORD__ i = 0; i < __unboxint(_A1_, __vectorLength(_A1_, obj)); ++i)
                __vectorSet(_A2_, newobj, __boxint(_A1_, i), gc_copyObject(__vectorRef(_A2_, obj, __boxint(_A1_, i)), from, to));
        }
        
        /* Tag object as moved */
        gc_setMoved(obj);

        return newobj;
    }

    return obj;
}

void gc_setFlags ( __BWORD__ obj, __WORD__ flags ) {
    __ptd_hdr__ *p = (__ptd_hdr__*)(__unbox(_A1_, obj));
    p->state = (p->state | flags);
}

void gc_setAlive ( __BWORD__ obj ) {
    gc_setFlags(obj, __GC_ALIVE__);
}

void gc_setMoved ( __BWORD__ obj ) {
    gc_setFlags(obj, __GC_MOVED__);
}

int gc_isAlive ( __BWORD__ obj ) {
    return ((((__ptd_hdr__*)(__unbox(_A1_, obj)))->state & __GC_MASK__) == __GC_ALIVE__);
}

int gc_isMoved ( __BWORD__ obj ) {
    return ((((__ptd_hdr__*)(__unbox(_A1_, obj)))->state & __GC_MASK__) == __GC_MOVED__);
}
