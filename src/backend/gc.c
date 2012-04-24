#include <stdlib.h>
#include <string.h>
#include "gc.h"
#include "gc_utils.h"
#include "sins_const.h"
#include "sins_types.h"
#include "primitives.h"
#include "bytefield.h"
#include "box.h"


void __gc( __bytefield__ *from, __bytefield__ *to ) {
    __BWORD__ obj;
    
    allocByteField(to, __PAIRSIZE__);
    
    for (__VAR__ i = 0; i < getVarNext(); ++i) {
        obj = getVar(i);
        
        if ((obj != __NULL__) && (__boxtype(obj) != __INT_TYPE__) && (gc_isAlive(obj))) {
            setVar(i, gc_copyObject(obj, from, to));
        }
    }

    swapHeap();
    freeByteField(from);
}

__BWORD__ gc_copyObject( __BWORD__ obj, __bytefield__ *from, __bytefield__ *to ) {
    __WORD__ objsize = __boxsize(obj);
    __WORD__ objtype = __boxtype(obj);
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
        newobj = __box((__WORD__)loc, objtype);
        
        /* Copy the object */
        memcpy(loc, (void*)__unbox(obj), (size_t)objsize);

        /* Copy the sub-objects */
        if (objtype == __PAIR_TYPE__) {
            __setCar(newobj, gc_copyObject(__getCar(obj), from, to));
            __setCdr(newobj, gc_copyObject(__getCdr(obj), from, to));
        }

        else if (objtype == __VEC_TYPE__) {
            for (__WORD__ i = 0; i < __vectorLength(obj); ++i)
                __vectorSet(newobj, i, gc_copyObject(__vectorRef(obj, i), from, to));
        }
        
        /* Tag object as moved */
        gc_setMoved(obj);

        return newobj;
    }

    return obj;
}

void gc_setFlags( __BWORD__ obj, __WORD__ flags ) {
    __ptd_hdr__ *p = (__ptd_hdr__*)(__unbox(obj));
    p->state = (p->state | flags);
}

void gc_setAlive( __BWORD__ obj ) {
    gc_setFlags(obj, __GC_ALIVE__);
}

void gc_setMoved( __BWORD__ obj ) {
    gc_setFlags(obj, __GC_MOVED__);
}

int gc_isAlive( __BWORD__ obj ) {
    return ((((__ptd_hdr__*)(__unbox(obj)))->state & __GC_MASK__) == __GC_ALIVE__);
}

int gc_isMoved( __BWORD__ obj ) {
    return ((((__ptd_hdr__*)(__unbox(obj)))->state & __GC_MASK__) == __GC_MOVED__);
}
