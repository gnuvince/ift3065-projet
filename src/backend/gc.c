#include <stdlib.h>
#include <string.h>
#include "gc.h"
#include "sins_const.h"
#include "sins_types.h"
#include "primitives.h"
#include "bytefield.h"
#include "bytefield_utils.h"
#include "box.h"

/* void *globEBP; */

void gc_run ( __bytefield__ *from, __bytefield__ *to ) {
    __BWORD__ obj;
    
    allocByteField(to, __PAIRSIZE__);

    for (__VAR__ i = 0; i < getVarNext(); ++i) {
        obj = getVar(i);
        
        if ((obj != __NULL__) && (__boxtype(_A_(1), obj) != __INT_TYPE__)) {
            setVar(i, gc_copyObject(obj, from, to));
        }
    }

    /* Uncomment to see the results */
    /* printf("Dest heap:\n"); */
    /* dumpByteField(to); */
    /* printf("From heap:\n"); */
    /* dumpByteField(from); */
    
    swapHeap();
    freeByteField(from);
}

__BWORD__ gc_copyObject ( __BWORD__ obj, __bytefield__ *from, __bytefield__ *to ) {
    __WORD__ objsize = __boxsize(_A_(1), obj);
    __WORD__ objtype = __boxtype(_A_(1), obj);
    __BWORD__ newobj;
    void *loc;

    if ((obj != __NULL__) && (objtype != __INT_TYPE__)) {
        
        if (gc_isMoved(obj)) {

            /* point to new obj location */
            return gc_getState(obj);
            
        }  else  {            

            /* Reserve space in new heap */
            loc = allocBlock(to, objsize);        
            if (loc == NULL) {
                printf("Out of memory");
                exit(__FAIL__);
            }

            /* Create new obj */
            newobj = __box(_A_(2), (__WORD__)loc, objtype);
        
            /* Copy the object */
            memcpy(loc, (void*)__unbox(_A_(1), obj), (size_t)objsize);

            /* Copy pair sub-objects */
            if (objtype == __PAIR_TYPE__) {
                __setCar(_A_(2), newobj, gc_copyObject(__getCar(_A_(1), obj), from, to));
                __setCdr(_A_(2), newobj, gc_copyObject(__getCdr(_A_(1), obj), from, to));
            }
        
            /* Copy vector sub-objects */
            else if ((objtype == __PTD_TYPE__) && (__boxsubtype(_A_(1), obj) == __VEC_TYPE__)) {
                for (__WORD__ i = 0; i < __unboxint(_A_(1), __vectorLength(_A_(1), obj)); ++i)
                    __vectorSet(_A_(2), newobj, __boxint(_A_(1), i), gc_copyObject(__vectorRef(_A_(2), obj, __boxint(_A_(1), i)), from, to));
            }
            
            /* Tag object as moved */
            gc_setMoved(obj, newobj);
            return newobj;
            
        }
        
    }  else  {
        /* null or int returned as is */
        return obj;        
    }
}

void gc_setState ( __BWORD__ obj, __BWORD__ state ) {
    __ptd_hdr__ *p = (__ptd_hdr__*)(__unbox(_A_(1), obj));
    p->state = state;
}

__BWORD__ gc_getState ( __BWORD__ obj ) {
    return ((__ptd_hdr__*)(__unbox(_A_(1), obj)))->state;
}

void gc_setAlive ( __BWORD__ obj ) {
    gc_setState(obj, 0);
}

void gc_setMoved ( __BWORD__ obj, __WORD__ dest) {
    gc_setState(obj, dest);
}

int gc_isAlive ( __BWORD__ obj ) {
    return (((__ptd_hdr__*)(__unbox(_A_(1), obj)))->state == 0);
}

int gc_isMoved ( __BWORD__ obj ) {
    return (((__ptd_hdr__*)(__unbox(_A_(1), obj)))->state != 0);
}
