#ifndef GC_H
#define GC_H

#include "sins_types.h"
#include "bytefield.h"

#define __GC_MASK__  1
#define __GC_ALIVE__ 0
#define __GC_MOVED__ 1

void gc_run ( __bytefield__ *from, __bytefield__ *to );

__BWORD__ gc_copyObject ( __BWORD__ root, __bytefield__ *from, __bytefield__ *to );

void gc_setState ( __BWORD__ obj, __BWORD__ state );

__BWORD__ gc_getState ( __BWORD__ obj );

void gc_setAlive ( __BWORD__ obj );

void gc_setMoved ( __BWORD__ obj, __WORD__ dest);

int gc_isAlive ( __BWORD__ obj );

int gc_isMoved ( __BWORD__ obj );


#endif
