#ifndef GC_H
#define GC_H

#include "sins_types.h"
#include "bytefield.h"

#define __GC_MASK__  1
#define __GC_ALIVE__ 0
#define __GC_MOVED__ 1

typedef struct __rootNode__ {
    int                  count;
    __BWORD__            *node;
    struct __rootNode__  *next;
} __rootNode_t__;

#define __ROOTNODESIZE__ (sizeof(__rootNode_t__))

typedef struct __frameNode__ {
    struct __rootNode__  *first;
    struct __frameNode__ *next;
} __frameNode_t__;

#define __FRAMENODESIZE__ (sizeof(__frameNode_t__))

void dumpCStack ( );
    
void dumpRootStack ( );

void dumpFrameStack ( );

void pushFrame ( );

void popFrame ( );

void pushRoot ( __BWORD__ *root );

void popRoot ( );

void pushCRoot ( __BWORD__ *root );

void popCRoot ( );

void gc_run ( __bytefield__ *from, __bytefield__ *to );

__BWORD__ gc_copyObject ( __BWORD__ root, __bytefield__ *from, __bytefield__ *to );

void gc_setState ( __BWORD__ obj, __BWORD__ state );

__BWORD__ gc_getState ( __BWORD__ obj );

void gc_setAlive ( __BWORD__ obj );

void gc_setMoved ( __BWORD__ obj, __WORD__ dest);

int gc_isAlive ( __BWORD__ obj );

int gc_isMoved ( __BWORD__ obj );


#endif
