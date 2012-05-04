#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "sins_const.h"
#include "sins_types.h"
#include "primitives.h"
#include "bytefield.h"
#include "bytefield_utils.h"
#include "box.h"

/* static __rootNode_t__*  rootStack  = NULL; */
/* static frameNodePtr frameStack = NULL; */
static __rootNode_t__   *rootStack  = NULL;
static __frameNode_t__  *frameStack = NULL;

void dumpRootStack ( ) {
    /* __rootNode_t__* rootNode = rootStack; */
    __rootNode_t__ *rootNode = rootStack;

    printf("===============================\n");
    printf("Root Stack\n");
    while (rootNode != NULL) {
        printf("-------------------------------\n");
        printf("Root node at   : 0x%08lx\n", (__WORD__)rootNode);
        printf("Object at      : 0x%08lx\n", (__WORD__)rootNode->node);
        printf("Value          : 0x%08lx\n", (__WORD__)*(rootNode->node));
        printf("Next at        : 0x%08lx\n", (__WORD__)rootNode->next);
        rootNode = rootNode->next;
    }
    printf("===============================\n");
}

void dumpFrameStack ( ) {
    __frameNode_t__* frameNode = frameStack;

    printf("===============================\n");
    printf("Frame Stack\n");
    while (frameNode != NULL) {
        printf("-------------------------------\n");
        printf("Frame node at  : 0x%08lx\n", (__WORD__)frameNode);
        printf("Top root at    : 0x%08lx\n", (__WORD__)(frameNode->first));
        printf("Next at        : 0x%08lx\n", (__WORD__)frameNode->next);
        frameNode = frameNode->next;
    }
    printf("===============================\n");
    
}

void pushFrame ( ) {
    __frameNode_t__ *newframe = (__frameNode_t__*)calloc(1, __FRAMENODESIZE__);

    if (newframe == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }

    newframe->first = NULL;
    newframe->next  = (__frameNode_t__*)frameStack;
    frameStack = newframe;
}

void popFrame ( ) {
    printf("popFrame\n");
    __frameNode_t__ *frame = frameStack;
    __rootNode_t__ *firstroot  = rootStack;
    __rootNode_t__ *nextroot;
    __rootNode_t__ *nextfirstroot;

    if (frameStack == NULL) {
        printf("No frame to pop\n");
        exit(__FAIL__);
    }

    nextroot = firstroot->next;
    if (frame->next == NULL)
        nextfirstroot = NULL;
    else
        nextfirstroot = (frame->next)->first;
    
    while ((firstroot != nextfirstroot) && (firstroot != NULL)) {
        printf("free(firstroot)\n");
        free(firstroot);
        rootStack = nextroot;
        firstroot = nextroot;
        if (firstroot != NULL)
            nextroot = firstroot->next;
        else
            nextroot = NULL;
    }
    printf("free(frame)\n");
    frameStack = frameStack->next;
    free(frame);
}

void pushRoot ( __BWORD__ *root ) {
    /* __rootNode_t__* newroot = (__rootNode_t__*)calloc(1, __ROOTNODESIZE__); */
    __rootNode_t__ *newroot = (__rootNode_t__*)calloc(1, __ROOTNODESIZE__);

    if (newroot == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }

    newroot->node = root;
    newroot->next = rootStack;
    rootStack = newroot;
    frameStack->first = newroot;
}

void popRoot ( ) {
    /* __rootNode_t__* root = rootStack; */
    __rootNode_t__ *root = rootStack;
    
    frameStack->first = root->next;
    rootStack = root->next;
    free(root);
}

void gc_run ( __bytefield__ *from, __bytefield__ *to ) {
    int            num;
    __BWORD__      obj;
    __WORD__       *globals;
    __rootNode_t__ *roots = rootStack;
    __rootNode_t__ *croots;

#ifdef KJSLKSJDLDFJK    
    allocByteField(to, __PAIRSIZE__);

    /* Process scheme roots */
    while (roots != NULL) {
        obj = *(root->node);
        if ((obj != __NULL__) && (__boxtype(_A_(1), obj) != __INT_TYPE__)) {
            *(root->node) = gc_copyObject(obj, from, to);
        }
        roots = roots->next;
    }

    /* Process global vars */
    __asm__ __volatile__ ("movl    $_TOTAL_VARIABLES_, %1    \n\t" 
              :
              :"m"(globals)         /* input */
              :
              );

    num = (int)*globals;

    for (int i = 0; i < num; i++) {
        obj = *((__BWORD__*)globals + i + 1);
        if ((obj != __NULL__) && (__boxtype(_A_(1), obj) != __INT_TYPE__)) {
            *((__BWORD__*)globals + i + 1) = gc_copyObject(obj, from, to);
        }
    }

    
    
    /* Process C roots */
    croots = getCStack();
    while (croots->node != NULL) {
        obj = *(croots->node);
        if ((obj != __NULL__) && (__boxtype(_A_(1), obj) != __INT_TYPE__)) {
            *(croots->node) = gc_copyObject(obj, from, to);
        }
        croots = (__rootNode__*)croots->next;
    }
    
    /* while (roots != NULL) { */
    /*     obj = *(root->node); */
    /*     if ((obj != __NULL__) && (__boxtype(_A_(1), obj) != __INT_TYPE__)) { */
    /*         *(root->node) = gc_copyObject(obj, from, to); */
    /*     } */
    /*     roots = roots->next; */
    /* } */



    /* Uncomment to see the results */
    /* printf("Dest heap:\n"); */
    /* dumpByteField(to); */
    /* printf("From heap:\n"); */
    /* dumpByteField(from); */
    
    swapHeap();
    freeByteField(from);
#endif    
}

/* void gc_run ( __bytefield__ *from, __bytefield__ *to ) { */
/*     __BWORD__ obj; */
    
/*     allocByteField(to, __PAIRSIZE__); */

/*     for (__VAR__ i = 0; i < getVarNext(); ++i) { */
/*         obj = getVar(i); */
        
/*         if ((obj != __NULL__) && (__boxtype(_A_(1), obj) != __INT_TYPE__)) { */
/*             setVar(i, gc_copyObject(obj, from, to)); */
/*         } */
/*     } */

/*     /\* Uncomment to see the results *\/ */
/*     /\* printf("Dest heap:\n"); *\/ */
/*     /\* dumpByteField(to); *\/ */
/*     /\* printf("From heap:\n"); *\/ */
/*     /\* dumpByteField(from); *\/ */
    
/*     swapHeap(); */
/*     freeByteField(from); */
/* } */

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


