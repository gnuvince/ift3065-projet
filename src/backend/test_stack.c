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

int main() {

    __BWORD__ v11 = __boxint(_A_(1), 1);
    __BWORD__ v12 = __boxint(_A_(1), 2);
    __BWORD__ v13 = __boxint(_A_(1), 3);
    __BWORD__ v14 = __boxint(_A_(1), 4);
        
    __BWORD__ v21 = __boxint(_A_(1), 5);
    __BWORD__ v22 = __boxint(_A_(1), 6);
    __BWORD__ v23 = __boxint(_A_(1), 7);
    __BWORD__ v24 = __boxint(_A_(1), 8);
        
    __BWORD__ v31 = __boxint(_A_(1), 9);
    __BWORD__ v32 = __boxint(_A_(1), 10);
    __BWORD__ v33 = __boxint(_A_(1), 11);
    __BWORD__ v34 = __boxint(_A_(1), 12);
        
    pushFrame();
    dumpFrameStack();
    pushRoot(&v11);
    pushRoot(&v12);
    pushRoot(&v13);
    pushRoot(&v14);
    dumpRootStack();
    dumpFrameStack();
    
    pushFrame();
    dumpFrameStack();    
    pushRoot(&v21);
    pushRoot(&v22);
    pushRoot(&v23);
    pushRoot(&v24);
    dumpRootStack();
    dumpFrameStack();
    
    pushFrame();
    dumpFrameStack();
    pushRoot(&v31);
    pushRoot(&v32);
    pushRoot(&v33);
    pushRoot(&v34);
    dumpRootStack();
    dumpFrameStack();
    
    popFrame();
    dumpFrameStack();
    
    popFrame();
    dumpFrameStack();
    
    popFrame();
    dumpFrameStack();
    dumpRootStack();
    
    return __SUCCESS__;
}
