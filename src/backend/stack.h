#ifndef STACK_H
#define STACK_H

#include "sins_types.h"

#define __STACKSIZE__ (1 << 20)

typedef struct {
    void *top;
    int size;
} __stack__;

void allocStack ( );

void freeStack ( );

void* getStackTop ( );

void* get_esp_c ( );

void* get_ebp_c ( );

void* get_esp_s ( );

void* get_ebp_s ( );

#endif
