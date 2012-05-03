#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "stack.h"
#include "sins_const.h"

__stack__ stack;
void *esp_c;
void *ebp_c;
void *esp_s;
void *ebp_s;


void allocStack ( ) {
    stack.top = (void*)calloc((size_t)__STACKSIZE__, 1);
    if (stack.top == NULL) {
        printf("Out of memory\n");
        exit(__FAIL__);
    }

    stack.size = __STACKSIZE__;
    esp_s = stack.top + stack.size;
    ebp_s = esp_s;
    /* printf("allocStack:\n"); */
    /* printf("stack.top: 0x%08lx\n", (__WORD__)stack.top); */
    /* printf("stack.size: 0x%08lx\n", (__WORD__)stack.size); */
    /* printf("esp_c: 0x%08lx\n", (__WORD__)esp_c); */
    /* printf("ebp_c: 0x%08lx\n", (__WORD__)ebp_c); */
    /* printf("esp_s: 0x%08lx\n", (__WORD__)esp_s); */
    /* printf("ebp_s: 0x%08lx\n", (__WORD__)ebp_s); */
}

void freeStack ( ) {
    free(stack.top);
}

void* getStackTop ( ) {
    return (stack.top + stack.size);
}

void* get_esp_c ( ) {
    return &esp_c;
}

void* get_ebp_c ( ) {
    return &ebp_c;
}

void* get_esp_s ( ) {
    return &esp_s;
}

void* get_ebp_s ( ) {
    return &ebp_s;
}
