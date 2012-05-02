#include <stdio.h>
#include <stdint.h>

uint32_t globX = 123;

void foo ( ) {
    __asm__ ( "movl %ebp, _globX;" );
}

void __bar ( uint32_t x ) {
    printf("d: %lu\n", x);
}



int main() {
    __asm__ ( "movl %ebp, _globX;" );
    foo();
    __bar(globX);
    return 0;
}
