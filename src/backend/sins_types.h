#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>

#ifdef ARCH32

#define word_t      uint32_t
#define wordFormat  "%08lx"
#define intFormat  "%lu"
#define pnum_t      int32_t
/* #define __prologue__  __asm__ ( "movl    %esp, %ebp;" */
/*                                 "pushl   %esp;" */
/*                                 "call    __setLastFrame;" */
/*                                 "addl    $4, %esp;" ); */

#else

#define word_t      uint64_t
#define wordFormat  "%016llx"
#define intFormat  "%llu"
#define pnum_t      int64_t
/* #define __prologue__  __asm__ ( "movq    %esp, %ebp;" */
/*                                 "pushq   %esp;" */
/*                                 "call    __setLastFrame;" */
/*                                 "addq    $8, %esp;" ); */

#endif

/* __asm__ ( "movl $10, %eax;" */
/*                 "movl $20, %ebx;" */
/*                 "addl %ebx, %eax;" */
/*     ); */



#define bword_t word_t

typedef word_t __WORD__;
#define __WORDSIZE__ (sizeof(__WORD__))

typedef bword_t __BWORD__;
#define __BWORDSIZE__ (sizeof(__BWORD__))

/* least significant byte == type pointed   */
/* all previous bytes == length             */
typedef struct {
    __WORD__ hdr;
    __WORD__ state;
} __ptd_hdr__;
#define __PTDHDRSIZE__ (sizeof(__ptd_hdr__)))

typedef __ptd_hdr__ __vector__;
typedef __ptd_hdr__ __string__;
typedef __ptd_hdr__ __char__;
typedef __ptd_hdr__ __sym__;

typedef struct {
    __WORD__  hdr;
    __WORD__  state;
    __BWORD__ car;
    __BWORD__ cdr;
} __pair__;
#define __PAIRSIZE__ (sizeof(__pair__))

#endif
