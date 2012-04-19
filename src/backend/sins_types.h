#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include "sins_types.h"

#define word_t  uint64_t
#define bword_t word_t

typedef word_t __WORD__;
#define __WORDSIZE__ sizeof(__WORD__)

typedef bword_t __BWORD__;
#define __BWORDSIZE__ sizeof(__BWORD__)

/* least significant byte == type pointed   */
/* all previous bytes == length             */
typedef struct {
    __WORD__ state;
    __WORD__ hdr;
} __ptd_hdr__;

typedef __ptd_hdr__ __vector__;
typedef __ptd_hdr__ __string__;
typedef __ptd_hdr__ __char__;

typedef struct {
    __WORD__ state;
    __BWORD__ car;
    __BWORD__ cdr;
} __pair__;

#define __PAIRSIZE__ (sizeof(__pair__))

typedef struct {
    void *field;
    __WORD__ fieldsize;
    __WORD__ next;
} __bytefield__;

#endif
