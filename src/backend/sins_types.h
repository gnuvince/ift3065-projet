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

#endif
