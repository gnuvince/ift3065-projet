#ifndef SINS_CONST_H
#define SINS_CONST_H

#include "sins_types.h"

#define __FALSE__ ~(__WORD__)0 - 13

/****************************************************/
/*                                                  */
/* Boxed types Pattern                              */
/* ------------------------------------------------ */
/* int         all box bits set to 0                */
/* pair        last 2 bits 11                       */
/* pointed     last 2 bits 01                       */
/* ascii       last 2 bits 10                       */
/*                                                  */
/****************************************************/

#define __BOX_SIZE__  2
#define __BOX_MASK__  3
#define __SUB_MASK__  255
#define __CHR_HMASK__ (~(__WORD__)0 >> (__WORDSIZE__ * 4)) << (__WORDSIZE__ * 4)

#define __INT_TYPE__  0
#define __INT_SHFT__  2
#define __PTD_TYPE__  1
#define __PTD_SHFT__  0
#define __PTD_VEC__   255
#define __PTD_STR__   0
#define __CHAR_TYPE__ 2
#define __CHAR_SHFT__ 8
#define __PAIR_TYPE__ 3
#define __PAIR_SHFT__ 0

#define __VEC_TYPE__ 255
#define __STR_TYPE__ 0


#endif
