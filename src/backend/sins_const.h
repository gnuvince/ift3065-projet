#ifndef SINS_CONST_H
#define SINS_CONST_H

#include "sins_types.h"

/****************************************************/
/*                                                  */
/* Boxed types   Pattern                            */
/* ------------------------------------------------ */
/*   int         all box bits set to 0              */
/*   pointed     last 2 bits 01                     */
/*   pair        last 2 bits 10                     */
/*   lambda      last 2 bits 11                     */
/*                                                  */
/****************************************************/

#define __FAIL__           1
#define __SUCCESS__        0

#define __BOX_SIZE__       2
#define __BOX_MASK__       3
#define __SUB_MASK__       255
#define __CHAR_MASK__      255

/* Base types */
#define __INT_TYPE__           0    /* x x x 0 0 */
#define __PTD_TYPE__           1    /* x x x 0 1 */
#define __PAIR_TYPE__          2    /* x x x 1 0 */
#define __LAMBDA_TYPE__        3    /* x x x 1 1 */
/* PTD subtypes */
#define __VEC_TYPE__           5    /* 0 0 1 0 1 */
#define __STR_TYPE__           9    /* 0 1 0 0 1 */
#define __CHAR_TYPE__         13    /* 0 1 1 0 1 */
#define __SYM_TYPE__          17    /* 1 0 0 0 1 */

#define __INT_SHFT__          2
#define __PTD_SHFT__          0
#define __PAIR_SHFT__         0
#define __LAMBDA_SHFT__       0
#define __VEC_LEN_SHFT__      8
#define __LAMBDA_LEN_SHFT__   8
#define __STR_LEN_SHFT__      8
#define __SYM_LEN_SHFT__      8
#define __CHAR_VAL_SHFT__     8

/* Std calling protocol dummy params */
#define _A_(NUM_ARGS) __NULL__, NUM_ARGS
#define _A1_ __NULL__, 1
#define _A2_ __NULL__, 2
#define _S_ __BWORD__ this, int num

#define __FALSE__ ((__BWORD__) __PTD_TYPE__)                       /* 1 */
#define __TRUE__  (((__BWORD__)1 << __BOX_SIZE__) + __PTD_TYPE__)  /* 5 */
#define __NULL__  ((__BWORD__) __PAIR_TYPE__)                      /* 2 */

/* ASCII chars */
#define __CH_0__    48
#define __CH_1__    49
#define __CH_2__    50
#define __CH_3__    51
#define __CH_4__    52
#define __CH_5__    53
#define __CH_6__    54
#define __CH_7__    55
#define __CH_8__    56
#define __CH_9__    57
   
#define __CH_A__    65
#define __CH_B__    66
#define __CH_C__    67
#define __CH_D__    68
#define __CH_E__    69
#define __CH_F__    70
#define __CH_G__    71
#define __CH_H__    72
#define __CH_I__    73
#define __CH_J__    74
#define __CH_K__    75
#define __CH_L__    76
#define __CH_M__    77
#define __CH_N__    78
#define __CH_O__    79
#define __CH_P__    80
#define __CH_Q__    81
#define __CH_R__    82
#define __CH_S__    83
#define __CH_T__    84
#define __CH_U__    85
#define __CH_V__    86
#define __CH_W__    87
#define __CH_X__    88
#define __CH_Y__    89
#define __CH_Z__    90

#define __CH_a__    97
#define __CH_b__    98
#define __CH_c__    99
#define __CH_d__    100
#define __CH_e__    101
#define __CH_f__    102
#define __CH_g__    103
#define __CH_h__    104
#define __CH_i__    105
#define __CH_j__    106
#define __CH_k__    107
#define __CH_l__    108
#define __CH_m__    109
#define __CH_n__    110
#define __CH_o__    111
#define __CH_p__    112
#define __CH_q__    113
#define __CH_r__    114
#define __CH_s__    115
#define __CH_t__    116
#define __CH_u__    117
#define __CH_v__    118
#define __CH_w__    119
#define __CH_x__    120
#define __CH_y__    121
#define __CH_z__    122

#define __CH_newline__    10
#define __CH_space__      32

#endif
