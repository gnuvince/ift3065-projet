#include <stdint.h>

typedef uint64_t __WORD__;
typedef __WORD__ __BWORD__;
#define bword_t uint64_t
#define __WORDSIZE__ 8

/* 3 bits for boxing */
#define __BOX_SIZE__  3
#define __BOX_MASK__  7
#define __PAIR_TYPE__ 0
#define __PAIR_SHFT__ 1 
#define __INT_TYPE__  1
#define __INT_SHFT__  3

typedef struct {
    __WORD__ size;
} __vector__;

typedef struct {
    __BWORD__ car;
    __BWORD__ cdr;
} __pair__;

#define __PAIRSIZE__ sizeof(__pair__)

typedef struct {
    void *field;
    __WORD__ fieldsize;
    __WORD__ next;
} __bytefield__;

void allocByteField( __bytefield__ *f, size_t num, size_t size );

void* allocBlock( __bytefield__ *f, __WORD__ size );

int __pair_p( __BWORD__ v );

int __int_p( __BWORD__ v );

__BWORD__ __getcar( __pair__ *p );

__BWORD__ __getcdr( __pair__ *p );

void __setcar( __pair__ *p, __BWORD__ newcar );

void __setcdr( __pair__ *p, __BWORD__ newcdr );

__pair__* __cons( __bytefield__ *f, __BWORD__ car, __BWORD__ cdr );

__vector__* __vector( __bytefield__ *f, __WORD__ size);

__BWORD__ __vector_ref( __vector__* v, __WORD__ ref);

void __vector_set( __vector__* v, __WORD__ ref, __BWORD__ val);

__BWORD__ __box( __WORD__ v, __WORD__ type );

__WORD__ __unbox( __BWORD__ v );


