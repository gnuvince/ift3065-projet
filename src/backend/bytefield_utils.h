#ifndef GC_UTILS_H
#define GC_UTILS_H

#include "sins_types.h"
#include "bytefield.h"

void dumpWord( __bytefield__ *f, __WORD__ w );

void dumpAddr( __bytefield__ *f, __WORD__ addr );

void dumpValue( __bytefield__ *f, __WORD__ val );

void dumpPtdState( __bytefield__ *f, __WORD__ pos );

void dumpPairHdr( __bytefield__ *f, __WORD__ pos );

void dumpPairState( __bytefield__ *f, __WORD__ pos );

void dumpVecIndex( __bytefield__ *f, __ptd_hdr__ *phdr, __WORD__ i );

void dumpVec( __bytefield__ *f, __WORD__ pos );

void dumpStrLine( __bytefield__ *f, __ptd_hdr__ *phdr, __WORD__ i);

void dumpStr( __bytefield__ *f, __WORD__ pos );

void dumpChar( __bytefield__ *f, __WORD__ pos );

void dumpInt( __bytefield__ *f, __WORD__ pos );

void dumpBoxed( __bytefield__ *f, __WORD__ pos );

void dumpPtd( __bytefield__ *f, __WORD__ pos );

void dumpPair( __bytefield__ *f, __WORD__ pos );

void dumpLambda( __bytefield__ *f, __WORD__ pos );

void dumpObject( __bytefield__ *f, __WORD__ pos );

void dumpByteField( __bytefield__ *f );

#endif
