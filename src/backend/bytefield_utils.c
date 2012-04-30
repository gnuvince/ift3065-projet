#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytefield_utils.h"
#include "sins_const.h"
#include "primitives.h"
#include "primitives_utils.h"
#include "box.h"

char *dline = "======================================================================================\n";
char *sline = "--------------------------------------------------------------------------------------\n";

void dumpWord( __bytefield__ *f, __WORD__ w ) {
    printf(wordFormat, w);
}

void dumpAddr( __bytefield__ *f, __WORD__ addr ) {
    dumpWord(f, addr);
    printf("   ");
}

void dumpValue( __bytefield__ *f, __WORD__ val ) {
    dumpWord(f, val);
    printf("        ");
}

void dumpPtdState( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;
    
    dumpAddr(f, addr);
    dumpValue(f, *(__WORD__*)addr);
    printf("state: ");
    dumpWord(f, *(__WORD__*)addr);
    printf("\n");
}

void dumpPairHdr( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;

    dumpAddr(f, addr);
    dumpValue(f, *(__WORD__*)addr);
    printf("pair at: ");
    dumpWord(f, addr);
    printf("\n");
}

void dumpPairState( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;

    dumpAddr(f, addr);
    dumpValue(f, *(__WORD__*)addr);
    printf("state: ");
    dumpWord(f, *(__WORD__*)addr);
    printf("\n");
}

void dumpVecIndex( __bytefield__ *f, __ptd_hdr__ *phdr, __WORD__ i ) {
    __WORD__ cellAddr = (__WORD__)((__WORD__*)((__WORD__)phdr + sizeof(__ptd_hdr__)) + i);
    
    /* Address */
    dumpAddr(f, cellAddr);

    /* Hex value */
    dumpValue(f, *(__WORD__*)cellAddr);
    
    /* Desc */
    printf("v[%llu]: ", i);
    dumpWord(f, *(__WORD__*)cellAddr);
    printf("\n");
}

void dumpVec( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;
    __ptd_hdr__ *phdr = (__ptd_hdr__*)addr;

    /* Address */
    dumpAddr(f, (__WORD__)phdr);

    /* Hex value */
    dumpValue(f, phdr->hdr);
    
    /* Desc */
    __WORD__ vlen = (__WORD__)(phdr->hdr >> __VEC_LEN_SHFT__);
    printf("vector size: %llu\n", vlen);

    /* State */
    dumpPtdState(f, pos + __WORDSIZE__);

    /* Vector content */
    for (__WORD__ i = 0; i < vlen; ++i) {
        dumpVecIndex(f, phdr, i);
    }
}

void dumpStrLine( __bytefield__ *f, __ptd_hdr__ *phdr, __WORD__ i) {
    __WORD__ lineAddr = (__WORD__)((__WORD__*)((__WORD__)phdr + sizeof(__ptd_hdr__)) + i);
    
    /* Address */
    dumpAddr(f, lineAddr);

    /* Hex value */
    dumpValue(f, *(__WORD__*)lineAddr);
    
    /* Desc */
    for (__WORD__ k = 0; k < __WORDSIZE__; ++k)
        putchar((int)*((char*)lineAddr + k));
    printf("\n");
}

void dumpStr( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;
    __ptd_hdr__ *phdr = (__ptd_hdr__*)addr;

    /* Address */
    dumpAddr(f, (__WORD__)phdr);

    /* Hex value */
    dumpValue(f, phdr->hdr);
    
    /* Desc */
    /* __WORD__ slen = (__WORD__)(phdr->hdr >> __STR_LEN_SHFT__); */
    __WORD__ slen = __unboxint(_A1_, __stringLength(_A1_, __boxptd(_A1_, addr)));
    printf("string length: %llu\n", slen);

    /* State */
    dumpPtdState(f, pos + __WORDSIZE__);

    /* String content */
    __WORD__ wholelines = ((slen + 1) / __WORDSIZE__);
    __WORD__ tailline = ((slen + 1) % __WORDSIZE__);
    if (tailline != 0)
        tailline = 1;
    for (__WORD__ i = 0; i < (wholelines + tailline); ++i) {
        dumpStrLine(f, phdr, i);
    }
}

void dumpChar( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;
    __ptd_hdr__ *phdr = (__ptd_hdr__*)addr;

    /* Address */
    dumpAddr(f, (__WORD__)phdr);

    /* Hex value */
    dumpValue(f, phdr->hdr);
    
    /* Desc */
    printf("char: ");
    int intval = (int)(phdr->hdr >> __CHAR_VAL_SHFT__);

    switch(intval) {
    case __CH_newline__:
        printf("newline");
        break;
        
    case __CH_space__:
        printf("space");
        break;
        
    default:
        putchar(intval);
        break;
    }
    
    printf("\n");

    /* State */
    dumpPtdState(f, pos + __WORDSIZE__);
}

void dumpInt( __bytefield__ *f, __WORD__ pos ) {
    /* Address */
    __WORD__ addr = ( __WORD__)f->field + pos;
    __BWORD__ val = *(__BWORD__*)addr;
    dumpAddr(f, addr);

    /* Hex value */
    dumpValue(f, val);
                
    /* Desc */
    printf("Boxed integer: ");
    dumpWord(f, __unboxint(_A1_, val));
    printf("\n");
}

void dumpBoxed( __bytefield__ *f, __WORD__ pos ) {
    /* Address */
    __WORD__ addr = ( __WORD__)f->field + pos;
    __BWORD__ bval = *(__BWORD__*)addr;
    __WORD__ t = __boxtype(_A1_, bval);
    __WORD__ saddr = 0;
    __WORD__ subt;

    if (t == __INT_TYPE__)
        dumpInt(f, pos);
    else {        
        dumpAddr(f, addr);

        /* Hex value */
        dumpValue(f, bval);

        /* Desc */
        printf ("Boxed ");
        switch (t) {
        case __PTD_TYPE__:
            saddr = __unboxptd(_A1_, bval);
            subt = __boxsubtype(_A1_, saddr);
            if (subt == __VEC_TYPE__)
                printf("vector at ");
            else if (subt == __STR_TYPE__)
                printf("string at ");
            else if (subt == __CHAR_TYPE__)
                printf("char at ");
            break;
            
        case __PAIR_TYPE__:
            saddr = __unboxpair(_A1_, bval);
            printf("pair at ");
            break;
            
        case __LAMBDA_TYPE__:
            saddr = __unboxlambda(_A1_, bval);
            printf("lambda at ");
            break;
            
        default:
            printf("unknown type");
            break;
        }
        
        dumpWord(f, saddr);
        printf("\n");
    }    
}

void dumpPtd( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;
    __BWORD__ bptd = __boxptd(_A1_, addr);
    __WORD__ nextpos = pos;
    __WORD__ slen;
    __WORD__ stail;
    __WORD__ spad;
    
    switch (__boxsubtype(_A1_, bptd)) {
    case __VEC_TYPE__:
        dumpVec(f, pos);
        
        nextpos += (__unboxint(_A1_, __vectorLength(_A1_, bptd)) * __WORDSIZE__) + sizeof(__vector__);
        break;
        
    case __STR_TYPE__:
        dumpStr(f, pos);
        
        slen = __unboxint(_A1_, __stringLength(_A1_, bptd)) + 1;
        stail = (slen % __WORDSIZE__);
        spad = 0;
        if (stail != 0)
            spad = __WORDSIZE__ - stail;
        nextpos +=  slen + spad + sizeof(__string__);
        break;
        
    case __CHAR_TYPE__:
        dumpChar(f, pos);
        
        nextpos += sizeof(__char__);
        break;
        
    default:
        printf("Unrecognized object type.");
        exit(__FAIL__);        
    }

    dumpObject(f, nextpos);
}

void dumpPair( __bytefield__ *f, __WORD__ pos ) {
    __WORD__ addr = (__WORD__)f->field + pos;
    __pair__ *p = (__pair__*)addr;

    dumpPairHdr(f, pos);
    dumpPairState(f, pos + __WORDSIZE__);
    dumpBoxed(f, pos + (2 * __WORDSIZE__));
    dumpBoxed(f, pos + (2 * __WORDSIZE__) + __BWORDSIZE__);

    dumpObject(f, pos + __PAIRSIZE__);
}

void dumpLambda( __bytefield__ *f, __WORD__ pos ) {
    printf("Lambda dump not yet available!");
    exit(__FAIL__);
}

void dumpObject( __bytefield__ *f, __WORD__ pos ) {
    __ptd_hdr__ *ptdh;

    if (pos < f->next) {
        printf(sline);
        ptdh = (__ptd_hdr__*)((__WORD__)f->field + pos);
        
        switch (ptdh->hdr & __BOX_MASK__) {
        case __INT_TYPE__:
            dumpInt(f, pos);
            break;
            
        case __PTD_TYPE__:
            dumpPtd(f, pos);
            break;
            
        case __PAIR_TYPE__:
            dumpPair(f, pos);
            break;
            
        case __LAMBDA_TYPE__:
            dumpLambda(f, pos);
            break;
            
        default:
            printf("Unrecognized object type.\n");            
            exit(__FAIL__);
        }
    }
}

void dumpByteField( __bytefield__ *f ) {
    __WORD__ root = (__WORD__)f->field;
    __WORD__ pos = 0;
    __WORD__ end = f->next;
    __WORD__ size = f->fieldsize;

    printf(dline);
    printf("ByteField\n");
    printf("  start: %016llx\n", root);
    printf("  size: %llu\n", size);
    printf("  end: %llu\n", end);

    dumpObject(f, pos);

    printf(dline);
}
