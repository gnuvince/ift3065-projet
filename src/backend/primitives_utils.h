void print_bword(__BWORD__ v) {
    printf("v: %llu\n", (bword_t)v);
}

void print_pair(__pair__ *p) {
    if (p == NULL)
        printf("NULL Pair\n");
    else {
        printf("Pair\n");
        printf("car: %llu, cdr: %llu\n", (bword_t)p->car, (bword_t)p->cdr);
    }
}

void print_byteField(__bytefield__ *f) {
    if (f == NULL)
        printf("NULL byteField\n");
    else {
        printf("Byte Field\n");
        printf("*field: %llu, fieldsize: %llu, next:%llu\n", (bword_t)f->field, (bword_t)f->fieldsize, (bword_t)f->next);
    }
}

void print_vector(__vector__ *v) {
    if (v == NULL)
        printf("NULL vector\n");
    else {
        printf("Vector\n");
        printf("size: %llu, addr: %llu\n", (bword_t)v->size, (bword_t)v);        
    }
}


