
#if defined(sun) || defined(__sun) || defined(__sun__)
/* Long ago, I became convinced that Sun's qsort() was broken.
   Probably it isn't any more, but I'm still more comfortable
   avoiding it. */

#define MAXSTACK 100

static void exchange(void *a, void *b, size_t size) {
    size_t i;
    int *ai = (int *) a;
    int *bi = (int *) b;
    char *ac;
    char *bc;

    /******************
     *  exchange a,b  *
     ******************/

    for (i = sizeof(int); i <= size; i += sizeof(int)) {
        int t = *ai;
        *ai++ = *bi;
        *bi++ = t;
    }
    ac = (char *) ai;
    bc = (char *) bi;
    for (i = i - sizeof(int) + 1; i <= size; i++) {
        char t = *ac;
        *ac++ = *bc;
        *bc++ = t;
    }
}

static void my_qsort(void *base, size_t nmemb, size_t size,
		     int (*compar)(const void *, const void *)) {
    void *lbStack[MAXSTACK], *ubStack[MAXSTACK];
    int sp;
    unsigned int offset;

    /********************
     *  ANSI-C qsort()  *
     ********************/

    lbStack[0] = (char *)base;
    ubStack[0] = (char *)base + (nmemb-1)*size;
    for (sp = 0; sp >= 0; sp--) {
        char *lb, *ub, *m;
        char *P, *i, *j;

        lb = lbStack[sp];
        ub = ubStack[sp];

        while (lb < ub) {

            /* select pivot and exchange with 1st element */
            offset = (ub - lb) >> 1;
            P = lb + offset - offset % size;
            exchange (lb, P, size);

            /* partition into two segments */
            i = lb + size;
            j = ub;
            while (1) {
                while (i < j && compar(lb, i) > 0) i += size;
                while (j >= i && compar(j, lb) > 0) j -= size;
                if (i >= j) break;
                exchange (i, j, size);
                j -= size;
                i += size;
            }

            /* pivot belongs in A[j] */
            exchange (lb, j, size);
            m = j;

            /* keep processing smallest segment, and stack largest */
            if (m - lb <= ub - m) {
                if (m + size < ub) {
                    lbStack[sp] = m + size;
                    ubStack[sp++] = ub;
                }
                ub = m - size;
            } else {
                if (m - size > lb) {
                    lbStack[sp] = lb; 
                    ubStack[sp++] = m - size;
                }
                lb = m + size;
            }
        }
    }
}

#else
# define my_qsort qsort
#endif
