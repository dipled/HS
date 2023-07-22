#include "genericsorts.h"
#include <stdio.h>
void *quicksort(void *base, size_t nelem, size_t typesize, int (*comparefn)(const void *, const void *))
{
    if (base == NULL || nelem <= 0 || typesize <= 0)
        return NULL;
    if (nelem == 1)
        return base;

    char *mem = (char *)base;
    int lsc = 0, eqc = 0, gtc = 0;

    char * ls = NULL;
    char * gt = NULL;
    char * eq = NULL;
    ls = (char *)malloc(typesize);
    eq = (char *)malloc(typesize);
    gt = (char *)malloc(typesize);
    void* elem;
    /* We'll be choosing the first element for our pivot just because it makes things easier, but
       it'd be better to choose a random pivot in our array */
    void* pivot;
    pivot = base;

    if (ls == NULL || eq == NULL || gt == NULL)
        return NULL;

    for (size_t i = 0; i < nelem; i++)
    {
        elem = (void*)(mem + i * typesize);
        int relation = comparefn(elem, pivot);

        if (relation == 1)
        {
            assert(memcpy((void *)(ls + lsc * typesize), elem, typesize));

            lsc++;
            ls = (char*)realloc((void *)ls, (lsc + 1) * typesize);
            assert(ls);
        }
        else if (relation == 2)
        {
            assert(memcpy((void *)(eq + eqc * typesize), elem, typesize));

            eqc++;
            eq = (char*) realloc((void *)eq, (eqc+1) * typesize);
            assert(eq);
        }
        else 
        {
            assert(memcpy((void *)(gt + gtc * typesize), elem, typesize));

            gtc++;

            gt = (char*)realloc((void *)gt, (gtc +1) * typesize);
            assert(gt);
        }
    }
    if (lsc > 0)
    {
        assert(quicksort((void *)ls, lsc, typesize, comparefn));
    }
    if (gtc > 0)
    {
        assert(quicksort((void *)gt, gtc, typesize, comparefn));
    }

    int count = 0;
    for (int i = 0; i < lsc; ++i)
    {
        assert(memcpy((void *)(mem + count * typesize), (void *)(ls + i * typesize), typesize));
        count++;
    }
    for (int i = 0; i < eqc; ++i)
    {
        assert(memcpy((void *)(mem + count * typesize), (void *)(eq + i * typesize), typesize));
        count++;
    }
    for (int i = 0; i < gtc; ++i)
    {
        assert(memcpy((void *)(mem + count * typesize), (void *)(gt + i * typesize), typesize));
        count++;
    }

    if (gt != NULL)
        free(gt);
    if (ls != NULL)
        free(ls);

    if (eq != NULL)
        free(eq);
        

    return base;
}
