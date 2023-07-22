#include "genericsorts.h"

void *quicksort(void *base, size_t nelem, size_t typesize, int (*comparefn)(const void *, const void *))
{
    if (base == NULL || nelem <= 0 || typesize <= 0)
        return NULL;
    if (nelem == 1)
        return base;

    char *mem = (char *)base;
    int lsc = 0, eqc = 0, gtc = 0;

    char *ls = (char *)malloc(typesize);
    char *eq = (char *)malloc(typesize);
    char *gt = (char *)malloc(typesize);

    /* We'll be choosing the first element for our pivot just because it makes things easier, but
       it'd be better to choose a random pivot in our array */
    void *pivot = (void *)mem;

    if (ls == NULL || eq == NULL || gt == NULL)
        return NULL;

    for (size_t i = 0; i < nelem; ++i)
    {
        void *elem = (void *)(mem + i * typesize);
        int relation = comparefn(elem, pivot);

        if (relation == 1)
        {
            assert(memcpy((void *)(ls + lsc * typesize), elem, typesize));

            lsc++;

            assert(realloc((void *)ls, (lsc + 1) * typesize));
        }
        else if (relation == 2)
        {
            assert(memcpy((void *)(eq + eqc * typesize), elem, typesize));

            eqc++;

            assert(realloc((void *)eq, (eqc + 1) * typesize));
        }
        else
        {
            assert(memcpy((void *)(gt + gtc * typesize), elem, typesize));

            gtc++;

            assert(realloc((void *)gt, (gtc + 1) * typesize));
        }
    }
    if (lsc > 0)
    {
        assert(quicksort((void *)ls, lsc, typesize, comparefn));
    }
    if (eqc > 0)
    {
        assert(quicksort((void *)eq, eqc, typesize, comparefn));
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
    if (ls != NULL)
        free(ls);

    if (gt != NULL)
        free(gt);

    if (eq != NULL)
        free(eq);

    return base;
}
