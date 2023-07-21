#include "genericsorts.h"

void *quicksort(void *base, size_t nelem, size_t typesize, int (*comparefn)(const void*, const void*))
{
    if (base == NULL || nelem <= 1 || typesize <= 0)
        return NULL;

    char *mem = (char*) base;
    int lsc = 0,  eqc = 0, gtc = 0;

    char *ls = (char*) malloc(typesize);
    char *eq = (char*) malloc(typesize);
    char *gt = (char*) malloc(typesize);

    /* We'll be choosing the first element for our pivot just because it makes things easier, but
       it'd be better to choose a random pivot in our array */
    void *pivot = (void*) mem;
    void *ret = NULL;

    if (ls == NULL || eq == NULL || gt == NULL)
        return NULL;

    for (int i = 0; i < nelem; ++i)
    {
        void* elem = (void*) (mem + i*typesize);
        int relation = comparefn(elem, pivot);

        if (relation == 1)
        {
            ret = memcpy((void*) (ls + lsc*typesize), elem, typesize);
            if (ret == NULL)
                return NULL;
            lsc++;

            ret = realloc((void*)ls, (lsc + 1)*typesize);
            if (ret == NULL)
                return NULL;
            
        }
        else if (relation == 2)
        {
            ret = memcpy((void*) eq + eqc*typesize, elem, typesize);
            if (ret == NULL)
                return NULL;
            eqc++;

            ret = realloc((void*)eq, (eqc+1)*typesize);
            if (ret == NULL)
                return NULL;
        }
        else
        {
            ret = memcpy((void*) (gt + gtc*typesize), elem, typesize);
            if (ret == NULL)
                return NULL;
            gtc++;

            ret = realloc((void*)gt, (gtc+1)*typesize);
            if (ret == NULL)
                return NULL;
        }
    }
    if (lsc > 0)
    {
        ret = quicksort((void*)ls, lsc, typesize, comparefn);
        if (ret == NULL)
                return NULL;
    }
    if (eqc > 0)
    {
        ret = quicksort((void*)eq, eqc, typesize, comparefn);
        if (ret == NULL)
                return NULL;
    }
    if (gtc > 0)
    {
        ret = quicksort((void*)gt, gtc, typesize, comparefn);
        if (ret == NULL)
                return NULL;
    }
    
    int count = 0;
    for (int i = 0; i < lsc; ++i)
    {
        memcpy((void*) (mem + count*typesize), (void*) (ls + i*typesize), typesize);
        count++;
    }
    for (int i = 0; i < eqc; ++i)
    {
        memcpy((void*) (mem + count*typesize), (void*) (eq + i*typesize), typesize);
        count++;
    }
    for (int i = 0; i < gtc; ++i)
    {
        memcpy((void*) (mem + count*typesize), (void*) (gt + i*typesize), typesize);
        count++;
    }
    return base;
}

