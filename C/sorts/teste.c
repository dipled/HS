#include <stdio.h>
#include "genericsorts.h"

int intCmp(const void *a, const void *b)
{
    if (*(int *)a < *(int *)b)
        return 1;
    if(*(int *)a == *(int *)b)
        return 2;
    return 0;
}

int main()
{
    int nelem = 6;
    int vet[] = {3, 1, 2,42342,-123,231541};
    void* ret = NULL;

    // ret = mergesort(vet, nelem, sizeof(int), intCmp);
    // if (ret == NULL)
    //     return -1;
    
    ret = quicksort(vet, nelem, sizeof(int), intCmp);
    if (ret == NULL)
        return -1;

    for (int i = 0; i < nelem; ++i)
        printf("%d ", vet[i]);
    return 0;
}