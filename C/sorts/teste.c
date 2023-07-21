#include <stdio.h>
#include "genericsorts.h"

int intCmp(const void *a, const void *b)
{
    if (*(int *)a < *(int *)b)
        return 1;
    if (*(int *)a == *(int *)b)
        return 2;
    return 0;
}

int main()
{

    int nelem = 6;
    int vet[] = {3, 1, 2, 42342, -123, 231541};
    int *vet2 = (int *)malloc(sizeof(int) * nelem);
    void *ret = NULL;
    ret = memcpy((void *)vet2, (void *)vet, sizeof(int) * nelem);
    if (ret == NULL)
        return -1;

    {
        ret = mergesort(vet, nelem, sizeof(int), intCmp);
        if (ret == NULL)
            return -1;
        printf("Quick-Sort: ");
        for (int i = 0; i < nelem; ++i)
            printf("%d ", vet[i]);
        printf("\n\n");
    }
    {
        ret = quicksort(vet2, nelem, sizeof(int), intCmp);
        if (ret == NULL)
            return -1;
        printf("Merge-Sort: ");
        for (int i = 0; i < nelem; ++i)
            printf("%d ", vet2[i]);
        printf("\n\n");
    }
    return 0;
}