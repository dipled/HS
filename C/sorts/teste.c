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

int main(void)
{
    size_t nelem = 9999999;
    int *vet = (int *)malloc(nelem * sizeof(int));
    int *vet2 = (int *)malloc(sizeof(int) * nelem);
    assert(vet);
    assert(vet2);
    for (int i = 0; i < nelem; ++i)
    {
        vet[i] = rand()%200;
    }
    memcpy((void *)vet2, (void *)vet, sizeof(int) * nelem);
    {
        assert(mergesort((void *)vet, nelem, sizeof(int), intCmp));
        printf("Merge-Sort: ");
        for (int i = 0; i < nelem; ++i)
            printf("%d ", vet[i]);
        printf("\n\n");
    }
    free(vet);
    {
        assert(quicksort((void *)vet2, nelem, sizeof(int), intCmp));
        printf("Quick-Sort: ");
        for (int i = 0; i < nelem; ++i)
            printf("%d ", vet2[i]);
        printf("\n\n");
    }
    if (vet2 != NULL)
        free(vet2);
    return 0;
}