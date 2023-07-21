#include <stdio.h>
#include "genericsorts.h"

int intCmp(const void *a, const void *b)
{
    if (*(int *)a <= *(int *)b)
        return 1;
    return 0;
}

int main()
{
    int vet[] = {3, 1, 2};

    mergesort(vet, 3, sizeof(int), intCmp);
    for (int i = 0; i < 3; ++i)
        printf("%d ", vet[i]);

    return 0;
}