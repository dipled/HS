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
    void* ret = NULL;
    
    ret = mergesort(vet, 3, sizeof(int), intCmp);
    if (ret == NULL)
        return -1;
    
    for (int i = 0; i < 3; ++i)
        printf("%d ", vet[i]);

    return 0;
}