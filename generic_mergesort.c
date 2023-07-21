#include <stdlib.h>
#include <memory.h>


void mg(void *vet, int s, int mid, int e, size_t typeSize, int (*compare)(const void*, const void*))
{
    int len1 = mid - s + 1;
    int len2 = e - mid;
    // Using char* to be able to manipulate elements from the vector (that now is void*).
    // We use memcpy and pointer arith to copy elements from the vector.
    // The use of char* fits nicely because char is one byte long, so if we're working, for example, with int values.
    // We know we must increment the array by 4 bytes to get to the next position.
    // That's why we use char*, so that arr += 4 increments the address in four bytes.
    // OBS: We could probably use our void* to perform these operations, but it isn't complient with C Standard,
    // the Standard is to use char* to perform these operations.
    char *arr = (char *)vet;
    char arr1[len1 * typeSize];
    char arr2[len2 * typeSize];

    // Constructing the aux arrays.
    for (int x = 0; x < len1; x++)
    {
        memcpy(arr1 + x * typeSize, arr + (s + x) * typeSize, typeSize);
    }
    for (int x = 0; x < len2; x++)
    {
        memcpy(arr2 + x * typeSize, arr + (mid + 1 + x) * typeSize, typeSize);
    }

    int i, j, k;
    i = 0;
    j = 0;
    k = s;

    while (i < len1 && j < len2)
    {
        // We use these void ptrs to make use of our compare function
        void *elem1;
        void *elem2;
        elem1 = (void *)(arr1 + i * typeSize);
        elem2 = (void *)(arr2 + j * typeSize);
        if (compare(elem1, elem2))
        {
            memcpy(arr + k * typeSize, arr1 + i * typeSize, typeSize);
            ++i;
        }
        else
        {
            memcpy(arr + k * typeSize, arr2 + j * typeSize, typeSize);
            ++j;
        }
        ++k;
    }
    while (i < len1)
    {
        memcpy(arr + k * typeSize, arr1 + i * typeSize, typeSize);
        ++i;
        ++k;
    }
    while (j < len2)
    {
        memcpy(arr + k * typeSize, arr2 + j * typeSize, typeSize);
        ++j;
        ++k;
    }
}

void ms(void *vet, int s, int e, size_t typeSize,int (*compare)(const void*, const void*))

{
    if (s >= e)
    {
        return;
    }

    int mid = (s + e) / 2;

    ms(vet, s, mid, typeSize,compare);
    ms(vet, mid + 1, e, typeSize,compare);
    mg(vet, s, mid, e, typeSize, compare);
}

int cmp(const void *a, const void *b)
{
    // int compare function
    // For this generic mergesort algorithm, we must implement a function that returns 1 or 0
    // I didn't figure out how to use the "real" cmp function, i.e, the one that returns void*
    if (*(int *)a <= *(int *)b)
        return 1;
    return 0;
}

int main()
{
    int vet[] = {34234, 1, 0, 9, 7, 4, 6, 6, 6};

    int x;
    int y;
    ms(vet, 0, 8, sizeof(int), cmp);
    return 0;
}
