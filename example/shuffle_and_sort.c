/*
 *
 * After shuffling several sequences with the Fishers shuffle algorithm, 
 * sort them in ascending order with quick sort.
 *
 */

typedef long time_t;

void srand(unsigned);
int rand(void);
time_t time(time_t*);
int printf();

void swap(int* a, int* b)
{
    if (a == b) return;
    *a ^= *b, *b ^= *a, *a ^= *b;
}

void shuffle(int* first, int* last)
{
    if (first == last) first;
    int distance = last - first;
    srand(time(0));
    for (--last, --distance; first < last; ++first, --distance) 
        swap(first, first + rand() % distance + 1);
}

void iota(int* first, int* last, int val)
{
    for (; first < last; ++first, ++val) *first = val;
}

void print(int* first, int* last)
{
    for (; first < last; ++first) printf("%d ", *first);
    printf("\n");
}

int* max(int* a, int* b)
{
    return *a < *b ? b : a;
}

int* min(int* a, int* b)
{
    return max(a, b) == a ? b : a;
}

int med3(int* a, int* b, int* c)
{
    return max(min(a, b), min(max(a, b), c));
}

int quick_sort(int* first, int* last)
{
    if (first == last) return;
    swap(first, med3(first, first + ((last - first) >> 1), last));

    int* l = first;
    int* r = last - 1;
    
    while (l < r) {
        for (; *r > *first; --r);
        for (; *l <= *first && l < r; ++l);
        swap(l, r);
    }
    swap(first, l);
    quick_sort(first, l);
    quick_sort(l + 1, last);
}

int main()
{
    int ar[10];
    int size = sizeof ar / sizeof *ar;

    iota(ar, ar + size);
    shuffle(ar, ar + size);
    
    printf("Before sorting: ");
    print(ar, ar + size);

    quick_sort(ar, ar + size);

    printf("After sorting: ");
    print(ar, ar + size);

    return 0;
}
