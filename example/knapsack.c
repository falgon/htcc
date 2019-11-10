/*
 *
 * Given @n@ items and their respective volume @ct@ and value @vals@ for a knapsack of capacity C, 
 * calculate the maximum sum of the values of the items in 
 * the knapsack so as not to exceed knapsack capacity @capacity@ (See also: https://en.wikipedia.org/wiki/Knapsack_problem).
 * A knapsack can only contain one item of the same type (\(x_i\in\left\{0,1\right\}\).
 *
 */

typedef long time_t;

int printf();
void* calloc(int, int);
void free(void*);
void srand(int);
int rand(void);
time_t time(time_t*);

int max(int a, int b) { return a > b ? a : b; }

// Solving with DP
int knapsack(int capacity, int* ct, int* vals, int n)
{
    int** tr = calloc(n + 1, sizeof(int*));
    for (int i = 0; i < n + 1; ++i) tr[i] = calloc(capacity + 1, sizeof(int));

    for (int i = 0; i <= n; ++i) {
        for (int j = 0; j <= capacity; ++j) {
            if (!i || !j) tr[i][j] = 0;
            else if (ct[i - 1] <= j) tr[i][j] = max(vals[i - 1] + tr[i - 1][j - ct[i - 1]], tr[i - 1][j]);
            else tr[i][j] = tr[i - 1][j];
        }
    }

    int result = tr[n][capacity];

    for (int i = 0; i < n + 1; ++i) {
        free(tr[i]);
        tr[i] = 0;
    }
    free(tr);
    tr = 0;
    
    return result;
}

int* rand_ints(int size, int min, int max)
{
    int* rs = calloc(size, sizeof(int));
    for (int i = 0; i < size; ++i) rs[i] = rand() % max + min;
    return rs;
}

void print(int* first, int* last)
{
    for (; first < last; ++first) printf("%d ", *first);
    printf("\n");
}

int main()
{
    srand(time(0));
    int item_size = rand() % 15 + 4;

    printf("item size: %d\n", item_size);
    
    int* vals = rand_ints(item_size, 50, 200);
    int* capacities = rand_ints(item_size, 10, 30);
    int capacity = rand() % 100 + 50;
    
    printf("value of items: ");
    print(vals, vals + item_size);

    printf("capacity of items: ");
    print(capacities, capacities + item_size);

    printf("capacity: %d\n", capacity);

    printf("retult: %d\n", knapsack(capacity, capacities, vals, item_size));
    
    free(vals);
    free(capacities);
    
    return 0;
}
