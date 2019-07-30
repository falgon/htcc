#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int test_func2(int n)
{
    if (n < 2) return n;

    const size_t size = n - 3;
    bool* p = calloc(size, sizeof(bool));
    
    printf("%s::%s() outputs: \"", __FILE__, __func__);
    
    for (size_t i = 0; i < size; ++i) {
        if (!p[i]) {
            for (size_t j = i + 1; j < size; ++j) {
                if (!((j + 2) % (i + 2))) p[j] = true;
            }
            printf("%ld ", i + 2);
        }
    }
    
    puts("\" [OK]");
    free(p);
    p = NULL;

    return n;
}
