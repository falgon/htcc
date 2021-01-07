#include "../test_utils.h"
#include <stdio.h>

int test_func1()
{
    return 0 > printf("%s::%s(): [OK]\n", __FILE__, __func__);
}
