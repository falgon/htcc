// function call test

int printf();
int exit();
int test_num;

int assert(long expected, long actual, char* code)
{
    if (expected == actual) {
        printf("[OK]:statements/func test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:statements/func test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int f() { return 42; }
int add(int x, int y) { return x + y; }
void swap(int* a, int* b) { *a ^= *b; *b ^= *a; *a ^= *b; }
void void_fn(int* a) { *a = 42; return; *a = 53; }
int param_decay(int ar[]) { return ar[0]; }
int rec(int a) { if (a == 0) return 42; return rec(a - 1); } 
int fib(int n) // fibonacci number
{
    if (n == 0) return 1;
    else if (n == 1) return 1;
    else if (n >= 2) return fib(n - 1) + fib(n - 2);
    else return 0;
}

int main()
{
    printf(">>>> tests: statements/func\n");
            
    test_num = 1;
    assert(42, f(), "f()");
    assert(45, f() + 3,  "f() + 3");
    assert(3, add(1, 2), "add(1, 2)");
    assert(44, ({ int b = rec(2); b + 2; }), "({ int b = rec(2); b + 2; })");
    assert(8, fib(5), "fib(5)");
    assert(0, ({ int ar[] = { 0 }; param_decay(ar); }), "({ int ar[] = { 0 }; param_decay(ar); })");
    assert(2, ({ int a = 1; int b = 2; swap(&a, &b); a; }), "({ int a = 1; int b = 2; swap(&a, &b); a; })");
    assert(1, ({ int a = 1; int b = 2; swap(&a, &b); b; }), "({ int a = 1; int b = 2; swap(&a, &b); b; })");
    assert(42, ({ int a = 1; void_fn(&a); a; }), "({ int a = 1; void_fn(&a); a; })");
    //assert(8, ({ int (*fibp)() = fib; fib(5); }), "({ int (*fibp)() = fib; fib(5); })");

    printf("All tests are passed!\n");
    return 0;
}
