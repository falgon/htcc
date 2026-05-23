#include <stdint.h>

int64_t sum7(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, int64_t g)
{
    return a + b + c + d + e + f + g;
}

int64_t sum16(int64_t a,int64_t b ,int64_t c,int64_t d,int64_t e,int64_t f, int64_t sta,int64_t stb, int64_t stc, int64_t std, int64_t ste, int64_t stf, int64_t stg, int64_t sth, int64_t sti, int64_t stj)
{
    return a + b + c + d + e + f + sta - stb + stc - std + ste - stf + stg - sth + sti - stj;
}
