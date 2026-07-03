#define DEBUG

#ifdef __linux__
#include <unistd.h>
// #define syscall(a,b,buf,pos)((a)==0?read(0,buf,pos):write(1,buf,pos))
#define fread(buf, a, pos, std) read(0, buf, pos)
#define fwrite(buf, a, pos, std) write(1, buf, pos)
#else
#include <stdio.h>
#include <string.h>
#ifndef _STRING_H
#define _STRING_H
#endif
#endif  // __linux__
#ifdef DEBUG
#include <stdio.h>
#define debug(format, args...) fprintf(stderr, format, ##args)
#endif
// #include<omp.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,sse4,avx,avx2,fma")
#define u32 unsigned int
#define u64 unsigned long long
#define max(a, b) ((a) > (b) ? (a) : (b))
#define DIG 6
/*
double M=(double)mod1*mod2,N=NUMLEN;
int DIG=(int)((log10(M)-log10(N))/2.0);
*/
#define NUMLEN 1000000
#define BASE 1000000  // 10^DIG
#define MAX 1048576   // 2^N <= NUMLEN
#ifdef __cplusplus
constexpr
#else
const
#endif
    u32 w1 = 3,
        w2 = 3, mod1 = 998244353, mod2 = 1004535809, inv_mod1 = 669690699;  // mod1^{-1}=powmod(mod1,mod2-2,mod2)
static u32 root[MAX >> 1], rev[MAX], lastRev;
static u32 a[MAX], b[MAX], c1[MAX], c2[MAX];
static char io_buf[(NUMLEN << 1) + 5], tmp[20];
static u32 A[MAX], B[MAX], na, nb;
static u64 C[MAX];
static __inline u32 powmod1(u32 n, u32 e) {
    u32 ret = 1;
    while (e) {
        if (e & 1) ret = (u64)ret * n % mod1;
        e >>= 1;
        n = (u64)n * n % mod1;
    }
    return ret;
}
static __inline u32 powmod2(u32 n, u32 e) {
    u32 ret = 1;
    while (e) {
        if (e & 1) ret = (u64)ret * n % mod2;
        e >>= 1;
        n = (u64)n * n % mod2;
    }
    return ret;
}
static __inline void init_root1(int n, unsigned char inv) {
    u32 ang = powmod1(w1, (mod1 - 1) / n);
    if (inv) ang = powmod1(ang, mod1 - 2);
    root[0] = 1;
    for (int i = 1; i < (n >> 1); i++) {
        root[i] = (u64)root[i - 1] * ang % mod1;
    }
}
static __inline void init_root2(int n, unsigned char inv) {
    u32 ang = powmod2(w2, (mod2 - 1) / n);
    if (inv) ang = powmod2(ang, mod2 - 2);
    root[0] = 1;
    for (int i = 1; i < (n >> 1); i++) {
        root[i] = (u64)root[i - 1] * ang % mod2;
    }
}
// up is good. down is something else wrong. ???
static __inline void ntt1(u32* f, int n) {
    for (int i = 1; i < n - 1; i++) {
        if (rev[i] < i) {
            u32 tmp = f[i];
            f[i] = f[rev[i]];
            f[rev[i]] = tmp;
        }
    }
    int step = n >> 1;
    for (int len = 2; len <= n; len <<= 1) {
        for (int j = 0; j < n; j += len) {
            for (int k = 0; k < (len >> 1); k++) {
                u32 u = f[j + k], v = (u64)f[j + k + (len >> 1)] * root[step * k] % mod1;
                u += v;
                v = (u64)u + mod1 - ((u64)v << 1);
                if (u >= mod1) u -= mod1;
                if (v >= mod1) v -= mod1;
                f[j + k] = u;
                f[j + k + (len >> 1)] = v;
            }
        }
        step >>= 1;
    }
}
static __inline void ntt2(u32* f, int n) {
    for (int i = 1; i < n - 1; i++) {
        if (rev[i] < i) {
            u32 tmp = f[i];
            f[i] = f[rev[i]];
            f[rev[i]] = tmp;
        }
    }
    int step = n >> 1;
    for (int len = 2; len <= n; len <<= 1) {
        for (int j = 0; j < n; j += len) {
            for (int k = 0; k < (len >> 1); k++) {
                u32 u = f[j + k], v = (u64)f[j + k + (len >> 1)] * root[step * k] % mod2;
                u += v;
                v = (u64)u + mod2 - (v << 1);
                if (u >= mod2) u -= mod2;
                if (v >= mod2) v -= mod2;
                f[j + k] = u;
                f[j + k + (len >> 1)] = v;
            }
        }
        step >>= 1;
    }
}
static __inline void conv1(u32* c, int n) {  // A * B
    init_root1(n, 0);
#ifdef _STRING_H
    memset(a, 0, n * sizeof(u32));
    memset(b, 0, n * sizeof(u32));
    memcpy(a, A, na * sizeof(u32));
    memcpy(b, B, nb * sizeof(u32));
#else
    for (int i = 0; i < n; i++) {
        a[i] = (i < na) ? A[i] : 0;
        b[i] = (i < nb) ? B[i] : 0;
    }
#endif
    ntt1(a, n);
    ntt1(b, n);
    for (int i = 0; i < n; i++) {
        c[i] = (u64)a[i] * b[i] % mod1;
    }
    init_root1(n, 1);
    ntt1(c, n);
    u32 t = powmod1(n, mod1 - 2);
    for (int i = 0; i < n; i++) {
        c[i] = (u64)c[i] * t % mod1;
    }
}
static __inline void conv2(u32* c, int n) {  // A * B
    init_root2(n, 0);
#ifdef _STRING_H
    memset(a, 0, n * sizeof(u32));
    memset(b, 0, n * sizeof(u32));
    memcpy(a, A, na * sizeof(u32));
    memcpy(b, B, nb * sizeof(u32));
#else
    for (int i = 0; i < n; i++) {
        a[i] = (i < na) ? A[i] : 0;
        b[i] = (i < nb) ? B[i] : 0;
    }
#endif
    ntt2(a, n);
    ntt2(b, n);
    for (int i = 0; i < n; i++) {
        c[i] = (u64)a[i] * b[i] % mod2;
    }
    init_root2(n, 1);
    ntt2(c, n);
    u32 t = powmod2(n, mod2 - 2);
    for (int i = 0; i < n; i++) {
        c[i] = (u64)c[i] * t % mod2;
    }
}
static __inline void conv(u64* c) {  // A * B
    int n = na + nb - 1, clz = __builtin_clz(n - 1);
    int N = 1 << (32 - clz);
    if (lastRev != N) {
        int L = clz - 1;
        for (int i = 1; i < N; i++) {
            rev[i] = (rev[i >> 1] | (i & 1) << L) >> 1;
        }
        lastRev = N;
    }
    conv1(c1, N);
    conv2(c2, N);
    for (int i = 0; i < n; i++) {
        u32 x = c1[i], y = c2[i];
        u32 z = y + mod2 - x;
        if (z >= mod2) z -= mod2;
        u32 k = (u64)z * inv_mod1 % mod2;
        c[i] = (u64)x + (u64)mod1 * k;
    }
}
int main() {
    int len = fread(io_buf, 1, (NUMLEN << 1) + 3, (__acrt_iob_func(0))), idx = 0, p, i, j, l;
    io_buf[len] = 0;
    while (io_buf[na] & 16) na++;
    p = na;
    while (!(io_buf[p] & 16)) p++;
    while (len > 0 && !(io_buf[len - 1] & 16)) len--;
    // bit hack : goto file/coding/fastio2.webarchive
    nb = len - p;
    for (i = na; i > 0; i -= DIG) {
        l = max(0, i - DIG);
        u32 r = 0;
        for (j = l; j < i; j++) r = r * 10 + (io_buf[j] & 15);
        A[idx++] = r;
    }
    debug("p=%d len=%d\n", p, len);
    debug("B string=\"");
    for (int i = p; i < len; i++) debug("%c", io_buf[i]);
    debug("\"\n");
    idx = 0;
    for (i = len; i > p; i -= DIG) {
        l = max(p, i - DIG);
        u32 r = 0;
        for (j = l; j < i; j++) r = r * 10 + (io_buf[j] & 15);
        B[idx++] = r;
    }

    na = (na + DIG - 1) / DIG, nb = (nb + DIG - 1) / DIG;
    if ((na == 1 && A[0] == 0) || (nb == 1 && B[0] == 0)) {
        fwrite((char*)"0", 1, 1, (__acrt_iob_func(1)));
        return 0;
    }
    int nc = na + nb - 1;
    __uint128_t carry = 0;
    conv(C);
    for (i = 0; i < nc; ++i) {
        carry += C[i];
        C[i] = carry % BASE;
        carry /= BASE;
    }
    while (carry) C[nc++] = carry % BASE, carry /= BASE;
    while (nc > 1 && C[nc - 1] == 0) nc--;
    int t = 0;
    idx = 0;
    u32 x = C[nc - 1];
    while (x) tmp[t++] = (x % 10) | 48, x /= 10;
    if (t == 0) tmp[t++] = '0';
    while (t--) io_buf[idx++] = tmp[t];
    for (i = nc - 2; i >= 0; --i) {
        x = C[i];
        // unroll-loops
        for (j = 0; j < DIG; j++)
            tmp[j] = (x % 10) | 48, x /= 10;
        for (j = DIG - 1; j >= 0; j--)
            io_buf[idx++] = tmp[j];
    }
    fwrite(io_buf, 1, idx, (__acrt_iob_func(1)));
}

// TODO : init_root func to add Barret Reduction(only include divl in asm)
// switch syscall input to mmap
// 119<<23+1 —> 7<<26+1 = 469762049 (much biggger, smaller), 5<<25+1=167772161
// if can, switch 7<<60+1(u64)
/* find the safe DIG. this can save mod1*mod2=1002772198720536577 : 1e18, max mul can (BASE-1)^2=>
6 then len:12 under, overflow u32
7:14under, 8:16under
9  then len:18 under, maximum
*/
// TODO: switch more faster I/O