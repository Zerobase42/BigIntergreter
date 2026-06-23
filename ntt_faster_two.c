#ifdef __linux__
#include <unistd.h>
#define syscall(a, b, buf, pos) ((a) == 0 ? read(b, buf, pos) : write(b, buf, pos))
#else
#include <stdio.h>
#include <string.h>
#define syscall(a, b, buf, pos) ((a) == 0 ? read(b, buf, pos) : write(b, buf, pos))
#define read(n, buf, pos) fread(buf, 1, pos, stdin)
#define write(n, wbuf, pos) fwrite(wbuf, 1, pos, stdout)
#endif
#include <omp.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
#define ll long long
#define max(a, b) ((a) > (b) ? (a) : (b))
#define DIG 6
/*
double M=(double)mod1*mod2,N=NUMLEN;
int DIG=(int)((log10(M)-log10(N))/2.0);
*/
#define NUMLEN 1000000
#define BASE 1000000  // 10^DIG
#define MAX 1048576   // 2^N <= NUMLEN
const ll w1 = 3;
const ll w2 = 3;
const ll mod1 = 998244353;
const ll mod2 = 1004535809;
const ll inv_mod1 = 669690699;  // powmod(mod1,mod2-2,mod2)
static __inline ll powmod1(ll a, ll b) {
    ll ret = 1;
    while (b) {
        if (b & 1) ret = ret * a % mod1;
        b >>= 1;
        a = a * a % mod1;
    }
    return ret;
}
static __inline ll powmod2(ll a, ll b) {
    ll ret = 1;
    while (b) {
        if (b & 1) ret = ret * a % mod2;
        b >>= 1;
        a = a * a % mod2;
    }
    return ret;
}
static ll root[MAX >> 1];
static int rev[MAX];
static __inline void init_root1(int n, unsigned char inv) {
    ll ang = powmod1(w1, (mod1 - 1) / n);
    if (inv) ang = powmod1(ang, mod1 - 2);
    root[0] = 1;
    for (int i = 1; i < (n >> 1); i++) {
        root[i] = root[i - 1] * ang % mod1;
    }
}
static __inline void init_root2(int n, unsigned char inv) {
    ll ang = powmod2(w2, (mod2 - 1) / n);
    if (inv) ang = powmod2(ang, mod2 - 2);
    root[0] = 1;
    for (int i = 1; i < (n >> 1); i++) {
        root[i] = root[i - 1] * ang % mod2;
    }
}
static __inline void ntt1(ll* f, int n, unsigned char inv) {
    int L = 31 - __builtin_clz(n);
    for (int i = 1; i < n; i++) {
        rev[i] = (rev[i >> 1] | (i & 1) << L) >> 1;
        if (rev[i] < i) {
            ll tmp = f[i];
            f[i] = f[rev[i]];
            f[rev[i]] = tmp;
        }
    }
    ll ang = powmod1(w1, (mod1 - 1) / n);
    if (inv) ang = powmod1(ang, mod1 - 2);
    for (int i = 2; i <= n; i <<= 1) {
        int step = n / i;
#pragma omp parallel for schedule(static)
        for (int j = 0; j < n; j += i) {
            for (int k = 0; k < (i >> 1); k++) {
                ll u = f[j | k];
                ll v = f[j | k | i >> 1] * root[step * k] % mod1;
                u += v, v = u - (v << 1);
                if (u >= mod1) u -= mod1;
                if (v < 0) v += mod1;
                f[j | k] = u;
                f[j | k | i >> 1] = v;
            }
        }
    }
    if (inv) {
        ll t = powmod1(n, mod1 - 2);
#pragma omp parallel for schedule(static)
        for (int i = 0; i < n; i++) {
            f[i] = f[i] * t % mod1;
        }
    }
}
static __inline void ntt2(ll* f, int n, unsigned char inv) {
    int L = 31 - __builtin_clz(n);
    for (int i = 1; i < n; i++) {
        rev[i] = (rev[i >> 1] | (i & 1) << L) >> 1;
        if (rev[i] < i) {
            ll tmp = f[i];
            f[i] = f[rev[i]];
            f[rev[i]] = tmp;
        }
    }
    ll ang = powmod2(w2, (mod2 - 1) / n);
    if (inv) ang = powmod2(ang, mod2 - 2);
    for (int i = 2; i <= n; i <<= 1) {
        int step = n / i;
#pragma omp parallel for schedule(static)
        for (int j = 0; j < n; j += i) {
            for (int k = 0; k < (i >> 1); k++) {
                ll u = f[j | k];
                ll v = f[j | k | i >> 1] * root[step * k] % mod2;
                u += v, v = u - (v << 1);
                if (u >= mod2) u -= mod2;
                if (v < 0) v += mod2;
                f[j | k] = u;
                f[j | k | i >> 1] = v;
            }
        }
    }
    if (inv) {
        ll t = powmod2(n, mod2 - 2);
#pragma omp parallel for schedule(static)
        for (int i = 0; i < n; i++) {
            f[i] = f[i] * t % mod2;
        }
    }
}
static ll a[MAX], b[MAX], c1[MAX], c2[MAX];
static __inline void conv1(ll* _a, ll* _b, int n, ll* c) {
    init_root1(n, 0);
    ntt1(a, n, 0);
    ntt1(b, n, 0);
#pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        c[i] = (__int128_t)a[i] * b[i] % mod1;
    }
    init_root1(n, 1);
    ntt1(c, n, 1);
}
static __inline void conv2(ll* _a, ll* _b, int n, ll* c) {
    init_root2(n, 0);
    ntt2(a, n, 0);
    ntt2(b, n, 0);
#pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        c[i] = (__int128_t)a[i] * b[i] % mod2;
    }
    init_root2(n, 1);
    ntt2(c, n, 1);
}
static __inline void conv(ll* _a, int na, ll* _b, int nb, ll* c) {
/*
memcpy(a,_a,na*sizeof(ll));
memcpy(b,_b,nb*sizeof(ll));
memset(a+na,0,(n-na)*sizeof(ll));
memset(b+nb,0,(n-nb)*sizeof(ll));
*/
#pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        a[i] = (i < na) ? _a[i] : 0;
        b[i] = (i < nb) ? _b[i] : 0;
    }
    int n = 1 << (32 - __builtin_clz(na + nb - 2));
    conv1(a, b, nb, n, c1);
    conv2(a, b, nb, n, c2);
#pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        ll x = c1[i], y = c2[i];
        __int128_t z = y - x;
        if (z < 0) z += mod2;
        ll k = z * inv_mod1 % mod2;
        c[i] = (__int128_t)x + (__int128_t)mod1 * k;
    }
}
static char io_buf[(NUMLEN << 1) + 10], tmp[20];
static ll A[MAX], B[MAX], C[MAX];
int main() {
    int len = syscall(0, 0, io_buf, (NUMLEN << 1) + 3), mid = 0, idx = 0, p, i, j, l;
    io_buf[len] = 0;
    while (io_buf[mid] > ' ') mid++;
    for (p = mid; io_buf[p] && io_buf[p] <= ' '; p++);
    for (len = p; io_buf[len] > ' '; len++);
    int la = mid, lb = len - p;
    int na = (la + DIG - 1) / DIG, nb = (lb + DIG - 1) / DIG;
    for (i = la; i > 0; i -= DIG) {
        l = max(0, i - DIG);
        ll r = 0;
        for (j = l; j < i; j++) r = r * 10 + (io_buf[j] - '0');
        A[idx++] = r;
    }
    idx = 0;
    for (i = len; i > p; i -= DIG) {
        l = max(p, i - DIG);
        ll r = 0;
        for (j = l; j < i; j++) r = r * 10 + (io_buf[j] - '0');
        B[idx++] = r;
    }
    if ((na == 1 && A[0] == 0) || (nb == 1 && B[0] == 0)) {
        syscall(1, 1, "0", 1);
        return 0;
    }
    int nc = na + nb - 1;
    __int128_t carry = 0;
    conv(A, na, B, nb, C);
    for (i = 0; i < nc; ++i) {
        __int128_t x = C[i] + carry;
        C[i] = (ll)x % BASE, carry = x / BASE;
    }
    while (carry) C[nc++] = carry % BASE, carry /= BASE;
    while (nc > 1 && C[nc - 1] == 0) nc--;
    int pos = 0, t = 0;
    ll x = C[nc - 1];
    while (x) tmp[t++] = x % 10 + '0', x /= 10;
    if (t == 0) tmp[t++] = '0';
    while (t--) io_buf[pos++] = tmp[t];
    for (i = nc - 2; i >= 0; --i) {
        x = C[i];
        for (j = 0; j < DIG; j++) tmp[j] = x % 10 + '0', x /= 10;
        for (j = DIG - 1; j >= 0; j--) io_buf[pos++] = tmp[j];
    }
    syscall(1, 1, io_buf, pos);
}