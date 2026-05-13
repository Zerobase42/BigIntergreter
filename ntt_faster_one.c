#ifdef __linux__
#include <unistd.h>
#else
#include <stdio.h>
#include <string.h>
#define syscall(a, b, buf, pos) ((a) == 0 ? read(b, buf, pos) : write(b, buf, pos))
#define read(n, buf, pos) fread(buf, 1, pos, stdin)
#define write(n, wbuf, pos) fwrite(wbuf, 1, pos, stdout)
#endif
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
#define ll long long
#define max(a, b) ((a) > (b) ? (a) : (b))
#define DIG 4
#define NUMLEN 1000000
#define BASE 10000
#define MAX 1048576  // 1<<20
const ll w = 3;
const ll mod = 998244353;

ll powmod(ll a, ll b) {
    ll ret = 1;
    while (b) {
        if (b & 1) ret = ret * a % mod;
        b >>= 1;
        a = a * a % mod;
    }
    return ret;
}
static ll root[MAX >> 1];
static int rev[MAX];
void ntt(ll* f, int n, bool inv = 0) {
    int L = 31 - __builtin_clz(n);
    for (int i = 1; i < n; i++) {
        rev[i] = (rev[i >> 1] | (i & 1) << L) >> 1;
        if (rev[i] < i) {
            ll tmp = f[i];
            f[i] = f[rev[i]];
            f[rev[i]] = tmp;
        }
    }
    ll ang = powmod(w, (mod - 1) / n);
    if (inv) ang = powmod(ang, mod - 2);
    root[0] = 1;
    for (int i = 1; i < (n >> 1); i++) root[i] = root[i - 1] * ang % mod;
    for (int i = 2; i <= n; i <<= 1) {
        int step = n / i;
        for (int j = 0; j < n; j += i) {
            for (int k = 0; k < (i >> 1); k++) {
                ll u = f[j | k], v = f[j | k | i >> 1] * root[step * k] % mod;
                f[j | k] = (u + v) % mod;
                f[j | k | i >> 1] = (u - v) % mod;
                if (f[j | k | i >> 1] < 0) f[j | k | i >> 1] += mod;
            }
        }
    }
    ll t = powmod(n, mod - 2);
    if (inv) {
        for (int i = 0; i < n; i++) f[i] = f[i] * t % mod;
    }
}
static ll a[MAX], b[MAX];
void conv(ll* _a, int na, ll* _b, int nb, ll* c) {
    int L = 32 - __builtin_clz(na + nb - 1), n = 1 << L;
    for (int i = 0; i < n; i++) a[i] = (i < na) ? _a[i] : 0, b[i] = (i < nb) ? _b[i] : 0;
    ntt(a, n, 0);
    ntt(b, n, 0);
    for (int i = 0; i < n; i++) c[i] = (__int128_t)a[i] * b[i] % mod;
    ntt(c, n, 1);
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
    for (i = len; i > mid + 1; i -= DIG) {
        l = max(mid + 1, i - DIG);
        ll r = 0;
        for (j = l; j < i; j++) r = r * 10 + (io_buf[j] - '0');
        B[idx++] = r;
    }
    int nc = na + nb - 1;
    ll carry = 0;
    conv(A, na, B, nb, C);
    for (i = 0; i < nc; ++i) {
        ll x = C[i] + carry;
        C[i] = x % BASE, carry = x / BASE;
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