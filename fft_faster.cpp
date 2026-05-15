#ifdef __linux__
#include <unistd.h>
#else
#include <stdio.h>
#define syscall(a, b, buf, pos) ((a) == 0 ? read(b, buf, pos) : write(b, buf, pos))
#define read(n, buf, pos) fread(buf, 1, pos, stdin)
#define write(n, buf, pos) fwrite(buf, 1, pos, stdout)
#endif
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
#define ll long long
#define max(a, b) ((a) > (b) ? (a) : (b))
#define DIG 4
#define NUMLEN 1000000
#define BASE 10000
#define MAX 1048576
const double PI = 3.14159265358979323846;
static double in_r[MAX], in_i[MAX];
#define ARR ((NUMLEN + DIG - 1) / DIG + 5)
static ll A[ARR], B[ARR], C[ARR << 1];
#undef ARR
static char io_buf[(NUMLEN << 1) + 10], tmp[20];
static inline void fft(double* a_r, double* a_i, int n) {
    static int rev[MAX], ln = 0;
    static double rt_r[MAX], rt_i[MAX];
    if (ln != n) {
        int L = 31 - __builtin_clz(n);
        rt_r[0] = rt_r[1] = 1.0;
        rt_i[0] = rt_i[1] = 0.0;
        for (int k = 2; k < n; k <<= 1) {
            register double x_r,x_i;
            __builtin_sincos(PI/k, &x_i, &x_r);
            for (int i = k; i < k + k; i++) {
                rt_r[i] = i & 1 ? rt_r[i >> 1] * x_r - rt_i[i >> 1] * x_i : rt_r[i >> 1];
                rt_i[i] = i & 1 ? rt_r[i >> 1] * x_i + rt_i[i >> 1] * x_r : rt_i[i >> 1];
            }       
        }       
        rev[0] = 0;
        for (int i = 0; i < n; i++) rev[i] = (rev[i >> 1] | (i & 1) << L) >> 1;
        ln = n;
    }
    for (int i = 0; i < n; i++)
        if (i < rev[i]) {
            double t = a_r[i];
            a_r[i] = a_r[rev[i]];
            a_r[rev[i]] = t;
            t = a_i[i];
            a_i[i] = a_i[rev[i]];
            a_i[rev[i]] = t;
        }
    for (int k = 1; k < n; k <<= 1) {
        for (int i = 0; i < n; i += (k << 1))
            for (int j = 0; j < k; j++) {
                double z_r = rt_r[j + k] * a_r[i + j + k] - rt_i[j + k] * a_i[i + j + k];
                double z_i = rt_r[j + k] * a_i[i + j + k] + rt_i[j + k] * a_r[i + j + k];
                a_r[i + j + k] = a_r[i + j] - z_r;
                a_i[i + j + k] = a_i[i + j] - z_i;
                a_r[i + j] += z_r;
                a_i[i + j] += z_i;
            }
    }
}
static inline void conv(ll* a, int sa, ll* b, int sb, ll* res) {
    int L = 32 - __builtin_clz(sa + sb - 1), n = 1 << L, i;
    for (i = 0; i < n; ++i) in_r[i] = in_i[i] = 0;
    for (i = 0; i < sa; ++i) in_r[i] = a[i];
    for (i = 0; i < sb; ++i) in_i[i] = b[i];
    fft(in_r, in_i, n);
    for (i = 0; i < n; ++i) {
        double r = in_r[i], im = in_i[i];
        in_r[i] = r * r - im * im;
        in_i[i] = 2 * r * im;
    }
    for (i = 0; i <= n >> 1; i++) {
        int j = (n - i) & (n - 1);
        double vi_r = in_r[i], vi_i = in_i[i], vj_r = in_r[j], vj_i = in_i[j];
        in_r[i] = vj_r - vi_r;
        in_i[i] = vj_i + vi_i;
        in_r[j] = vi_r - vj_r;
        in_i[j] = vi_i + vj_i;
    }
    fft(in_r, in_i, n);
    for (i = 0; i < sa + sb - 1; ++i) {
        double val = in_i[i] / (4 * n);
        res[i] = (ll)(val > 0 ? val + 0.5 : val - 0.5);
    }
}
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