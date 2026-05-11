#ifdef __linux__
#include <unistd.h>
#else
#include <stdio.h>
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
#define MAX 1048576
const long double PI = 3.14159265358979323846;

static double in_r[MAX], in_i[MAX];
static ll A[MAX >> 1], B[MAX >> 1], C[MAX >> 1];
static char io_buf[(NUMLEN << 1) + 10], tmp[20];
static inline void fft(double* a_r, double* a_i, int n) {
    static int rev[MAX], ln = 0;
    static double rt_r[MAX], rt_i[MAX];
    if (ln != n) {
        int L = 31 - __builtin_clz(n);
        double th, x_r, x_i;
        rt_r[0] = rt_r[1] = 1.0;
        rt_i[0] = rt_i[1] = 0.0;
        for (int k = 2; k < n; k <<= 1) {
            th = PI / k, x_r = __builtin_cos(th), x_i = __builtin_sin(th);
            for (int i = k; i < k + k; i++) {
                rt_r[i] = i & 1 ? rt_r[i >> 1] * x_r - rt_i[i >> 1] * x_i : rt_r[i >> 1];
                rt_i[i] = i & 1 ? rt_r[i >> 1] * x_i + rt_i[i >> 1] * x_r : rt_i[i >> 1];
            }
        }
        rev[0] = 0;
        for (int i = 1; i < n; i++) rev[i] = (rev[i >> 2] | ((i & 3) << L)) >> 2;
        ln = n;
    }
    for (int i = 1; i < n - 1; i++)
        if (i < rev[i]) {
            double t = a_r[i];
            a_r[i] = a_r[rev[i]];
            a_r[rev[i]] = t;
            t = a_i[i];
            a_i[i] = a_i[rev[i]];
            a_i[rev[i]] = t;
        }
    for (int k = 1; k < n; k <<= 2) {
        int step = n >> (2 + __builtin_ctz(k));
        for (int i = 0; i < n; i += (k << 2))
            for (int j = 0; j < k; j++) {
                // a.real*b.real-a.imag*b.imag
                // a.real*b.imag+a.imag*b.real
                double a0_r = a_r[i + j], a0_i = a_i[i + j],
                       a1_r = a_r[i + j + k] * rt_r[step * j] - a_i[i + j + k] * rt_i[step * j],
                       a1_i = a_i[i + j + k] * rt_r[step * j] + a_r[i + j + k] * rt_i[step * j],
                       a2_r = a_r[i + j + (k << 1)] * rt_r[step * (j << 1)] - a_i[i + j + (k << 1)] * rt_i[step * (j << 1)],
                       a2_i = a_i[i + j + (k << 1)] * rt_r[step * (j << 1)] + a_r[i + j + (k << 1)] * rt_i[step * (j << 1)],
                       a3_r = a_r[i + j + (k << 1) + k] * rt_r[step * (j * 3)] - a_i[i + j + (k << 1) + k] * rt_i[step * (j * 3)],
                       a3_i = a_i[i + j + (k << 1) + k] * rt_r[step * (j * 3)] + a_r[i + j + (k << 1) + k] * rt_i[step * (j * 3)];

                a_r[i + j + k] = a0_r - z_r;
                a_i[i + j + k] = a0_i - z_i;
                a_r[i + j] = a0_r + z_r;
                a_i[i + j] = a0_i + z_i;
            }
    }
}
static inline void conv(ll* a, int sa, ll* b, int sb, ll* res) {
    int L = 32 - __builtin_clz(sa + sb - 1), n = (L & 1 ? 2 : 1) << L, i;
    for (i = 0; i < sa; ++i) in_r[i] = a[i];
    for (i = 0; i < sb; ++i) in_i[i] = b[i];
    fft(in_r, in_i, n);
    for (i = 0; i < n; ++i) {
        double r = in_r[i], im = in_i[i];
        in_r[i] = r * r - im * im;
        in_i[i] = 2 * r * im;
    }
    for (i = 0; i <= n >> 1; ++i) {
        int j = (n - i) & (n - 1);
        double v_i_r = in_r[i], v_i_i = in_i[i],
               v_j_r = in_r[j], v_j_i = in_i[j];
        in_r[i] = v_j_r - v_i_r;
        in_i[i] = v_j_i + v_i_i;
        in_r[j] = v_i_r - v_j_r;
        in_i[j] = v_i_i + v_j_i;
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