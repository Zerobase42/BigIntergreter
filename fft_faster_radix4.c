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
const long double PI = 3.14159265358979323846;
/*
typedef struct {
    double real, imag;
} _complex;
static inline _complex make_complex(double real, double imag) {
    _complex z;
    z.real = real;
    z.imag = imag;
    return z;
}
static inline _complex c_add(_complex a, _complex b) { return make_complex(a.real + b.real, a.imag + b.imag); }
static inline _complex c_sub(_complex a, _complex b) { return make_complex(a.real - b.real, a.imag - b.imag); }
static inline _complex c_mul(_complex a, _complex b) { return make_complex(a.real * b.real - a.imag * b.imag, a.real * b.imag + a.imag * b.real); }
static inline _complex c_mul_conj(_complex a, _complex b) { return make_complex(a.real * b.real + a.imag * b.imag, a.imag * b.real - a.real * b.imag); }

static inline void c_addeq(_complex* a, _complex b) {
    a->real += b.real;
    a->imag += b.imag;
}
static inline _complex c_conj(_complex a) { return make_complex(a.real, -a.imag); }
static inline _complex c_polar(double th) { return make_complex(__builtin_cos(th), __builtin_sin(th)); }
#define cd _complex
*/
static double in_r[MAX], in_i[MAX];
static ll A[MAX >> 1], B[MAX >> 1], C[MAX >> 1];
static char io_buf[(NUMLEN << 1) + 10], tmp[20];

int ln = 0;
static double rt_r[MAX], rt_i[MAX];

static inline void fft_dif(double* a_r, double* a_i, int L) {
    int n = 1 << L;
    if (ln != n) {
        ln = n;
        for (int i = 0; i < n; i++) {
            double th = -2.0 * PI * i / n;
            rt_r[i] = __builtin_cos(th);
            rt_i[i] = __builtin_sin(th);
        }
    }

    for (int k = n >> 2, step = 1; k >= 1; k >>= 2, step <<= 2) {
        int l = k << 1;
        int m = l | k;
        for (int i = 0; i < n; i += k << 2) {
            for (int j = 0; j < k; j++) {
                int sj = step * j;
                int sj2 = sj << 1;
                int sj3 = sj + sj2;

                double a0_r = a_r[i | j], a0_i = a_i[i | j];
                double a1_r = a_r[i | j | k], a1_i = a_i[i | j | k];
                double a2_r = a_r[i | j | l], a2_i = a_i[i | j | l];
                double a3_r = a_r[i | j | m], a3_i = a_i[i | j | m];

                double add02_r = a0_r + a2_r, add02_i = a0_i + a2_i;
                double sub02_r = a0_r - a2_r, sub02_i = a0_i - a2_i;
                double add13_r = a1_r + a3_r, add13_i = a1_i + a3_i;
                double sub13i_r = a3_i - a1_i, sub13i_i = a1_r - a3_r;

                double r0_r = add02_r + add13_r, r0_i = add02_i + add13_i;
                double r1_r = sub02_r - sub13i_r, r1_i = sub02_i - sub13i_i;
                double r2_r = add02_r - add13_r, r2_i = add02_i - add13_i;
                double r3_r = sub02_r + sub13i_r, r3_i = sub02_i + sub13i_i;

                a_r[i | j] = r0_r;
                a_i[i | j] = r0_i;
                a_r[i | j | k] = r1_r * rt_r[sj] - r1_i * rt_i[sj];
                a_i[i | j | k] = r1_r * rt_i[sj] + r1_i * rt_r[sj];
                a_r[i | j | l] = r2_r * rt_r[sj2] - r2_i * rt_i[sj2];
                a_i[i | j | l] = r2_r * rt_i[sj2] + r2_i * rt_r[sj2];
                a_r[i | j | m] = r3_r * rt_r[sj3] - r3_i * rt_i[sj3];
                a_i[i | j | m] = r3_r * rt_i[sj3] + r3_i * rt_r[sj3];
            }
        }
    }
}

static inline void ifft_dit(double* a_r, double* a_i, int L) {
    int n = 1 << L;
    if (ln != n) {
        ln = n;
        for (int i = 0; i < n; i++) {
            double th = 2.0 * PI * i / n;
            rt_r[i] = __builtin_cos(th);
            rt_i[i] = __builtin_sin(th);
        }
    }

    for (int k = 1, step = n >> 2; k < n; k <<= 2, step >>= 2) {
        for (int i = 0; i < n; i += k << 2) {
            int l = k << 1;
            int m = l | k;
            for (int j = 0; j < k; j++) {
                int sj = step * j;
                int sj2 = sj << 1;
                int sj3 = sj + sj2;

                double a0_r = a_r[i | j], a0_i = a_i[i | j];
                double a1_r = a_r[i | j | k] * rt_r[sj] + a_i[i | j | k] * rt_i[sj];
                double a1_i = a_i[i | j | k] * rt_r[sj] - a_r[i | j | k] * rt_i[sj];
                double a2_r = a_r[i | j | l] * rt_r[sj2] + a_i[i | j | l] * rt_i[sj2];
                double a2_i = a_i[i | j | l] * rt_r[sj2] - a_r[i | j | l] * rt_i[sj2];
                double a3_r = a_r[i | j | m] * rt_r[sj3] + a_i[i | j | m] * rt_i[sj3];
                double a3_i = a_i[i | j | m] * rt_r[sj3] - a_r[i | j | m] * rt_i[sj3];

                double add02_r = a0_r + a2_r, add02_i = a0_i + a2_i;
                double sub02_r = a0_r - a2_r, sub02_i = a0_i - a2_i;
                double add13_r = a1_r + a3_r, add13_i = a1_i + a3_i;
                double sub13i_r = a1_i - a3_i, sub13i_i = a3_r - a1_r;

                a_r[i | j] = add02_r + add13_r;
                a_i[i | j] = add02_i + add13_i;
                a_r[i | j | k] = sub02_r - sub13i_r;
                a_i[i | j | k] = sub02_i - sub13i_i;
                a_r[i | j | l] = add02_r - add13_r;
                a_i[i | j | l] = add02_i - add13_i;
                a_r[i | j | m] = sub02_r + sub13i_r;
                a_i[i | j | m] = sub02_i + sub13i_i;
            }
        }
    }
}

static inline void conv(ll* a, int sa, ll* b, int sb, ll* res) {
    int L = (sa + sb) <= 2 ? 0 : 32 - __builtin_clz(sa + sb - 2);
    L += (L & 1);
    int n = 1 << L, i;
    for (i = 0; i < n; i++) in_r[i] = i < sa ? a[i] : 0, in_i[i] = i < sb ? b[i] : 0;

    fft_dif(in_r, in_i, L);
    for (i = 0; i < n; ++i) {
        double r = in_r[i], im = in_i[i];
        in_r[i] = (r + im) * (r - im);
        in_i[i] = 2 * r * im;
    }

    ifft_dit(in_r, in_i, L);
    double invn = 0.5 / n;
    for (i = 0; i < sa + sb - 1; ++i) res[i] = (ll)(in_i[i] * invn + 0.5);
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