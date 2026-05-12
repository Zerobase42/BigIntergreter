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
static inline void c_addeq(_complex* a, _complex b) {
    a->real += b.real;
    a->imag += b.imag;
}
static inline _complex c_conj(_complex a) { return make_complex(a.real, -a.imag); }
static inline _complex c_polar(double th) { return make_complex(__builtin_cos(th), __builtin_sin(th)); }
#define cd _complex
static cd in[MAX];
static ll A[MAX >> 1], B[MAX >> 1], C[MAX >> 1];
static char io_buf[(NUMLEN << 1) + 10], tmp[20];
static inline void fft(cd* a, int L) {
    static int rev[MAX], ln = 0;
    static cd rt[MAX];
    int n = 1 << L;
    if (ln != n) {
        ln = n;
        for (int i = 0; i < n; i++) {
            rt[i] = c_polar(-2.0 * PI * i / n);
            rev[i] = (rev[i >> 2] | ((i & 3) << L)) >> 2;
        }
    }
    for (int i = 0; i < n; i++)
        if (i < rev[i]) {
            cd t = a[i];
            a[i] = a[rev[i]];
            a[rev[i]] = t;
        }
    // i(a+bi) = -b+ai

    for (int k = 1, step = n >> 2; k < n; k <<= 2, step >>= 2) {
        for (int i = 0; i < n; i += k << 2) {
            int l = k << 1;
            int m = l | k;
            for (int j = 0; j < k; j++) {
                int sj = step * j;
                int sj2 = sj << 1;
                int sj3 = sj + sj2;

                cd a0 = a[i | j];
                cd a1 = c_mul(a[i | j | k], rt[sj]);
                cd a2 = c_mul(a[i | j | l], rt[sj2]);
                cd a3 = c_mul(a[i | j | m], rt[sj3]);

                cd add02 = c_add(a0, a2);
                cd sub02 = c_sub(a0, a2);
                cd add13 = c_add(a1, a3);
                cd sub13i = make_complex(a3.imag - a1.imag, a1.real - a3.real);

                a[i | j] = c_add(add02, add13);
                a[i | j | k] = c_sub(sub02, sub13i);
                a[i | j | l] = c_sub(add02, add13);
                a[i | j | m] = c_add(sub02, sub13i);
            }
        }
    }
}
static inline void conv(ll* a, int sa, ll* b, int sb, ll* res) {
    int L = (sa + sb) <= 2 ? 0 : 32 - __builtin_clz(sa + sb - 2);
    L += (L & 1);
    int n = 1 << L, i;

    memset(in, 0, sizeof(in[0]) * n);
    for (i = 0; i < sa; ++i) in[i].real = a[i];
    for (i = 0; i < sb; ++i) in[i].imag = b[i];

    fft(in, L);

    for (i = 0; i < n; ++i) {
        double r = in[i].real, im = in[i].imag;
        in[i].real = r * r - im * im;
        in[i].imag = 2 * r * im;
    }

    fft(in, L);

    res[0] = (ll)(in[0].imag + 0.5) >> (L + 1);
    for (i = 1; i < sa + sb - 1; ++i) res[i] = (ll)(in[n - i].imag + 0.5) >> (L + 1);
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