#include <unistd.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
#define ll long long
#define max(a, b) ((a) > (b) ? (a) : (b))
#define DIG 5
#define NUMLEN 1000000
#define BASE 100000
#define MAX 1048576
const long double PI = 3.14159265358979323846;

#define strlen __builtin_strlen
/*
static size_t strlen(char* zb) {
    size_t r = 0;
    while (*zb != '\n' && *zb != 0) {
        zb++;
        r++;
    }
    return r;
}
*/

typedef struct {
    double real, imag;
} _complex;
static inline _complex make_complex(double real, double imag) {
    _complex z;
    z.real = real;
    z.imag = imag;
    return z;
}
static inline _complex c_add(_complex a, _complex b) {
    return make_complex(a.real + b.real, a.imag + b.imag);
}
static inline _complex c_sub(_complex a, _complex b) {
    return make_complex(a.real - b.real, a.imag - b.imag);
}
static inline _complex c_mul(_complex a, _complex b) {
    return make_complex(
        a.real * b.real - a.imag * b.imag,
        a.real * b.imag + a.imag * b.real);
}
static inline void c_addeq(_complex* a, _complex b) {
    a->real += b.real;
    a->imag += b.imag;
}
static inline void c_subeq(_complex* a, _complex b) {
    a->real -= b.real;
    a->imag -= b.imag;
}
static inline void c_muleq(_complex* a, _complex b) {
    double nr = a->real * b.real - a->imag * b.imag;
    a->imag = a->real * b.imag + a->imag * b.real;
    a->real = nr;
}
static inline _complex c_conj(_complex a) {
    return make_complex(a.real, -a.imag);
}
static inline _complex c_polar(double th) {
    return make_complex(__builtin_cos(th), __builtin_sin(th));
}
#define cd _complex
static inline void fft(cd* a, int n) {
    int L = 31 - __builtin_clz(n);
    static int rev[MAX], ln = 0;
    static cd rt[MAX];
    if (ln != n) {
        rt[0] = rt[1] = make_complex(1.0, 0.0);
        for (int k = 2; k < n; k <<= 1) {
            cd x = c_polar(PI / k);
            for (int i = k; i < k + k; i++) rt[i] = i & 1 ? c_mul(rt[i / 2], x) : rt[i / 2];
        }
        rev[0] = 0;
        for (int i = 0; i < n; i++) rev[i] = (rev[i / 2] | (i & 1) << L) / 2;
        ln = n;
    }
    for (int i = 0; i < n; i++)
        if (i < rev[i]) {
            cd t = a[i];
            a[i] = a[rev[i]];
            a[rev[i]] = t;
        }
    for (int k = 1; k < n; k <<= 1) {
        for (int i = 0; i < n; i += (k << 1))
            for (int j = 0; j < k; j++) {
                cd z = c_mul(rt[j + k], a[i + j + k]);
                a[i + j + k] = c_sub(a[i + j], z);
                c_addeq(&a[i + j], z);
            }
    }
}
static inline void conv(ll* a, int sa, ll* b, int sb, ll* res) {
    int L = 32 - __builtin_clz(sa + sb - 1), n = 1 << L, i;
    cd in[n], out[n];
    for (i = 0; i < sa; ++i) in[i].real = a[i];
    for (i = 0; i < sb; ++i) in[i].imag = b[i];
    fft(in, n);
    for (int i = 0; i < n; ++i) {
        double r = in[i].real, im = in[i].imag;
        in[i].real = r * r - im * im;
        in[i].imag = 2 * r * im;
    }
    for (i = 0; i < n; i++) out[i] = c_sub(in[-i & (n - 1)], c_conj(in[i]));
    fft(out, n);
    for (i = 0; i < sa + sb - 1; ++i) {
        double val = out[i].imag / (4 * n);
        res[i] = (ll)(val > 0 ? val + 0.5 : val - 0.5);
    }
}
int main() {
    char buf[(NUMLEN << 1) + 4];
    int len = syscall(0, 0, buf, (NUMLEN << 1) + 3), mid = 0, idx = 0, p, i, j, l;
    buf[len] = 0;
    while (buf[mid] > ' ') mid++;
    for (p = mid; buf[p] && buf[p] <= ' '; p++);
    for (len = p; buf[len] > ' '; len++);
    int la = mid, lb = len - p;

    int na = (la + DIG - 1) / DIG, nb = (lb + DIG - 1) / DIG;
    ll A[na], B[nb];
    for (i = la; i > 0; i -= DIG) {
        l = max(0, i - DIG);
        ll r = 0;
        for (j = l; j < i; j++) r = r * 10 + (buf[j] - '0');
        A[idx++] = r;
    }
    idx = 0;
    for (i = len; i > mid + 1; i -= DIG) {
        l = max(mid + 1, i - DIG);
        ll r = 0;
        for (j = l; j < i; j++) r = r * 10 + (buf[j] - '0');
        B[idx++] = r;
    }
    int nc = na + nb - 1;
    ll C[nc], carry = 0;
    conv(A, na, B, nb, C);
    for (i = 0; i < nc; ++i) {
        ll x = C[i] + carry;
        C[i] = x % BASE, carry = x / BASE;
    }
    while (carry) C[nc++] = carry % BASE, carry /= BASE;
    while (nc > 1 && C[nc - 1] == 0) nc--;
    char wbuf[nc * DIG], tmp[20];
    int pos = 0, t = 0;
    ll x = C[nc - 1];
    while (x) tmp[t++] = x % 10 + '0', x /= 10;
    if (t == 0) tmp[t++] = '0';
    while (t--) wbuf[pos++] = tmp[t];
    for (i = nc - 2; i >= 0; --i) {
        x = C[i];
        for (j = 0; j < DIG; j++) tmp[j] = x % 10 + '0', x /= 10;
        for (j = DIG - 1; j >= 0; j--) wbuf[pos++] = tmp[j];
    }
    syscall(1, 1, wbuf, pos);
}