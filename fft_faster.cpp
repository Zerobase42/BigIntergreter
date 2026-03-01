#include <bits/stdc++.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
using namespace std;

constexpr int BASE = 100000, DIG = 5;

void fft(vector<complex<double>>& a) {
    int n = (int)a.size(), L = 31 - __builtin_clz(n);
    static vector<complex<long double>> R(2, 1);
    static vector<complex<double>> rt(2, 1);  //(^10% faster if double)
    for (static int k = 2; k < n; k *= 2) {
        R.resize(n);
        rt.resize(n);
        auto x = polar(1.0L, acos(-1.0L) / k);
        for (int i = k; i < k + k; i++) rt[i] = R[i] = i & 1 ? R[i / 2] * x : R[i / 2];
    }
    vector<int> rev(n);
    for (int i = 0; i < n; i++) rev[i] = (rev[i / 2] | (i & 1) << L) / 2;
    for (int i = 0; i < n; i++)
        if (i < rev[i]) swap(a[i], a[rev[i]]);
    for (int k = 1; k < n; k *= 2) {
        for (int i = 0; i < n; i += 2 * k)
            for (int j = 0; j < k; j++) {
                // complex<double>z=rt[j+k]*a[i+j+k];//(25% faster if hand-rolled)    ///include-line
                auto x = (double*)&rt[j + k], y = (double*)&a[i + j + k];                 /// exclude-line
                complex<double> z(x[0] * y[0] - x[1] * y[1], x[0] * y[1] + x[1] * y[0]);  /// exclude-line
                a[i + j + k] = a[i + j] - z;
                a[i + j] += z;
            }
    }
}
template <typename T>
vector<T> conv(const vector<T>& a, const vector<T>& b) {
    if (a.empty() || b.empty()) return {};
    vector<T> res((int)a.size() + (int)b.size() - 1);
    int L = 32 - __builtin_clz((int)res.size()), n = 1 << L;
    vector<complex<double>> in(n), out(n);
    copy(a.begin(), a.end(), begin(in));
    for (int i = 0; i < (int)b.size(); i++) in[i].imag(b[i]);
    fft(in);
    for (complex<double>& x : in) x *= x;
    for (int i = 0; i < n; i++) out[i] = in[-i & (n - 1)] - conj(in[i]);
    fft(out);
    for (int i = 0; i < (int)res.size(); i++) {
        double val = imag(out[i]) / (4 * n);
        res[i] = (long long)(val > 0 ? val + 0.5 : val - 0.5);
    }
    return res;
}
int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    string a, b;
    cin >> a >> b;

    vector<long long> A, B;
    A.reserve(a.size() / DIG + 1);
    B.reserve(b.size() / DIG + 1);
    for (int i = a.size(); i >= 0; i -= DIG) {
        int l = max(0, i - DIG), r = 0;
        for (int j = l; j < i; ++j) r = r * 10 + (a[j] - '0');
        A.push_back(r);
    }

    for (int i = b.size(); i >= 0; i -= DIG) {
        int l = max(0, i - DIG), r = 0;
        for (int j = l; j < i; ++j) r = r * 10 + (b[j] - '0');
        B.push_back(r);
    }

    vector<long long> C = conv(A, B);

    long long carry = 0;
    for (size_t i = 0; i < C.size(); ++i) {
        long long x = C[i] + carry;
        C[i] = x % BASE;
        carry = x / BASE;
    }
    while (carry) {
        C.push_back(carry % BASE);
        carry /= BASE;
    }
    while (C.size() > 1 && C.back() == 0)
        C.pop_back();

    for (long long i : C)
        cout << setw(MAXLEN) << setfill('0') << i;
}