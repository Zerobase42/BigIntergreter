#include <bits/stdc++.h>
using namespace std;
using i128 = __int128_t;
constexpr int DIG = 8;
constexpr int pow10(int n) {
    int r = 1;
    while (n--) r *= 10;
    return r;
}
constexpr int BASE = pow10(DIG);
using ll = long long;
template <int MOD, int G = 3>
struct NTT {
    static int modpow(long long a, long long e) {
        long long r = 1;
        while (e) {
            if (e & 1) r = r * a % MOD;
            a = a * a % MOD;
            e >>= 1;
        }
        return r;
    }
    static void ntt(vector<int>& a, bool inv) {
        int n = a.size();
        static vector<int> rev;
        static vector<int> roots{ 0, 1 };
        if ((int)rev.size() != n) {
            rev.assign(n, 0);
            int lg = __builtin_ctz(n);
            for (int i = 0; i < n; i++)
                rev[i] = (rev[i >> 1] >> 1) | ((i & 1) << (lg - 1));
        }
        if ((int)roots.size() < n) {
            int k = __builtin_ctz(roots.size());
            roots.resize(n);
            while ((1 << k) < n) {
                int e = modpow(G, (MOD - 1) >> (k + 1));
                for (int i = 1 << (k - 1); i < (1 << k); i++) {
                    roots[i << 1] = roots[i];
                    roots[i << 1 | 1] = (ll)roots[i] * e % MOD;
                }
                k++;
            }
        }
        for (int i = 0; i < n; i++)
            if (i < rev[i]) swap(a[i], a[rev[i]]);
        for (int len = 1; len < n; len <<= 1) {
            for (int i = 0; i < n; i += len << 1) {
                for (int j = 0; j < len; j++) {
                    int u = a[i + j];
                    int v = (ll)a[i + j + len] * roots[len + j] % MOD;
                    a[i + j] = u + v < MOD ? u + v : u + v - MOD;
                    a[i + j + len] = u - v >= 0 ? u - v : u - v + MOD;
                }
            }
        }
        if (inv) {
            reverse(a.begin() + 1, a.end());
            int invn = modpow(n, MOD - 2);
            for (int& x : a) x = (ll)x * invn % MOD;
        }
    }
    static vector<int> multiply(vector<int> a, vector<int> b) {
        int need = a.size() + b.size() - 1;
        int n = 1;
        while (n < need) n <<= 1;
        a.resize(n);
        b.resize(n);
        ntt(a, false);
        ntt(b, false);
        for (int i = 0; i < n; i++)
            a[i] = (ll)a[i] * b[i] % MOD;
        ntt(a, true);
        a.resize(need);
        return a;
    }
};
using NTT1 = NTT<998244353>;
using NTT2 = NTT<1004535809>;
using NTT3 = NTT<469762049>;
constexpr ll M1 = 998244353;
constexpr ll M2 = 1004535809;
constexpr ll M3 = 469762049;
constexpr ll M1_INV_M2 = 669690699;
constexpr ll M12_INV_M3 = 354521948;
vector<i128> convolutionCRT(const vector<int>& A,
                            const vector<int>& B) {
    auto c1 = NTT1::multiply(A, B);
    auto c2 = NTT2::multiply(A, B);
    auto c3 = NTT3::multiply(A, B);
    int n = c1.size();
    vector<i128> ans(n);
    const i128 M12 = (i128)M1 * M2;
    for (int i = 0; i < n; i++) {
        ll x1 = c1[i];
        ll t = ((c2[i] - x1) % M2 + M2) % M2;
        t = (i128)t * M1_INV_M2 % M2;
        i128 x12 = (i128)x1 + (i128)t * M1;
        ll r = (ll)(x12 % M3);
        ll u = ((c3[i] - r) % M3 + M3) % M3;
        u = (i128)u * M12_INV_M3 % M3;
        ans[i] = x12 + (i128)u * M12;
    }
    return ans;
}
int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    string sa, sb;
    cin >> sa >> sb;
    if (sa == "0" || sb == "0") {
        cout << 0 << '\n';
        return 0;
    }
    vector<int> A, B;
    for (int i = (int)sa.size(); i > 0; i -= DIG) {
        int x = 0;
        int l = max(0, i - DIG);
        for (int j = l; j < i; j++)
            x = x * 10 + (sa[j] - '0');
        A.push_back(x);
    }
    for (int i = (int)sb.size(); i > 0; i -= DIG) {
        int x = 0;
        int l = max(0, i - DIG);
        for (int j = l; j < i; j++)
            x = x * 10 + (sb[j] - '0');
        B.push_back(x);
    }
    vector<i128> C = convolutionCRT(A, B);
    C.push_back(0);
    for (size_t i = 0; i + 1 < C.size(); i++) {
        C[i + 1] += C[i] / BASE;
        C[i] %= BASE;
    }
    while (C.size() > 1 && C.back() == 0)
        C.pop_back();
    auto print128 = [](i128 x) {
        if (x == 0) {
            cout << '0';
            return;
        }
        string s;
        while (x) {
            s.push_back(char('0' + x % 10));
            x /= 10;
        }
        reverse(s.begin(), s.end());
        cout << s;
    };
    print128(C.back());
    for (int i = (int)C.size() - 2; i >= 0; i--) {
        i128 x = C[i];
        char buf[DIG];
        for (int j = DIG - 1; j >= 0; j--) {
            buf[j] = char('0' + x % 10);
            x /= 10;
        }
        cout.write(buf, DIG);
    }
    cout << '\n';
    return 0;
}