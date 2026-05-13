#include <bits/stdc++.h>
#define all(v) v.begin(), v.end()
using namespace std;

typedef long long ll;
typedef vector<ll> poly;

const ll w = 3;
const ll mod = 998244353;

ll pw(ll a, ll b) {
    ll ret = 1;
    while (b) {
        if (b & 1) ret = ret * a % mod;
        b >>= 1;
        a = a * a % mod;
    }
    return ret;
}

void ntt(poly& f, bool inv = 0) {
    int n = f.size(), j = 0;
    vector<ll> root(n >> 1);
    for (int i = 1; i < n; i++) {
        int bit = (n >> 1);
        while (j >= bit) {
            j -= bit;
            bit >>= 1;
        }
        j += bit;
        if (i < j) swap(f[i], f[j]);
    }
    ll ang = pw(w, (mod - 1) / n);
    if (inv) ang = pw(ang, mod - 2);
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
    ll t = pw(n, mod - 2);
    if (inv)
        for (int i = 0; i < n; i++) f[i] = f[i] * t % mod;
}

vector<ll> multiply(poly& _a, poly& _b) {
    vector<ll> a(all(_a)), b(all(_b));
    int n = 2;
    while (n < a.size() + b.size()) n <<= 1;
    a.resize(n);
    b.resize(n);
    ntt(a);
    ntt(b);
    for (int i = 0; i < n; i++) a[i] = a[i] * b[i] % mod;
    ntt(a, 1);
    return a;
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    const int DIG = 4;
    const int BASE = 10000;
    string a, b;
    cin >> a >> b;
    vector<ll> x, y;
    for (int i = (int)a.size(); i > 0; i -= DIG) {
        int l = max(0, i - DIG);
        x.push_back(stoll(a.substr(l, i - l)));
    }
    for (int i = (int)b.size(); i > 0; i -= DIG) {
        int l = max(0, i - DIG);
        y.push_back(stoll(b.substr(l, i - l)));
    }
    vector<ll> v = multiply(x, y);
    v.push_back(0);

    for (int i = 0; i + 1 < (int)v.size(); i++) {
        if (v[i] < 0) {
            ll borrow = (-v[i] + BASE - 1) / BASE;
            v[i + 1] -= borrow;
            v[i] += borrow * BASE;
        } else {
            v[i + 1] += v[i] / BASE;
            v[i] %= BASE;
        }
    }

    while (v.size() > 1 && v.back() == 0)v.pop_back();

    cout << v.back();

    char buf[20];

    for (int i = (int)v.size() - 2; i >= 0; i--) {
        snprintf(buf, sizeof(buf), "%0*lld", DIG, v[i]);
        cout << buf;
    }
}