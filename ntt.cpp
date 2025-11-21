#include <bits/stdc++.h>
using namespace std;
using ll = long long;
const ll M = 998244353, G = 3;

ll fp(ll x, ll y) {
    ll r = 1;
    while (y) {
        if (y & 1) r = r * x % M;
        x = x * x % M, y >>= 1;
    }
    return r;
}
void ntt(vector<ll>& a, bool inv = 0) {
    int n = a.size();
    for (int i = 1, j = 0; i < n; i++) {
        int b = n >> 1;
        for (; j & b; b >>= 1) j ^= b;
        j ^= b;
        if (i < j) swap(a[i], a[j]);
    }
    for (int s = 2; s <= n; s <<= 1) {
        ll wn = fp(G, (M - 1) / s);
        if (inv) wn = fp(wn, M - 2);
        for (int i = 0; i < n; i += s) {
            ll w = 1;
            for (int j = 0; j < s / 2; j++) {
                ll u = a[i + j], v = a[i + j + s/2] * w % M;
                a[i + j] = (u + v) % M;
                a[i + j + s/2] = (u - v + M) % M;
                w = w * wn % M;
            }
        }
    }
    if (inv) {
        ll inv_n = fp(n, M - 2);
        for (ll& x : a) x = x * inv_n % M;
    }
}

string multy(string A,string B){
    int n = 1, a = A.size(), b = B.size();
    while (n < a + b) n <<= 1;
    vector<ll> x(n), y(n);
    for (int i = 0; i < a; i++) x[i] = A[a - 1 - i] - '0';
    for (int i = 0; i < b; i++) y[i] = B[b - 1 - i] - '0';
    ntt(x), ntt(y);
    for (int i = 0; i < n; i++) x[i] = x[i] * y[i] % M;
    ntt(x, 1);

    vector<ll> res(n);
    for (int i = 0; i < n; i++) {
        res[i] += x[i];
        if (res[i] >= 10) res[i + 1] += res[i] / 10, res[i] %= 10;
    }
    while (res.back() == 0 && res.size() > 1) res.pop_back();
    string r;
    for (int i = res.size() - 1; i >= 0; i--) r+=res[i];
    return r;
}

int main() {
    string A, B; cin >> A >> B;
    cout<<multy(A,B);
}