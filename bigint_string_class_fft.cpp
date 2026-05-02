#include <bits/stdc++.h>
using namespace std;

#ifndef DIVIDE_METHOD
#define DIVIDE_METHOD true
#endif

class big_int {
   private:
    static const long long BASE = 1000000000LL;  // 1e9
    vector<long long> a;                         // little-endian
    bool neg = false;

    void trim() {
        while (!a.empty() && a.back() == 0) a.pop_back();
        if (a.empty()) neg = false;
    }

    static int absCmp(const big_int& x, const big_int& y) {
        if (x.a.size() != y.a.size())
            return x.a.size() < y.a.size() ? -1 : 1;
        for (int i = (int)x.a.size() - 1; i >= 0; --i) {
            if (x.a[i] != y.a[i])
                return x.a[i] < y.a[i] ? -1 : 1;
        }
        return 0;
    }

    static big_int absAdd(const big_int& x, const big_int& y) {
        big_int r;
        long long carry = 0;
        int n = max(x.a.size(), y.a.size());
        r.a.resize(n);
        for (int i = 0; i < n; i++) {
            long long cur = carry;
            if (i < (int)x.a.size()) cur += x.a[i];
            if (i < (int)y.a.size()) cur += y.a[i];
            r.a[i] = cur % BASE;
            carry = cur / BASE;
        }
        if (carry) r.a.push_back(carry);
        return r;
    }

    static big_int absSub(const big_int& x, const big_int& y) {  // |x|>=|y|
        big_int r;
        r.a.resize(x.a.size());
        long long carry = 0;
        for (int i = 0; i < (int)x.a.size(); i++) {
            long long cur = x.a[i] - carry - (i < (int)y.a.size() ? y.a[i] : 0);
            if (cur < 0)
                cur += BASE, carry = 1;
            else
                carry = 0;
            r.a[i] = cur;
        }
        r.trim();
        return r;
    }

   public:
    big_int(long long v = 0) {
        if (v < 0) neg = true, v = -v;
        while (v) {
            a.push_back(v % BASE);
            v /= BASE;
        }
    }

    big_int(const string& s) {
        string t = s;
        if (!t.empty() && t[0] == '-') {
            neg = true;
            t = t.substr(1);
        }
        for (int i = (int)t.size(); i > 0; i -= 9) {
            int l = max(0, i - 9);
            a.push_back(stoll(t.substr(l, i - l)));
        }
        trim();
    }

    string str() const {
        if (a.empty()) return "0";
        string s = neg ? "-" : "";
        s += to_string(a.back());
        char buf[16];
        for (int i = (int)a.size() - 2; i >= 0; --i) {
            sprintf(buf, "%09lld", a[i]);
            s += buf;
        }
        return s;
    }

    friend ostream& operator<<(ostream& os, const big_int& x) {
        return os << x.str();
    }

    friend istream& operator>>(istream& is, big_int& x) {
        string s;
        is >> s;
        x = big_int(s);
        return is;
    }

    bool operator<(const big_int& o) const {
        if (neg != o.neg) return neg;
        int c = absCmp(*this, o);
        return neg ? c > 0 : c < 0;
    }
    bool operator>(const big_int& o) const { return o < *this; }
    bool operator<=(const big_int& o) const { return !(o < *this); }
    bool operator>=(const big_int& o) const { return !(*this < o); }
    bool operator==(const big_int& o) const { return neg == o.neg && a == o.a; }
    bool operator!=(const big_int& o) const { return !(*this == o); }

    big_int operator-() const {
        big_int r = *this;
        if (!r.a.empty()) r.neg = !r.neg;
        return r;
    }

    big_int operator+(const big_int& o) const {
        big_int r;
        if (neg == o.neg) {
            r = absAdd(*this, o);
            r.neg = neg;
        } else {
            int c = absCmp(*this, o);
            if (c >= 0)
                r = absSub(*this, o), r.neg = neg;
            else
                r = absSub(o, *this), r.neg = o.neg;
        }
        r.trim();
        return r;
    }

    big_int operator-(const big_int& o) const {
        return *this + (-o);
    }

    big_int operator*(const big_int& o) const {
        big_int r;
        r.neg = neg ^ o.neg;
        r.a.assign(a.size() + o.a.size(), 0);

        for (int i = 0; i < (int)a.size(); i++) {
            long long carry = 0;
            for (int j = 0; j < (int)o.a.size() || carry; j++) {
                __int128 cur = r.a[i + j] + (__int128)a[i] * (j < (int)o.a.size() ? o.a[j] : 0) + carry;
                r.a[i + j] = (long long)(cur % BASE);
                carry = (long long)(cur / BASE);
            }
        }
        r.trim();
        return r;
    }

    big_int operator+=(const big_int& o) { return *this = *this + o; }
    big_int operator-=(const big_int& o) { return *this = *this - o; }
    big_int operator*=(const big_int& o) { return *this = *this * o; }
};

int main() {
    big_int a, b;
    cin >> a >> b;
    cout << a + b << '\n';
    cout << a - b << '\n';
    cout << a * b << '\n';
}