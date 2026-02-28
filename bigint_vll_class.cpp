#include <bits/stdc++.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
#define ll long long
#define vll vector<ll>
constexpr int MAXLEN=16;
constexpr ll pow10(int n) {
    return n == 0 ? 1LL : 10LL * pow10(n - 1);
}
constexpr ll MAXCARRY=pow10(MAXLEN);
using namespace std;
class big_int {
   private:
    vll number;  // LSB-first
    bool neg;
    static void trim(vll& a) {
        while (a.size() > 1 && a.back() == 0)
            a.pop_back();
    }
    static int cmpAbs(const vll& a, const vll& b) {
        if (a.size() != b.size())
            return a.size() < b.size() ? -1 : 1;

        for (int i = (int)a.size() - 1; i >= 0; --i) {
            if (a[i] != b[i])
                return a[i] < b[i] ? -1 : 1;
        }
        return 0;
    }
    static vll addAbs(const vll& a, const vll& b) {
        vll res;
        res.reserve(max(a.size(), b.size()) + 1);

        ll carry = 0;
        for (size_t i = 0; i < max(a.size(), b.size()) || carry; ++i) {
            ll x = (i < a.size() ? a[i] : 0);
            ll y = (i < b.size() ? b[i] : 0);

            ll s = x + y + carry;
            res.push_back(s % MAXCARRY);
            carry = s / MAXCARRY;
        }

        return res;
    }
    static vll subAbs(const vll& a, const vll& b) {
        vll res;
        res.reserve(a.size());

        ll borrow = 0;
        for (size_t i = 0; i < a.size(); ++i) {
            ll x = a[i];
            ll y = (i < b.size() ? b[i] : 0);

            ll s = x - y - borrow;
            if (s < 0) {
                s += MAXCARRY;
                borrow = 1;
            } else
                borrow = 0;

            res.push_back(s);
        }

        trim(res);
        return res;
    }
   public:
    big_int() : number(1, 0), neg(false) {}
    big_int(ll x) {
        if (x < 0) {
            neg = true;
            x = -x;
        } else
            neg = false;

        if (x == 0) {
            number = { 0 };
            return;
        }
        while (x) {
            number.push_back(x % MAXCARRY);
            x /= MAXCARRY;
        }
    }
    friend istream& operator>>(istream& in, big_int& n) {
        string s;
        in >> s;

        n.number.clear();
        n.neg = false;

        if (s[0] == '-') {
            n.neg = true;
            s = s.substr(1);
        }

        int pos = 0;
        while (pos + 1 < (int)s.size() && s[pos] == '0') pos++;
        s = s.substr(pos);

        for (int i = s.size(); i > 0; i -= MAXLEN) {
            int l = max(0, i - MAXLEN);
            ll x = 0;
            for (int j = l; j < i; ++j)
                x = x * 10 + (s[j] - '0');

            n.number.push_back(x);  // LSB-first
        }

        if (n.number.empty())
            n.number.push_back(0);

        if (n.number.size() == 1 && n.number[0] == 0)
            n.neg = false;

        return in;
    }
    friend ostream& operator<<(ostream& out, const big_int& n) {
        if (n.neg) out << '-';

        int sz = n.number.size();
        out << n.number[sz - 1];

        for (int i = sz - 2; i >= 0; --i)
            out << setw(MAXLEN) << setfill('0') << n.number[i];
        return out;
    }
    big_int operator+(const big_int& other) const {
        big_int res;

        if (neg == other.neg) {
            res.number = addAbs(number, other.number);
            res.neg = neg;
        } else {
            int c = cmpAbs(number, other.number);
            if (c == 0) return big_int();

            if (c > 0) {
                res.number = subAbs(number, other.number);
                res.neg = neg;
            } else {
                res.number = subAbs(other.number, number);
                res.neg = other.neg;
            }
        }
        return res;
    }
    big_int operator-(const big_int& other) const {
        big_int tmp = other;
        tmp.neg = !tmp.neg;
        return *this + tmp;
    }
    bool operator<(const big_int& other) const {
        if (neg != other.neg)return neg;
        int c = cmpAbs(number, other.number);
        return neg ? (c > 0) : (c < 0);
    }
    bool operator==(const big_int& other) const {
        return neg == other.neg && number == other.number;
    }
    big_int& operator+=(const big_int& other) {
        *this = *this + other;
        return *this;
    }
    big_int& operator-=(const big_int& other) {
        *this = *this - other;
        return *this;
    }
    bool is_neg() const {
        return neg;
    }
};