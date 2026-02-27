#include <bits/stdc++.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
#define MAXCARRY 1000000000LL
#define vll vector<long long>
#define ll long long
using namespace std;
class big_int {
   private:
    vll number;  // 앞 자리가 큰 수
    bool neg;    // neg가 true면 음수
    static void normalize(vll& a) {
        int i = 0;
        while (i + 1 < a.size() && a[i] == 0) i++;
        if (i > 0) a.erase(a.begin(), a.begin() + i);
    }
    static int cmpAbs(const vll& a, const vll& b) {
        if (a.size() != b.size())
            return a.size() < b.size() ? -1 : 1;
        for (size_t i = 0; i < a.size(); i++) {
            if (a[i] != b[i])
                return a[i] < b[i] ? -1 : 1;
        }
        return 0;
    }
    static vll sumS(const vll& a, const vll& b) {
        vll res;
        int i = (int)a.size() - 1, j = (int)b.size() - 1;
        ll c = 0;
        while (i >= 0 || j >= 0 || c) {
            ll x = (i >= 0 ? a[i] : 0), y = (j >= 0 ? b[j] : 0), s = x + y + c;
            res.push_back(s % MAXCARRY);
            c = s / MAXCARRY;
            i--;
            j--;
        }
        reverse(res.begin(), res.end());
        normalize(res);
        return res;
    }
    static vll subS(const vll& a, const vll& b) {
        vll res;
        int i = (int)a.size() - 1, j = (int)b.size() - 1;
        ll borrow = 0;
        while (i >= 0) {
            ll x = a[i], y = (j >= 0 ? b[j] : 0), s = x - y - borrow;
            if (s < 0) s += MAXCARRY;
            borrow = (ll)s < 0;
            res.push_back(s);
            i--;
            j--;
        }
        reverse(res.begin(), res.end());
        normalize(res);
        return res;
    }

   public:
    big_int() : number(1, 0), neg(false) {}
    big_int(long long x) {
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
        reverse(number.begin(), number.end());
    }
    friend istream& operator>>(istream& in, big_int& n) {
        string s;
        in >> s;
        n.neg = false;
        n.number.clear();
        if (s[0] == '-') {
            n.neg = true;
            s = s.substr(1);
        }
        int pos = 0;
        while (pos + 1 < (int)s.size() && s[pos] == '0') pos++;
        s = s.substr(pos);
        for (int i = s.size(); i > 0; i -= 9) {
            int x = 0, l = max(0, i - 9);
            for (int j = l; j < i; j++) x = x * 10 + (s[j] - '0');
            n.number.push_back(x);
        }
        reverse(n.number.begin(), n.number.end());
        if (n.number.empty()) n.number.push_back(0);
        if (n.number.size() == 1 && n.number[0] == 0) n.neg = false;
        return in;
    }
    friend ostream& operator<<(ostream& out, const big_int& n) {
        if (n.neg) out << '-';
        out << n.number[0];
        for (int i = 1; i < (int)n.number.size(); i++)
            out << setw(9) << setfill('0') << n.number[i];
        return out;
    }
    big_int operator+(const big_int& n) const {
        big_int res;
        if (this->neg == n.neg) {
            res.number = sumS(this->number, n.number);
            res.neg = this->neg;
        } else {
            int c = cmpAbs(this->number, n.number);

            if (c == 0)
                return big_int();
            else if (c > 0) {
                res.number = subS(this->number, n.number);
                res.neg = this->neg;
            } else {
                res.number = subS(n.number, this->number);
                res.neg = n.neg;
            }
        }
        return res;
    }
};