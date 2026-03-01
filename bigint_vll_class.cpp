#include <bits/stdc++.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
#define ll long long
#define vll vector<ll>
constexpr int MAXLEN = 18;
constexpr ll pow10(int n) {
    return n == 0 ? 1LL : 10LL * pow10(n - 1);
}
constexpr ll MAXCARRY = pow10(MAXLEN);
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
    static void fft(vector<complex<double>>& a) {
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
                    // complex<double>z=rt[j+k]*a[i+j+k];//(25% faster if hand-rolled)///include-line
                    auto x = (double*)&rt[j + k], y = (double*)&a[i + j + k];                 /// exclude-line
                    complex<double> z(x[0] * y[0] - x[1] * y[1], x[0] * y[1] + x[1] * y[0]);  /// exclude-line
                    a[i + j + k] = a[i + j] - z;
                    a[i + j] += z;
                }
        }
    }
    template <typename T>
    static vector<T> conv(const vector<T>& a, const vector<T>& b) {
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
            res[i] = static_cast<T>(imag(out[i]) / (4 * n) + ((is_integral_v<T>) ? (imag(out[i]) > 0 ? 0.5 : -0.5) : 0));
        }
        return res;
    }
    static big_int mulS(const big_int& A, const big_int& B) {
        const ll BASE = 1000;  // 10^3
        if (A.number.size() == 1 && A.number[0] == 0) return big_int();
        if (B.number.size() == 1 && B.number[0] == 0) return big_int();

        big_int R;
        R.neg = A.neg ^ B.neg;

        vll a, b;

        for (ll x : A.number) {
            for (int i = 0; i < MAXLEN; i += 3) {
                a.push_back(x % BASE);
                x /= BASE;
            }
        }

        for (ll x : B.number) {
            for (int i = 0; i < MAXLEN; i += 3) {
                b.push_back(x % BASE);
                x /= BASE;
            }
        }

        auto c = conv(a, b);

        ll carry = 0;
        for (size_t i = 0; i < c.size(); ++i) {
            __int128 t = (__int128)c[i] + carry;
            c[i] = (ll)(t % BASE);
            carry = (ll)(t / BASE);
        }

        while (carry) {
            c.push_back(carry % BASE);
            carry /= BASE;
        }

        R.number.clear();

        for (size_t i = 0; i < c.size(); i += 6) {
            __int128 val = 0;
            for (int j = 5; j >= 0; --j) {
                val *= BASE;
                if (i + j < c.size())
                    val += c[i + j];
            }
            R.number.push_back((ll)val);
        }
        trim(R.number);
        if (R.number.size() == 1 && R.number[0] == 0) R.neg = false;
        return R;
    }

   public:
    big_int() : number(1, 0), neg(false) {}
    big_int(ll x) {
        neg = x < 0;
        if (neg) x = -x;
        if (x == 0) {
            number = { 0 };
            return;
        }
        while (x) {
            number.push_back(x % MAXCARRY);
            x /= MAXCARRY;
        }
    }
    /*
    big_int(string s) {
        if(s=="0")return;
        neg=s[0]=='-';
        if(neg)s=s.substr(1);
        int ssize=s.size();
        for(int i=0;i<ssize;++i){
            int j=(ssize-i-1)/MAXLEN;
            number[j]=10*v[j]+s[i]-'0';
        }
    }*/
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
        if (n.number.empty) return out << "0";
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
    big_int operator*(const big_int& other) const {
        return mulS(*this, other);
    }
    bool operator<(const big_int& other) const {
        if (neg != other.neg) return neg;
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
    big_int& operator*=(const big_int& other) {
        *this = *this * other;
        return *this;
    }
    bool is_neg() const {
        return neg;
    }
};