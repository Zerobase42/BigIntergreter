#include <bits/stdc++.h>

using namespace std;

using ll = long long;
using cd = complex<double>;

const double pi = acos(-1);

void fft(vector<complex<double>> &a, bool inv = false) {
	int n = a.size();
	for (int k = 0; k < n; ++k) {
		int b{};
		for (int z = 1; z < n; z *= 2) {
			b *= 2;
			if (k & z) {
				++b;
			}
		}
		if (k < b) {
			swap(a[k], a[b]);
		}
	}
	static vector<complex<double>> r, ir;
	if (r.empty()) {
		r.resize(n / 2);
		ir.resize(n / 2);
		for (int i = 0; i < r.size(); ++i) {
			r[i] = complex<double>(cos(2 * pi / n * i), sin(2 * pi / n * i));
			ir[i] = conj(r[i]);
		}
	}
	for (int m = 2; m <= n; m *= 2) {
		for (int k = 0; k < n; k += m) {
			for (int j = 0; j < m / 2; ++j) {
				complex<double> u = a[k + j];
				complex<double> t = a[k + j + m / 2] * (inv ? ir[n / m * j] : r[n / m * j]);
				a[k + j] = u + t;
				a[k + j + m / 2] = u - t;
			}
		}
	}
	if (inv) {
		for (int i = 0; i < n; ++i) {
			a[i] /= n;
		}
	}
}

string multyply(string a,string b){
    vector<string>s={a,b};
    vector<vector<complex<double>>>f(2,vector<complex<double>>(1<<19));
	for(int i=0;i<2;++i){
		for(int j=s[i].size()-1,k=0;j>=0;--j){
			k=10*k+s[i][s[i].size()-j-1]-'0';
			if (j%4==0){
				f[i][j / 4]=k;
				k=0;
			}
		}
	}
	fft(f[0]);fft(f[1]);
	for (int i=0;i<f[0].size();++i) {
		f[0][i]*=f[1][i];
	}
	fft(f[0],true);
	vector<long long> A(f[0].size());
	for (int i=0;i<A.size()-1;++i) {
		A[i]+=lround(f[0][i].real());
		A[i+1]+=A[i]/10000;
		A[i]%=10000;
	}
	bool flag=false;
    string res;
	for(int i=A.size()-1;i>=0;--i){
		if (flag) {
            char buf[10];
            sprintf(buf,"%04ld",A[i]);
            res+=buf;
		}else if(A[i]>0||i==0){
			res+=to_string(A[i]);
			flag=true;
		}
	}
    return res;
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(nullptr);
    string a,b;
	cin >> a>>b;
	cout << multyply(a,b) << "\n";
	return 0;
}