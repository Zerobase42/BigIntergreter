#include<bits/stdc++.h>
using namespace std;
bool neg(const string& s){
    return !s.empty()&&s[0]=='-';
}

void fft(vector<complex<double>>&a){
	int n=(int)a.size(), L=31-__builtin_clz(n);
	static vector<complex<long double>>R(2,1);
	static vector<complex<double>>rt(2,1); //(^10% faster if double)
	for(static int k=2; k<n; k*=2){
		R.resize(n);rt.resize(n);
		auto x=polar(1.0L, acos(-1.0L)/k);
		for(int i=k;i<k+k;i++)rt[i]=R[i]=i&1?R[i/2]*x:R[i/2];
	}
	vector<int>rev(n);
	for(int i=0;i<n;i++)rev[i]=(rev[i/2]|(i&1)<<L)/2;
	for(int i=0;i<n;i++)if(i<rev[i])swap(a[i], a[rev[i]]);
	for(int k=1;k<n;k*=2){
		for(int i=0;i<n;i+=2*k)for(int j=0;j<k;j++){
			//complex<double>z=rt[j+k]*a[i+j+k];//(25% faster if hand-rolled)    ///include-line
			auto x=(double*)&rt[j+k],y=(double*)&a[i+j+k];                       ///exclude-line
			complex<double>z(x[0]*y[0]-x[1]*y[1], x[0]*y[1]+x[1]*y[0]);          ///exclude-line
			a[i+j+k]=a[i+j]-z;
			a[i+j]+=z;
		}
	}
}
template<typename T>
vector<T>conv(const vector<T>&a,const vector<T>&b){
	if(a.empty()||b.empty())return {};
	vector<T>res((int)a.size()+(int)b.size()-1);
	int L=32-__builtin_clz((int)res.size()),n=1<<L;
	vector<complex<double>>in(n),out(n);
	copy(a.begin(),a.end(),begin(in));
	for(int i=0;i<(int)b.size();i++)in[i].imag(b[i]);
	fft(in);
	for(complex<double>&x:in)x*=x;
	for(int i=0;i<n;i++)out[i]=in[-i&(n-1)]-conj(in[i]);
	fft(out);
	// if not uses integral, use this.
	// for(int i=0;i<(int)res.size();i++) 
	//     res[i]=static_cast<T>(imag(out[i])/(4*n)+(is_integral_v<T>?(imag(out[i])>0?0.5:-0.5):0));
    for (int i = 0; i < (int)res.size(); ++i) {
        double val = imag(out[i]) / (4 * n);
        res[i] = (long long)(val > 0 ? val + 0.5 : val - 0.5);
    }
    return res;
}

string multy(const string& A,const string& B){
	const int BASE=1000,DIG=3; // BASE=10^DIG

	bool na=neg(A),nb=neg(B);
	string sA=na?A.substr(1):A,sB=nb?B.substr(1):B;
	if(sA=="0"||sB=="0")return "0";
	vector<int>a,b;
	for(int i=sA.size();i>=0;i-=DIG){
		int x=0,l=max(0,i-DIG);
		for(int j=l;j<i;j++)x=x*10+A[j]-'0';
		a.push_back(x);
	}
	for(int i=sB.size();i>=0;i-=DIG){
		int x=0,l=max(0,i-DIG);
		for(int j=l;j<i;j++)x=x*10+B[j]-'0';
		b.push_back(x);
	}
	vector<long long>A64(a.begin(),a.end()),B64(b.begin(),b.end());
	vector<long long>c=conv(A64,B64);
	
	long long carry=0;
	for(size_t i=0;i<(int)c.size();i++){
		long long x=c[i]+carry;
		c[i]=x%BASE;carry=x/BASE;
	}
	while(carry){
		c.push_back(carry%BASE);
		carry/=BASE;
	}
	while(c.size()>1&&c.back()==0)c.pop_back();
	string res=to_string(c.back());
	char buf[20];
	for(int i=(int)c.size()-2;i>=0;i--){
		snprintf(buf,sizeof(buf),"%0*d",DIG,(int)c[i]);
		res+=buf;
	}
	return (na^nb?"-":"")+res;
}
/*
int main(){
    ios_base::sync_with_stdio(false);cin.tie(NULL);
    string a,b;
    cin>>a>>b;
    cout<<multy(a,b);
}
*/