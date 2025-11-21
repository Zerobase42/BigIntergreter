// BigIntergreter On C++
//multyply : FFT
// made by 0B42(ZeroBase42)
// string ver.
#include <iostream>
#include <string>
#include <algorithm>
#include <cstring>
#include <stdexcept>
#include <cmath>
const bool DIVIDE_METHOD=true; // true: 0, false: -inf
using ll=long long;
using cd=complex<double>;
const double pi=acos(-1);
using namespace std;

void erase0_1(string& s){
    int p=s.find_first_not_of('0');
    if(p==(int)string::npos)s="0";
    else s.erase(0,p);
}
inline void erase0(string& a, string& b){
    erase0_1(a);erase0_1(b);
}
inline bool neg(string s){ // negative : 1, not : 0
    return !s.empty()&&s[0]=='-';
}

string maxS(string a,string b){ // return Big number
	bool na=neg(a),nb=neg(b);
    if(na&&!nb)return b;
    if(!na&&nb)return a;
	string A=na?a.substr(1):a,B=nb?b.substr(1):b;
	erase0(A,B);
    if(A.size()!=B.size())return(A.size()>B.size())^na?a:b;
    if(A!=B)return(A>B)^na?a:b;
    return a;
}
int absS(string a,string b){
	erase0(a,b);
    // A>B :1, A==B:0,A<B: -1
	if(a.size()>b.size())return 1;
    if(a.size()<b.size())return -1;
    if(a==b)return 0;
    return(a>b?1:-1);
}
string addS(const string& a,const string& b){ // add A,B (0<=A,B)(not negative)
    string r;
	int i=(int)a.size()-1,j=(int)b.size()-1,c=0;
    while(i>=0||j>=0||c){
        c+=(i>=0?a[i--]-'0':0)+(j>=0?b[j--]-'0':0);
        r+=c%10+'0';c/=10;
    }
    reverse(r.begin(),r.end());
    return r;
}

// ^^ preprocessing code ^^
string subS(const string& a,const string& b){
    bool na=neg(a),nb=neg(b);
    if(na&&nb)return subS(b.substr(1),a.substr(1));
    if(na&&!nb)return "-"+addS(a.substr(1),b);
    if(!na&&nb)return addS(a,b.substr(1));
    int c=absS(a,b);
    if(!c)return"0";
	string r;
    if(c>0){
		int i=a.size()-1,j=b.size()-1,cc=0;
		while(i>=0){
			int x=a[i--]-'0'-(j>=0?b[j--]-'0':0)-cc;cc=x<0?1:0;
            if(x<0)x+=10;
          r+=x+'0';
		}
	}
    else{
		int i=b.size()-1,j=a.size()-1,cc=0;
		while(i>=0){
			int x=b[i--]-'0'-(j>=0?a[j--]-'0':0)-cc;cc=x<0?1:0;
			if(x<0)x+=10;
            r+=x+'0';
		}
	}
	while(r.size()>1&&r.back()=='0')r.pop_back();
	reverse(r.begin(),r.end());return c>0?r:"-"+r;
}
string sumS(string a,string b){
	bool na=neg(a),nb=neg(b);
	string A=na?a.substr(1):a,B=nb?b.substr(1):b;
	if(!(na^nb))return na?"-"+addS(A,B):addS(A,B);
	int cmp=absS(A,B);if(!cmp)return"0";
	return(cmp>0?(na?"-":"")+subS(A,B):(nb?"-":"")+subS(B,A));
}
void fft(vector<cd>& a,bool inv=false){
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
	static vector<cd> r, ir;
	if (r.empty()) {
		r.resize(n / 2);
		ir.resize(n / 2);
		for (int i = 0; i < r.size(); ++i) {
			r[i] = cd(cos(2 * pi / n * i), sin(2 * pi / n * i));
			ir[i] = conj(r[i]);
		}
	}
	for (int m = 2; m <= n; m *= 2) {
		for (int k = 0; k < n; k += m) {
			for (int j = 0; j < m / 2; ++j) {
				cd u = a[k + j];
				cd t = a[k + j + m / 2] * (inv ? ir[n / m * j] : r[n / m * j]);
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
string mulS(const string& a,const string& b){
	vector<string>s={a,b};
    vector<vector<cd>>f(2,vector<cd>(1<<19));
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
	vector<ll> A(f[0].size());
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
inline string check_modS(const string& A,const string& B,string q){
    return subS(A,mulS(B,q));
}
string divS(const string& A,const string& B){
    string a=A,b=B;
	bool na=neg(a),nb=neg(b);
	if(na)a=a.substr(1);
	if(nb)b=b.substr(1);
    if(a=="0")return "0";
	if(b=="0")throw out_of_range("DivideByZeroError");
	string q="",t="0";
	for(char c:a){
		t+=c;erase0_1(t);
		int x=0;
		while(true){
            string nxt=subS(t,b);
            if(neg(nxt))break;
            t=nxt;x++;
        }
        q+=x+'0';
	}
    erase0_1(q);
    if(na^nb)q="-"+q;
    if(DIVIDE_METHOD){
        if(neg(check_modS(A,B,q))){
    	    if(na^nb)q=subS(q,"1");
            else q=sumS(q,"1");
        }
    }else{
        if(check_modS(A,B,q)=="0")return q=="-0"?"0":q;
        if(na^nb)q=subS(q,"1");
    }
    return q=="-0"?"0":q;
}
string modS(string a,string b){
    return subS(a,mulS(b,divS(a,b)));
}
int main(){
	return 0;
}