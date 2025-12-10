#include <bits/stdc++.h>
using namespace std;

#ifndef DIVIDE_METHOD
#define DIVIDE_METHOD 0
#endif
class big_int{
    private:
        string number;
        static void erase0_1(string&s){int p=s.find_first_not_of(48);p==(int)string::npos?s="0":s.erase(0,p);}
        static void erase0(string&a,string&b){erase0_1(a);erase0_1(b);}
        static bool neg(const string&s){return!s.empty()&&s[0]=='-';}
        static string strip(const string& s){if(s.empty()||s=="-0")return"0";bool n=s[0]=='-';string t=n?s.substr(1):s;erase0_1(t);return t=="0"?"0":n?"-"+t:t;}
        static int absS(string a,string b){erase0(a,b);if(a.size()>b.size())return 1;if(a.size()<b.size())return -1;if(a==b)return 0;return a>b?1:-1;}
        static string addS(string a,string b){string r;int i=a.size()-1,j=b.size()-1,c=0;while(i+1||j+1||c){c+=(i+1?a[i--]-48:0)+(j+1?b[j--]-48:0);r+=c%10+48;c/=10;}reverse(r.begin(),r.end());return r;}
        static string subS(const string&a,const string&b){bool n=neg(a),m=neg(b);if(n&&m)return subS(b.substr(1),a.substr(1));if(n^m)return n?"-"+addS(a.substr(1),b):addS(a,b.substr(1));int c=absS(a,b);if(!c)return"0";const string&x=c>0?a:b,&y=c>0?b:a;int i=x.size()-1,j=y.size()-1,k=0;string r;while(i>=0){int v=x[i--]-48-(j>=0?y[j--]-48:0)-k;k=v<0;if(v<0)v+=10;r+=v+48;}while(r.size()>1&&r.back()==48)r.pop_back();reverse(r.begin(),r.end());return c>0?r:"-"+r;}
        static string sumS(string a,string b){bool x=neg(a),y=neg(b);string A=x?a.substr(1):a,B=y?b.substr(1):b;if(!(x^y))return x?"-"+addS(A,B):addS(A,B);int c=absS(A,B);return c?((c>0?(x?"-":"")+subS(A,B):(y?"-":"")+subS(B,A))):"0";}
        static void fft(vector<complex<double>>&a){int n=a.size(),l=31-__builtin_clz(n);static vector<complex<long double>>r(2,1);static vector<complex<double>>t(2,1);for(static int k=2;k<n;k*=2){r.resize(n);t.resize(n);auto x=polar(1.0L,acos(-1.0L)/k);for(int i=k;i<k+k;i++)t[i]=r[i]=i&1?r[i/2]*x:r[i/2];}vector<int>v(n);for(int i=0;i<n;i++)v[i]=(v[i/2]|(i&1)<<l)/2;for(int i=0;i<n;i++)if(i<v[i])swap(a[i],a[v[i]]);for(int k=1;k<n;k*=2)for(int i=0;i<n;i+=2*k)for(int j=0;j<k;j++){auto x=(double*)&t[j+k],y=(double*)&a[i+j+k];complex<double>z(x[0]*y[0]-x[1]*y[1],x[0]*y[1]+x[1]*y[0]);a[i+j+k]=a[i+j]-z;a[i+j]+=z;}}
        template<typename T>
        static vector<T>conv(const vector<T>&a, const vector<T>&b){if(a.empty()||b.empty())return {};vector<T>res((int)a.size()+(int)b.size()-1);int L=32-__builtin_clz((int)res.size()), n=1<<L;vector<complex<double>>in(n), out(n);copy(a.begin(),a.end(),begin(in));for(int i=0;i<(int)b.size();i++)in[i].imag(b[i]);fft(in);for(complex<double>&x:in)x*=x;for(int i=0;i<n;i++)out[i]=in[-i&(n-1)]-conj(in[i]);fft(out);for(int i=0;i<(int)res.size();i++)res[i]=static_cast<T>(imag(out[i])/(4*n)+((is_integral_v<T>)?(imag(out[i])>0?0.5:-0.5):0));return res;}
        static string mulS(const string&a,const string&b){int B=1000,D=3;bool n=neg(a),m=neg(b);string x=n?a.substr(1):a,y=m?b.substr(1):b;if(x=="0"||y=="0")return"0";vector<int>p,q;for(int i=x.size();i>0;i-=D){int t=0,l=max(0,i-D);for(int j=l;j<i;j++)t=t*10+x[j]-48;p.push_back(t);}for(int i=y.size();i>0;i-=D){int t=0,l=max(0,i-D);for(int j=l;j<i;j++)t=t*10+y[j]-48;q.push_back(t);}vector<long long>r=conv(vector<long long>(p.begin(),p.end()),vector<long long>(q.begin(),q.end()));long long c=0;for(size_t i=0;i<r.size();i++){long long t=r[i]+c;r[i]=t%B;c=t/B;}while(c){r.push_back(c%B);c/=B;}while(r.size()>1&&r.back()==0)r.pop_back();string s=to_string(r.back());for(int i=r.size()-2;i>=0;i--){string t=to_string((int)r[i]);while(t.size()<D)t='0'+t;s+=t;}return n^m?"-"+s:s;}
        static string check_modS(const string&a,const string&b,string q){return subS(a,mulS(b,q));}
        static string divS(const string& a,const string& b){string c=a,d=b;bool e=neg(c),f=neg(d);if(e)c=c.substr(1);if(f)d=d.substr(1);if(c=="0")return"0";if(d=="0")throw out_of_range("dividebyzeroerror");string g="",h="0";for(char k:c){h+=k;erase0_1(h);int i=0;while(1){string j=subS(h,d);if(neg(j))break;h=j;i++;}g+=i+48;}erase0_1(g);if(e^f)g="-"+g;if(DIVIDE_METHOD){if(neg(check_modS(a,b,g))){if(e^f)g=subS(g,"1");else g=sumS(g,"1");}}else{if(check_modS(a,b,g)=="0")return g=="-0"?"0":g;if(e^f)g=subS(g,"1");}return g=="-0"?"0":g;}
        static string modS(string a,string b){return subS(a,mulS(b,divS(a,b)));}
    public:
        big_int():number("0"){}
        big_int(const string&s):number(strip(s)){}
        big_int(const char*s):number(strip(string(s))){}
        big_int(int x){number=to_string(x);}
        big_int(long long x){number=to_string(x);}
        string str()const{return number;}
        friend istream&operator>>(istream& i,big_int& n){string s;i>>s;n=big_int(s);return i;}
        friend ostream&operator<<(ostream&o,const big_int&n){o<<n.str();return o;}
        bool operator<(const big_int& n)const{bool a=neg(number),b=neg(n.number);if(a&&!b)return 1;if(!a&&b)return 0;string x=a?number.substr(1):number,y=b?n.number.substr(1):n.number;erase0(x,y);if(x.size()!=y.size())return (x.size()<y.size())^a;return x!=y?(x<y)^a:0;}
        bool operator>(const big_int& n)const{return n<*this;};
        bool operator==(const big_int& n)const{return number==n.number;}
        bool operator<=(const big_int& n)const{return number<n.number||number==n.number;}
        bool operator>=(const big_int& n)const{return number>n.number||number==n.number;}
        bool operator!=(const big_int& n)const{return!(number==n.number);}
        big_int operator+(const big_int& n)const{return big_int(sumS(this->number,n.number));}
        big_int operator-(const big_int& n)const{return big_int(subS(this->number,n.number));}
        big_int operator*(const big_int& n)const{return big_int(mulS(this->number,n.number));}
        big_int operator/(const big_int& n)const{return big_int(divS(this->number,n.number));}
        big_int operator%(const big_int& n)const{return big_int(modS(this->number,n.number));}
        big_int operator+=(const big_int& n){this->number=strip(sumS(this->number,n.number));return*this;}
        big_int operator-=(const big_int& n){this->number=strip(subS(this->number,n.number));return*this;}
        big_int operator*=(const big_int& n){this->number=strip(mulS(this->number,n.number));return*this;}
        big_int operator/=(const big_int& n){this->number=strip(divS(this->number,n.number));return*this;}
        big_int&operator++(){*this+=big_int("1");return*this;}
        big_int operator++(int){big_int t=*this;++(*this);return t;}
        big_int&operator--(){*this-=big_int("1");return*this;}
        big_int operator--(int){big_int t=*this;--(*this);return t;}
};
int main(){
    return 0;
}