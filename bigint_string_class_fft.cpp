#include <bits/stdc++.h>
using namespace std;

#ifndef DIVIDE_METHOD
#define DIVIDE_METHOD true
#endif
class big_int{
    private:
        string number;
        template<typename T>
        static void erase0_1(string& s){
            int p=s.find_first_not_of('0');
            if(p==(int)string::npos)s="0";
            else s.erase(0,p);
        }
        static void erase0(string& a,string& b){
            erase0_1(a);erase0_1(b);
        }
        static bool neg(const string& s){
            return !s.empty()&&s[0]=='-';
        }
        static string strip(const string& s){
            string t;
            if(s.empty()||s=="-0")return"0";
            if(s[0]=='-'){
                t=s.substr(1);erase0_1(t);
                return t=="0"?"0":"-"+t;
            }else{
                t=s;erase0_1(t);
                return t.empty()?"0":t;
            }
        }
        static int absS(string a,string b){
            erase0(a,b);
            if(a.size()>b.size())return 1;
            if(a.size()<b.size())return -1;
            if(a==b)return 0;
            return(a>b?1:-1);
        }
        static string addS(string a,string b){
            string r;
            int i=(int)a.size()-1,j=(int)b.size()-1,c=0;
            while(i>=0||j>=0||c){
                c+=(i>=0?a[i--]-'0':0)+(j>=0?b[j--]-'0':0);
                r+=c%10+'0';c/=10;
            }
            reverse(r.begin(),r.end());
            return r;
        }
        static string subS(const string& a,const string& b){
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
        static string sumS(string a,string b){
            bool na=neg(a),nb=neg(b);
            string A=na?a.substr(1):a,B=nb?b.substr(1):b;
            if(!(na^nb))return na?"-"+addS(A,B):addS(A,B);
            int cmp=absS(A,B);if(!cmp)return"0";
            return(cmp>0?(na?"-":"")+subS(A,B):(nb?"-":"")+subS(B,A));
        }
        static void fft(vector<complex<double>>&a){
            int n=(int)a.size(),L=31-__builtin_clz(n);
            static vector<complex<long double>>R(2,1);
            static vector<complex<double>>rt(2,1);//(^10% faster if double)
            for(static int k=2;k<n;k*=2){
                R.resize(n);
                rt.resize(n);
                auto x=polar(1.0L, acos(-1.0L)/k);
                for(int i=k;i<k+k;i++)rt[i]=R[i]=i&1?R[i/2]*x:R[i/2];
            }
            vector<int>rev(n);
            for(int i=0;i<n;i++)rev[i]=(rev[i/2]|(i&1)<<L)/2;
            for(int i=0;i<n;i++)if(i<rev[i])swap(a[i], a[rev[i]]);
            for(int k=1;k<n;k*=2){
                for(int i=0;i<n;i+=2*k)for(int j=0;j<k;j++){
                    //complex<double>z=rt[j+k]*a[i+j+k];//(25% faster if hand-rolled)///include-line
                    auto x=(double*)&rt[j+k],y=(double*)&a[i+j+k];            ///exclude-line
                    complex<double>z(x[0]*y[0]-x[1]*y[1],x[0]*y[1]+x[1]*y[0]);///exclude-line
                    a[i+j+k]=a[i+j]-z;
                    a[i+j]+=z;
                }
            }
        }    
        static vector<T>conv(const vector<T>&a, const vector<T>&b){
        	if(a.empty()||b.empty())return {};
        	vector<T>res((int)a.size()+(int)b.size()-1);
        	int L=32-__builtin_clz((int)res.size()), n=1<<L;
        	vector<complex<double>>in(n), out(n);
        	copy(a.begin(),a.end(),begin(in));
        	for(int i=0;i<(int)b.size();i++)in[i].imag(b[i]);
        	fft(in);
        	for(complex<double>&x:in)x*=x;
        	for(int i=0;i<n;i++)out[i]=in[-i&(n-1)]-conj(in[i]);
        	fft(out);
        	for(int i=0;i<(int)res.size();i++){
        		res[i]=static_cast<T>(imag(out[i])/(4*n)+((is_integral_v<T>)?(imag(out[i])>0?0.5:-0.5):0));
        	}
        	return res;
        }
        static string mulS(const string& A,const string& B){
        	int BASE=1000,DIG=3; // BASE=10^DIG
        
        	bool na=neg(A),nb=neg(B);
        	string sA=na?A.substr(1):A,sB=nb?B.substr(1):B;
        	if(sA=="0"||sB=="0")return "0";
        	vector<int>a,b;
        	for(int i=A.size();i>0;i-=DIG){
        		int x=0,l=max(0,i-DIG);
        		for(int j=l;j<i;j++)x=x*10+A[j]-'0';
        		a.push_back(x);
        	}
        	for(int i=sB.size()-1;i>=0;i--){
        		int x=0,l=max(0,i-DIG);
        		for(int j=l;j<i;j++)x=x*10+B[j]-'0';
        		b.push_back(x);
        	}
        	vector<long long>A64(a.begin(),a.end()),B64(b.begin(),b.end());
        	vector<long long>c=conv(A64,B64);
        	
        	long long int carry=0;
        	for(size_t i=0;i<(int)c.size();i++){
        		long long x=c[i]+carry;
        		c[i]=x/BASE;carry=x/BASE;
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
        	return res;
        }
        static string check_modS(const string& A,const string& B,string q){
            return subS(A,mulS(B,q));
        }
        static string divS(const string& A,const string& B){
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
        static string modS(string a,string b){
            return subS(a,mulS(b,divS(a,b)));
        }

    public:
        big_int():number("0"){}
        big_int(const string& s):number(strip(s)){}
        big_int(const char* s):number(strip(string(s))){}
        big_int(int x) {
            number=to_string(x);
        }
        big_int(long long x) {
            number = to_string(x);
        }
        string str()const{
            return number;
        }
        friend istream& operator>>(istream& in, big_int& n){
            string s;
            in>>s;
            n=big_int(s);
            return in;
        }
        friend ostream& operator<<(ostream& out,const big_int& n){
            out<<n.str();
            return out;
        }
        bool operator<(const big_int& n)const{
            bool na=neg(number),nb=neg(n.number);
            if (na&&!nb)return true;
            if (!na&&nb)return false;
            string a=(na?number.substr(1):number),b=(nb?n.number.substr(1):n.number);
            erase0(a,b);
            if(a.size()!=b.size())return(a.size()<b.size())^na;
            if(a!=b)return(a<b)^na;
            return false;
        };
        bool operator>(const big_int& n)const{
            return n<*this;
        };
        bool operator==(const big_int& n)const{
            return number==n.number;
        }
        bool operator<=(const big_int& n)const{
            return number<n.number||number==n.number;
        }
        bool operator>=(const big_int& n)const{
            return number>n.number||number==n.number;
        }
        bool operator!=(const big_int& n)const{
            return !(number==n.number);
        }
        big_int operator+(const big_int& n)const{
            return big_int(sumS(this->number,n.number));
        }
        big_int operator-(const big_int& n)const{
            return big_int(subS(this->number,n.number));
        }
        big_int operator*(const big_int& n)const{
            return big_int(mulS(this->number,n.number));
        }
        big_int operator/(const big_int& n)const{
            return big_int(divS(this->number,n.number));
        }
        big_int operator%(const big_int& n)const{
            return big_int(modS(this->number,n.number));
        }
        big_int operator+=(const big_int& n){
            this->number=strip(sumS(this->number,n.number));
            return *this;
        }
        big_int operator-=(const big_int& n){
            this->number=strip(subS(this->number,n.number));
            return *this;
        }
        big_int operator*=(const big_int& n){
            this->number=strip(mulS(this->number,n.number));
            return *this;
        }
        big_int operator/=(const big_int& n){
            this->number=strip(divS(this->number,n.number));
            return *this;
        }
        big_int& operator++(){
            *this+=big_int("1");
            return *this;
        }
        big_int operator++(int){
            big_int tmp=*this;
            ++(*this);
            return tmp;
        }
        big_int& operator--(){
            *this-=big_int("1");
            return *this;
        }
        big_int operator--(int){
            big_int tmp=*this;
            --(*this);
            return tmp;
        }
};

int main(){
    return 0;
}