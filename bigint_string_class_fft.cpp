#include <bits/stdc++.h>
using namespace std;

#ifndef DIVIDE_METHOD
#define DIVIDE_METHOD true
#endif
class big_int{
    private:
        string number;
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
        static void fft(vector<complex<double>> &a,bool inv=false){
            int n=a.size();
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
        string mulS(const string& a,const string& b){
            vector<string>s={a,b};
            vector<vector<complex<double>>>f(2,vector<complex<double>>(1<<19));
            for(int i=0;i<2;++i)
                for(int j=s[i].size()-1,k=0;j>=0;--j){
                    k=10*k+s[i][s[i].size()-j-1]-'0';
                    if (j%4==0){
                        f[i][j / 4]=k;
                        k=0;
                    }
                }
            fft(f[0]);fft(f[1]);
            for (int i=0;i<f[0].size();++i)f[0][i]*=f[1][i];
            fft(f[0],true);
            vector<long long> A(f[0].size());
            for (int i=0;i<A.size()-1;++i){
                A[i]+=lround(f[0][i].real());
                A[i+1]+=A[i]/10000;
                A[i]%=10000;
            }
            bool flag=false;
            string res;
            for(int i=A.size()-1;i>=0;--i){
                if(flag){
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