#include <bits/stdc++.h>
#define DIVIDE_METHOD true
using namespace std;
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
        static string mulS(const string& A,const string& B){
            string a=A,b=B;
            bool k=neg(a)^neg(b);
            if(a[0]=='-')a=a.substr(1);
            if(b[0]=='-')b=b.substr(1);
            if(a=="0"||b=="0")return"0";
            int n=a.size(),m=b.size();
            int r[n+m];
            memset(r,0,sizeof(int)*(n+m));
            for(int i=0;i<m;i++)
                for(int j=0;j<n;j++){
                    int t=(a[n-1-j]-'0')*(b[m-1-i]-'0');
                    r[n+m-1-i-j]+=t;
                }
            for(int i=n+m-1;i>0;i--)r[i-1]+=r[i]/10,r[i]%=10;
            bool f=0;
            string s="";
            for(int i=0;i<n+m;i++){
                if(!f){
                    if(r[i]!=0)f=1;
                    else if(i==n+m-1)s+="0";
                }
                if(f)s+=r[i]+'0';
            }
        return k?"-"+s:s;
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
        string str()const{return number;}
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
};

int main(){
    return 0;
}