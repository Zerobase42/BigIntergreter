#ifdef __linux__
#include<unistd.h>
#else
#include<stdio.h>
#define read(a,b,c)fread(b,1,c,stdin)
#define write(a,b,c)fwrite(b,1,c,stdout)
#endif
#pragma GCC optimize("O3,unroll-loops")
#include <algorithm>
#include <string>
#include <vector>
using namespace std;
using ll=long long;
using i128=__int128_t;
ll modpow(ll a,ll e,ll mod){
    ll r=1;
    while(e){
        if(e&1)r=(i128)r*a%mod;
        a=(i128)a*a%mod;e>>=1;
    }
    return r;
}
template<int MOD,int G>
struct NTT{
    static int pw(ll a,ll e){
        ll r=1;
        while(e){
            if(e&1)r=r*a%MOD;
            a=a*a%MOD;e>>=1;
        }
        return(int)r;
    }
    static void ntt(vector<int>&a,bool inv){
        int n=a.size();
        for(int i=1,j=0;i<n;i++){
            int bit=n>>1;
            while(j&bit){
                j^=bit;
                bit>>=1;
            }
            j^=bit;
            if(i<j)
                swap(a[i],a[j]);
        }
        for(int len=2;len<=n;len<<=1){
            int wlen=pw(G,(MOD-1)/len);
            if(inv)wlen=pw(wlen,MOD-2);
            for(int i=0;i<n;i+=len){
                ll w=1;
                int half=len>>1;
                for(int j=0;j<half;j++){
                    int u=a[i+j],v=(ll)a[i+j+half]*w%MOD;
                    int x=u+v,y=u-v;
                    if(x>=MOD)x-=MOD;
                    if(y<0)y+=MOD;
                    a[i+j]=x;
                    a[i+j+half]=y;
                    w=w*wlen%MOD;
                }
            }
        }
        if(inv){
            int invn=pw(n,MOD-2);
            for(int&x:a)x=(ll)x*invn%MOD;
        }
    }
    static vector<int>mul(vector<int>a,vector<int>b){
        int need=a.size()+b.size()-1;
        int n=1;
        while(n<need)
            n<<=1;
        a.resize(n);
        b.resize(n);
        ntt(a,0);
        ntt(b,0);
        for(int i=0;i<n;i++)
            a[i]=(ll)a[i]*b[i]%MOD;
        ntt(a,1);
        a.resize(need);
        return a;
    }
};
constexpr int MOD1=998244353;
constexpr int MOD2=1004535809;
using NTT1=NTT<MOD1,3>;
using NTT2=NTT<MOD2,3>;
vector<int>multiply_bigint(const string&A,const string&B){
    if(A=="0"||B=="0")
        return{0};
    const int BASE=10000;
    const int DIG=4;
    vector<int>a,b;
    for(int i=(int)A.size();i>0;i-=DIG){
        int x=0;
        int l=max(0,i-DIG);
        for(int j=l;j<i;j++)
            x=x*10+(A[j]-'0');
        a.push_back(x);
    }
    for(int i=(int)B.size();i>0;i-=DIG){
        int x=0;
        int l=max(0,i-DIG);
        for(int j=l;j<i;j++)
            x=x*10+(B[j]-'0');
        b.push_back(x);
    }
    auto x=NTT1::mul(a,b);
    auto y=NTT2::mul(a,b);
    int n=x.size();
    vector<ll>c(n);
    static const ll m1=MOD1;
    static const ll m2=MOD2;
    static const ll inv_m1=
        modpow(m1,m2-2,m2);
    for(int i=0;i<n;i++){
        ll t=
            (y[i]-x[i])%m2;
        if(t<0)
            t+=m2;
        t=(i128)t*inv_m1%m2;
        c[i]=x[i]+(i128)m1*t;
    }
    ll carry=0;
    for(int i=0;i<n;i++){
        ll cur=c[i]+carry;
        c[i]=cur%BASE;
        carry=cur/BASE;
    }
    while(carry){
        c.push_back(carry%BASE);
        carry/=BASE;
    }
    while(c.size()>1&&c.back()==0)
        c.pop_back();
    vector<int>res(c.begin(),c.end());
    return res;
}
char outbuf[1<<22];
int main() {
    static char s1[1 << 20], s2[1 << 20];

    // scanf로 문자열 입력 받기
    if (scanf("%s %s", s1, s2) != 2) return 0;

    auto v = multiply_bigint(s1, s2);
    if (v.empty()) return 0;

    // 결과 출력
    printf("%d", v.back());  // 가장 앞자리 출력
    for (int i = (int)v.size() - 2; i >= 0; i--) {
        printf("%04d", v[i]);  // 4자리씩 맞추어 출력
    }
    return 0;
}