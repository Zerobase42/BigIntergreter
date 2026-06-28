#ifdef __linux__
#include<unistd.h>
#define syscall(a,b,buf,pos)((a)==0?read(0,buf,pos):write(1,buf,pos))
#else
#include<stdio.h>
#include<string.h>
#define syscall(a,b,buf,pos)((a)==0?read(0,buf,pos):write(1,buf,pos))
#define read(n,buf,pos) fread(buf,1,pos,stdin)
#define write(n,wbuf,pos) fwrite(wbuf,1,pos,stdout)
#endif
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,sse4,avx,avx2,fma")
// #define ll long long
#define u32 unsigned int
#define u64 unsigned long long
#define max(a,b)((a)>(b)?(a):(b))
#define DIG 6
/*
double M=(double)mod1*mod2,N=NUMLEN;
int DIG=(int)((log10(M)-log10(N))/2.0);
*/
#define NUMLEN 1000000
#define BASE 1000000// 10^DIG
#define MAX 1048576 // 2^N <= NUMLEN
#ifndef __cplusplus
const
#else
constexpr
#endif
u32 w1=3,w2=3,mod1=998244353,mod2=1004535809,inv_mod1=669690699;// mod1^{-1} = powmod(mod1,mod2-2,mod2)
static u32 root[MAX>>1],u32 rev[MAX],lastRev;
static u32 a[MAX],b[MAX],c1[MAX],c2[MAX];

static __inline u32 powmod1(u32 a,u32 b){
    u32 ret=1;
    while(b){
        if(b&1) ret=(u64)ret*a%mod1;
        b>>=1;
        a=(u64)a*a%mod1;
    }
    return ret;
}
static __inline u32 powmod2(u32 a,u32 b){
    u32 ret=1;
    while(b){
        if(b&1) ret=(u64)ret*a%mod2;
        b>>=1;
        a=(u64)a*a%mod2;
    }
    return ret;
}
static __inline void init_root1(int n,unsigned char inv){
    u32 ang=powmod1(w1,(mod1-1)/n);
    if(inv) ang=powmod1(ang,mod1-2);
    root[0]=1;
    for(int i=1;i<(n>>1);i++){
        root[i]=(u64)root[i-1]*ang%mod1;
    }
}
static __inline void init_root2(int n,unsigned char inv){
    u32 ang=powmod2(w2,(mod2-1)/n);
    if(inv) ang=powmod2(ang,mod2-2);
    root[0]=1;
    for(int i=1;i<(n>>1);i++){
        root[i]=(u64)root[i-1]*ang%mod2;
    }
}
static __inline void ntt1(u32*f,int n,unsigned char inv){
    for(int i=1;i<n-1;i++){
        if(rev[i]<i){
            u32 tmp=f[i];
            f[i]=f[rev[i]];
            f[rev[i]]=tmp;
        }
    }
    for(int len=2;len<=n;len<<=1){
        int step=n/len;
        for(int j=0;j<n;j+=len){
            for(int k=0;k<(len>>1);k++){
                u32 u=f[j+k],v=(u64)f[j+k+(len>>1)]*root[step*k]%mod1;
                u+=v;v=u+mod1-(v<<1);
                if(u>=mod1)u-=mod1;
                if(v>=mod1)v-=mod1;
                f[j+k]=u;
                f[j+k+(len>>1)]=v;
            }
        }
    }
    if(inv){
        u32 t=powmod1(n,mod1-2);
        for(int i=0;i<n;i++){
            f[i]=(u64)f[i]*t%mod1;
        }
    }
}
static __inline void ntt2(u32*f,int n,unsigned char inv){
    for(int i=1;i<n-1;i++){
        if(rev[i]<i){
            u32 tmp=f[i];
            f[i]=f[rev[i]];
            f[rev[i]]=tmp;
        }
    }
    for(int len=2;len<=n;len<<=1){
        int step=n/len;
        for(int j=0;j<n;j+=len){
            for(int k=0;k<(len>>1);k++){
                u32 u=f[j+k],v=(u64)f[j+k+(len>>1)]*root[step*k]%mod2;
                u+=v;v=u+mod2-(v<<1);
                if(u>=mod2)u-=mod2;
                if(v>=mod2)v-=mod2;
                f[j+k]=u;
                f[j+k+(len>>1)]=v;
            }
        }
    }
    if(inv){
        u32 t=powmod2(n,mod2-2);
        for(int i=0;i<n;i++){
            f[i]=(u64)f[i]*t%mod2;
        }
    }
}
static __inline void conv1(u32*_a,int na,u32*_b,int nb,u32*c){
    int L=32-__builtin_clz(na+nb-2),n=1<<L;
    init_root1(n,0);
    for(int i=0;i<n;i++){
        a[i]=(i<na)?_a[i]:0;
        b[i]=(i<nb)?_b[i]:0;
    }
    ntt1(a,n,0);
    ntt1(b,n,0);
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod1;
    }
    init_root1(n,1);
    ntt1(c,n,1);
}
static __inline void conv2(u32*_a,int na,u32*_b,int nb,u32*c){
    int L=32-__builtin_clz(na+nb-2),n=1<<L;
    init_root2(n,0);
    for(int i=0;i<n;i++){
        a[i]=(i<na)?_a[i]:0;
        b[i]=(i<nb)?_b[i]:0;
    }
    ntt2(a,n,0);
    ntt2(b,n,0);
    for(int i=0;i<n;i++){
        a[i]=(u64)a[i]*b[i]%mod2;
    }
    init_root2(n,1);
    ntt2(c,n,1);
}
static __inline void conv(u32*_a,int na,u32*_b,int nb,u32*c){
    int N=1<<(32-__builtin_clz(na+nb-2));
    if(lastRev!=N){
        int L=31-__builtin_clz(N);
        for(int i=1;i<N;i++){
            rev[i]=(rev[i>>1]|(i&1)<<L)>>1;
        }
        lastRev=N;
    }
    conv1(a,na,b,nb,c1);
    conv2(a,na,b,nb,c2);
    int n=na+nb-1;
    for(int i=0;i<n;i++){
        u32 x=c1[i],y=c2[i];
        u32 z=y+mod2-x;
        if(z>=mod2)z-=mod2;
        u32 k=(u64)z*inv_mod1%mod2;
        c[i]=(u64)x+(u64)mod1*k;
    }
}
static char io_buf[(NUMLEN<<1)+10],tmp[20];
static u32 A[MAX],B[MAX],C[MAX];
int main(){
    int len=syscall(0,0,io_buf,(NUMLEN<<1)+3),mid=0,idx=0,p,i,j,l;
    io_buf[len]=0;
    while(io_buf[mid]>' ')mid++;
    for(p=mid;io_buf[p]&&io_buf[p]<=' ';p++);
    for(len=p;io_buf[len]>' ';len++);
    int la=mid,lb=len-p;
    int na=(la+DIG-1)/DIG,nb=(lb+DIG-1)/DIG;
    for(i=la;i>0;i-=DIG){
        l=max(0,i-DIG);
        u32 r=0;
        for(j=l;j<i;j++)r=r*10+(io_buf[j]-'0');
        A[idx++]=r;
    }
    idx=0;
    for(i=len;i>p;i-=DIG){
        l=max(p,i-DIG);
        u32 r=0;
        for(j=l;j<i;j++)r=r*10+(io_buf[j]-'0');
        B[idx++]=r;
    }
    if((na==1&&A[0]==0)||(nb==1&&B[0]==0)){
        syscall(1,1,(char*)"0",1);
        return 0;
    }
    int nc=na+nb-1;
    u64 carry=0;
    conv(A,na,B,nb,C);
    for(i=0;i<nc;++i){
        u64 x=C[i]+carry;
        C[i]=(u64)x%BASE;
        carry=x/BASE;
    }
    while(carry)C[nc++]=carry%BASE,carry/=BASE;
    while(nc>1&&C[nc-1]==0)nc--;
    int pos=0,t=0;
    ll x=C[nc-1];
    while(x)tmp[t++]=x%10+'0',x/=10;
    if(t==0)tmp[t++]='0';
    while(t--)io_buf[pos++]=tmp[t];
    for(i=nc-2;i>=0;--i){
        x=C[i];
        for(j=0;j<DIG;j++)tmp[j]=x%10+'0',x/=10;
        for(j=DIG-1;j>=0;j--)io_buf[pos++]=tmp[j];
    }
    syscall(1,1,io_buf,pos);
}

// TODO : init_root func to add Barret Reduction(only include divl in asm)
// switch syscall input to mmap
// 119<<23+1 —> 7<<26+1 = 469762049 (much biggger, smaller), 5<<25+1=16772161
// if can, switch 7<<60+1(u64)
/* find the safe DIG. this can save mod1*mod2=1002772198720536577 : 1e18, max mul can (BASE-1)^2=> 
6 then len:12 under, overflow u32
7:14under, 8:16under
9  then len:18 under, maximum
*/
