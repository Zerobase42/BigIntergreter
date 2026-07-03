#define DEBUG

#ifndef _MEMORY_H
#define _MEMORY_H
#include<string.h>  //memset,memcpy
#endif
#include<stdio.h>
#include<immintrin.h>// smid
#include<unistd.h>// read,write
#ifdef DEBUG
#define debug(format,args...)fprintf(stderr,format,##args)
#else
#define debug(format,args...)
#endif
#ifdef __linux__
#include<sys/mman.h>  // mmap
#define fread(buf,a,pos,std)read(0,buf,pos)
#define fwrite(buf,a,pos,std)write(1,buf,pos)
#endif 
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,sse4,avx,avx2,fma")
#define u32 unsigned int
#define u64 unsigned long long
#define u128 __uint128_t
#define max(a,b)((a)>(b)?(a):(b))
#define DIG 6
#define NUMLEN 1000000
#define BASE 1000000 
#define MAX 1048576  
#ifdef __cplusplus
constexpr
#else
const
#endif
u32 w1=3,w2=3,mod1=998244353,mod2=1004535809,inv_mod1=669690699,k=62;
#ifdef __cplusplus
constexpr
#else
const
#endif
u64 x1=4619796751,x2=4590862742;// x = ceil((1<<k)/mod)(k=62)
static u32 root[MAX>>1],rev[MAX],lastRev;
static u32 a[MAX],b[MAX],c1[MAX],c2[MAX];
static char io_buf[(NUMLEN<<1)+5],tmp[20];
static u32 A[MAX],B[MAX],na,nb;
static u64 C[MAX];
static __inline u32 powmod1(u32 n,u32 e){
    u32 ret=1;
    while(e){
        if(e&1)ret=(u64)ret*n%mod1;
        e>>=1;
        n=(u64)n*n%mod1;
    }
    return ret;
}
static __inline u32 powmod2(u32 n,u32 e){
    u32 ret=1;
    while(e){
        if(e&1)ret=(u64)ret*n%mod2;
        e>>=1;
        n=(u64)n*n%mod2;
    }
    return ret;
}
static __inline void init_root1(int n,unsigned char inv){
    u32 ang=powmod1(w1,(mod1-1)/n);
    if(inv)ang=powmod1(ang,mod1-2);
    root[0]=1;
    for(int i=1;i<(n>>1);i++){
        root[i]=(u64)root[i-1]*ang%mod1;
    }
}
static __inline void init_root2(int n,unsigned char inv){
    u32 ang=powmod2(w2,(mod2-1)/n);
    if(inv)ang=powmod2(ang,mod2-2);
    root[0]=1;
    for(int i=1;i<(n>>1);i++){
        root[i]=(u64)root[i-1]*ang%mod2;
    }
}
static __inline void ntt1(u32*f,int n){
    for(int i=1;i<n-1;i++){
        if(rev[i]<i){
            u32 tmp=f[i];
            f[i]=f[rev[i]];
            f[rev[i]]=tmp;
        }
    }
    int step=n>>1;
    for(int len=2;len<=n;len<<=1){
        for(int j=0;j<n;j+=len){
            for(int k=0;k<(len>>1);k++){
                u32 u=f[j+k],v=(u64)f[j+k+(len>>1)]*root[step*k]%mod1;
                u+=v;
                v=(u64)u+mod1-((u64)v<<1);
                if(u>=mod1)u-=mod1;
                if(v>=mod1)v-=mod1;
                f[j+k]=u;
                f[j+k+(len>>1)]=v;
            }
        }
        step>>=1;
    }
}
static __inline void ntt2(u32*f,int n){
    for(int i=1;i<n-1;i++){
        if(rev[i]<i){
            u32 tmp=f[i];
            f[i]=f[rev[i]];
            f[rev[i]]=tmp;
        }
    }
    int step=n>>1;
    for(int len=2;len<=n;len<<=1){
        for(int j=0;j<n;j+=len){
            for(int k=0;k<(len>>1);k++){
                u32 u=f[j+k],v=(u64)f[j+k+(len>>1)]*root[step*k]%mod2;
                u+=v;
                v=(u64)u+mod2-(v<<1);
                if(u>=mod2)u-=mod2;
                if(v>=mod2)v-=mod2;
                f[j+k]=u;
                f[j+k+(len>>1)]=v;
            }
        }
        step>>=1;
    }
}
static __inline void conv1(u32*c,int n){
    init_root1(n,0);
#ifdef _MEMORY_H
    //memset(a,0,n*sizeof(u32));
    //memset(b,0,n*sizeof(u32));
    memcpy(a,A,na*sizeof(u32));
    memcpy(b,B,nb*sizeof(u32));
#else
    for(int i=0;i<n;i++){
        a[i]=(i<na)?A[i]:0;
        b[i]=(i<nb)?B[i]:0;
    }
#endif
    ntt1(a,n);
    ntt1(b,n);
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod1;
    }
    init_root1(n,1);
    ntt1(c,n);
    u32 t=powmod1(n,mod1-2);
    for(int i=0;i<n;i++){
        c[i]=(u64)c[i]*t%mod1;
    }
}
static __inline void conv2(u32*c,int n){
    init_root2(n,0);
#ifdef _MEMORY_H
    memset(a,0,n*sizeof(u32));
    memset(b,0,n*sizeof(u32));
    memcpy(a,A,na*sizeof(u32));
    memcpy(b,B,nb*sizeof(u32));
#else
    for(int i=0;i<n;i++){
        a[i]=(i<na)?A[i]:0;
        b[i]=(i<nb)?B[i]:0;
    }
#endif
    ntt2(a,n);
    ntt2(b,n);
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod2;
    }
    init_root2(n,1);
    ntt2(c,n);
    u32 t=powmod2(n,mod2-2);
    for(int i=0;i<n;i++){
        c[i]=(u64)c[i]*t%mod2;
    }
}
static __inline void conv(u64*c){
    int n=na+nb-1,clz=__builtin_clz(n-1);
    int e=32-clz,N=1<<e;
    if(lastRev!=N){
        int L=e;
        for(int i=1;i<N;i++){
            rev[i]=(rev[i>>1]|(i&1)<<L)>>1;
        }
        lastRev=N;
    }
    conv1(c1,N);
    conv2(c2,N);
    for(int i=0;i<n;i++){
        u32 x=c1[i],y=c2[i];
        u32 z=y+mod2-x;
        if(z>=mod2)z-=mod2;
        u32 k=(u64)z*inv_mod1%mod2;
        c[i]=(u64)x+(u64)mod1*k;
    }
}
int main(){
    int len=fread(io_buf,1,(NUMLEN<<1)+3,stdin),mid=0,idx=0,p,r,i,j,l;
    while (len &&!(io_buf[len-1]&16))len--;
    io_buf[len]=0;
    while(io_buf[mid]&16)mid++;
    for(p=mid;io_buf[p]&&!(io_buf[p]&16);p++);
    for(len=p;io_buf[len]&16;len++);
    int la=mid,lb=len-p;
    na=(la+DIG-1)/DIG,nb=(lb+DIG-1)/DIG;
    for(i=la;i>0;i-=DIG){
        l=max(0,i-DIG);
        r=0;
        for(j=l;j<i;j++)r=r*10+(io_buf[j]-'0');
        A[idx++]=r;
    }
    idx=0;
    for(i=len;i>p;i-=DIG){
        l=max(p,i-DIG);
        r=0;
        for(j=l;j<i;j++)r=r*10+(io_buf[j]-'0');
        B[idx++]=r;
    }
    if((na==1&&A[0]==0)||(nb==1&&B[0]==0)){
        fwrite((char*)"0",1,1,stdout);
        return 0;
    }
    int nc=na+nb-1;
    u64 carry=0;
    conv(C);
    for(i=0;i<nc;++i){
        carry+=C[i];
        C[i]=carry%BASE;
        carry/=BASE;
    }
    while(carry)C[nc++]=carry%BASE,carry/=BASE;
    while(nc>1&&C[nc-1]==0)nc--;
    int t=0;
    idx=0;
    u32 x=C[nc-1];
    while(x)tmp[t++]=(x%10)|48,x/=10;
    if(t==0)tmp[t++]='0';
    while(t--)io_buf[idx++]=tmp[t];
    for(i=nc-2;i>=0;--i){
        x=C[i];
        for(j=0;j<DIG;j++)
            tmp[j]=(x%10)|48,x/=10;
        for(j=DIG-1;j>=0;j--)
            io_buf[idx++]=tmp[j];
    }
    fwrite(io_buf,1,idx,stdout);
}