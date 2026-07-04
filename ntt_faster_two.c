#define DEBUG 0
#define BARRET 0 // useless
#define SMID 1
//설정
#ifndef _MEMORY_H
#define _MEMORY_H
#include<string.h>
#endif
#include<stdio.h>
#include<immintrin.h>
#include<unistd.h>
#if DEBUG==1
#define debug(format,args...)fprintf(stderr,format,##args)
#else
#define debug(format,args...)
#endif
#ifdef __linux__    
#include<sys/mman.h>
#include<sys/stat.h>
#define fwrite(buf,a,pos,std)write(1,buf,pos)
#define fwrite(buf,a,pos,std)write(1,buf,pos)
#endif 
#include<omp.h>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx,avx2,fma")
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
u32 w1=3,w2=3,mod1=998244353,mod2=1004535809,inv_mod1=669690699,k=93;
#if BARRET==1
#ifdef __cplusplus
constexpr
#else
const
#endif
u64 x1=((u128(1)<<k)+mod1-1)/mod1,x2=((u128(1)<<k)+mod2-1)/mod2;
#endif
static u32 root[MAX>>1],rev[MAX],lastRev,a[MAX],b[MAX],c1[MAX],c2[MAX],A[MAX],B[MAX],na,nb;
static u32 C[MAX];
static char io_buf[(NUMLEN<<1)+5],tmp[20];
#if BARRET==1
static __inline u32 modular1(u128 x){
    u32 ret=((u128)x*x1)>>k;
    return x-ret*mod1;
}
static __inline u32 modular2(u128 x){
    u32 ret=((u128)x*x2)>>k;
    return x-ret*mod2;
}
#endif
#ifdef SMID
static __inline void parse_blocks_simd(char*buf,int hi,int lo,u32*out,int*idx){
    const __m256i ten=_mm256_set1_epi32(10);
    const __m256i mask255=_mm256_set1_epi32(0xFF);
    const __m256i zero48=_mm256_set1_epi32('0');
    int i=hi;
    while(i-8*DIG>=lo){
        __m256i acc=_mm256_setzero_si256();
        for(int d=0;d<DIG;d++){
            // block b(0..7)의 d번째 자리 바이트 offset = i-(b+1)*DIG+d
            __m256i vidx=_mm256_setr_epi32(
                i-1*DIG+d,i-2*DIG+d,i-3*DIG+d,i-4*DIG+d,
                i-5*DIG+d,i-6*DIG+d,i-7*DIG+d,i-8*DIG+d);
            __m256i raw=_mm256_i32gather_epi32((const int*)buf,vidx,1);
            __m256i dig=_mm256_sub_epi32(_mm256_and_si256(raw,mask255),zero48);
            acc=_mm256_add_epi32(_mm256_mullo_epi32(acc,ten),dig);
        }
        u32 r[8];
        _mm256_storeu_si256((__m256i*)r,acc);
        for(int b=0;b<8;b++) out[(*idx)++]=r[b];
        i-=8*DIG;
    }
    for(;i>lo;i-=DIG){                    // 8블록 미만 나머지는 스칼라
        int l=max(lo,i-DIG);
        u32 r=0;
        for(int j=l;j<i;j++)r=r*10+(buf[j]-'0');
        out[(*idx)++]=r;
    }
}
static __inline __m256i div10_epu32(__m256i x,__m256i*rem){
    const __m256i magic=_mm256_set1_epi32((int)0xCCCCCCCD);
    const __m256i ten=_mm256_set1_epi32(10);
    __m256i odd=_mm256_srli_epi64(x,32);                     // 홀수 레인을 각 64비트 하위로 이동
    __m256i pe=_mm256_srli_epi64(_mm256_mul_epu32(x,magic),35);   // 짝수 레인(0,2,4,6) 몫
    __m256i po=_mm256_srli_epi64(_mm256_mul_epu32(odd,magic),35); // 홀수 레인(1,3,5,7) 몫
    __m256i q=_mm256_or_si256(pe,_mm256_slli_epi64(po,32));  // 다시 원래 레인 위치로 병합
    *rem=_mm256_sub_epi32(x,_mm256_mullo_epi32(q,ten));      // x - q*10
    return q;
}
#endif
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
    #pragma omp parallel for
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
    #pragma omp parallel for
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
    memcpy(a,A,na*sizeof(u32));
    memset(a+na,0,(n-na)*sizeof(u32));
    memcpy(b,B,nb*sizeof(u32));
    memset(b+nb,0,(n-nb)*sizeof(u32));
#else
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        a[i]=(i<na)?A[i]:0;
        b[i]=(i<nb)?B[i]:0;
    }
#endif
    ntt1(a,n);
    ntt1(b,n);
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod1;
    }
    init_root1(n,1);
    ntt1(c,n);
    u32 t=powmod1(n,mod1-2);
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)c[i]*t%mod1;
    }
}
static __inline void conv2(u32*c,int n){
    init_root2(n,0);
#ifdef _MEMORY_H
    memcpy(a,A,na*sizeof(u32));
    memset(a+na,0,(n-na)*sizeof(u32));
    memcpy(b,B,nb*sizeof(u32));
    memset(b+nb,0,(n-nb)*sizeof(u32));
#else
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        a[i]=(i<na)?A[i]:0;
        b[i]=(i<nb)?B[i]:0;
    }
#endif
    ntt2(a,n);
    ntt2(b,n);
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod2;
    }
    init_root2(n,1);
    ntt2(c,n);
    u32 t=powmod2(n,mod2-2);
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)c[i]*t%mod2;
    }
}
int main(){
    int mid=0,idx=0,p,r,i,j,l,len;
#ifdef __linux__
    struct stat st;
    fstat(0,&st);
    len=(int)st.st_size;
    char*buf=(char*)mmap(NULL,len,PROT_READ,MAP_SHARED,0,0);
#else
    len=fread(io_buf,1,(NUMLEN<<1)+3,stdin);
    io_buf[len]=0;
    char*buf=io_buf;
#endif
    while(len&&!(buf[len-1]&16))len--;
    while(buf[mid]&16)mid++;
    for(p=mid;buf[p]&&!(buf[p]&16);p++);
    for(len=p;buf[len]&16;len++);
    int la=mid,lb=len-p;
    na=(la+DIG-1)/DIG,nb=(lb+DIG-1)/DIG;
    idx=0;
    parse_blocks_simd(buf,la,0,A,&idx);
    idx=0;
    parse_blocks_simd(buf,len,p,B,&idx);
    if((na==1&&A[0]==0)||(nb==1&&B[0]==0)){
        fwrite((char*)"0",1,1,stdout);
        return 0;
    }
    int nc=na+nb-1;
    u64 carry=0;
    {
        int clz=__builtin_clz(nc-1);
        int e=32-clz,N=1<<e;
        if(lastRev!=N){
            for(int i=1;i<N;i++){
                rev[i]=(rev[i>>1]|(i&1)<<e)>>1;
            }
            lastRev=N;
        }
        conv1(c1,N);
        conv2(c2,N);
        for(int i=0;i<nc;i++){
            u32 x=c1[i],y=c2[i];
            u32 z=y+mod2-x;
            if(z>=mod2)z-=mod2;
            u32 k=(u64)z*inv_mod1%mod2;
            carry+=(u64)x+(u64)mod1*k;
            C[i]=carry%BASE;
            carry/=BASE;
        }
        while(carry)
            C[nc++]=carry%BASE,carry/=BASE;
    }
    while(nc>1&&C[nc-1]==0)nc--;
#if SMID==1
    idx=0;
    i=nc-1;
    for(; i-7>=0;i-=8){
        __m256i x=_mm256_loadu_si256((__m256i*)&C[i-7]); // 레인0..7 = C[i-7..i] (오름차순)
        u32 digits[8][DIG];
        //unroll-loops
        for(j=DIG-1;j>=0;j--){
            __m256i rem;
            x=div10_epu32(x,&rem);
            u32 r[8];
            _mm256_storeu_si256((__m256i*)r,rem);
            //unroll-loops
            for(int b=0;b<8;b++) digits[b][j]=r[b]|48;
        }
        //unroll-loops
        for(int b=7;b>=0;b--)          // C[i]가 가장 상위이므로 b=7부터 역순으로 출력
            for(j=0;j<DIG;j++)
                io_buf[idx++]=digits[b][j];
    }
    for(; i>=0; --i){                  // 8개 미만 남은 나머지는 기존 스칼라 방식
        u32 x=C[i];
        for(j=DIG-1;j>=0;j--){
            io_buf[idx+j]=(x%10)|48;
            x/=10;
        }
        idx+=DIG;
    }
    int start=0;
    while(io_buf[start]=='0' && start<idx-1) start++;
    fwrite(io_buf+start,1,idx-start,stdout);
#else
    idx=0;
    for(i=nc-1;i>=0;--i){
        u32 x=C[i];
        for(j=DIG-1;j>=0;j--){
            io_buf[idx+j]=(x%10)|48;
            x/=10;
        }
        idx+=DIG;
    }
    int start=0;
    while(io_buf[start]=='0'&&start<idx-1)start++;
    fwrite(io_buf+start,1,idx-start,stdout);
#endif
#ifdef __linux__
    munmap(buf,st.st_size);
#endif
}