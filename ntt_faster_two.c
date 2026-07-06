#define DEBUG 1
#define BARRET 0
#define SIMD 0
#define NTT_SIMD 1
#ifdef __cplusplus
#define const constexpr
#endif
#if SIMD==1
#include<stdalign.h>
#endif
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
static const u64 mu1=(u64)(((u128)1<<64)/mod1);
static const u64 mu2=(u64)(((u128)1<<64)/mod2);
static const u32 mu1_lo=(u32)mu1,mu1_hi=(u32)(mu1>>32);
static const u32 mu2_lo=(u32)mu2,mu2_hi=(u32)(mu2>>32);
#endif
#if SIMD==1
alignas(32)
#endif
static u32 root[MAX>>1],rev[MAX],lastRev,a[MAX],b[MAX],c1[MAX],c2[MAX],A[MAX],B[MAX],C[MAX],twid1[MAX>>1],twid2[MAX>>1];
static char io_buf[(NUMLEN<<1)+5],tmp[20];
u32 na,nb;
#if SIMD==1
static inline void parse_blocks_simd(char*buf,int hi,int lo,u32*out,int*idx){
    const __m256i ten=_mm256_set1_epi32(10);
    const __m256i mask255=_mm256_set1_epi32(0xFF);
    const __m256i zero48=_mm256_set1_epi32('0');
    int i=hi;
    while(i-8*DIG>=lo){
        __m256i acc=_mm256_setzero_si256();
        for(int d=0;d<DIG;d++){
            __m256i vidx=_mm256_setr_epi32(
                i-1*DIG+d,i-2*DIG+d,i-3*DIG+d,i-4*DIG+d,
                i-5*DIG+d,i-6*DIG+d,i-7*DIG+d,i-8*DIG+d);
            __m256i raw=_mm256_i32gather_epi32((const int*)buf,vidx,1);
            __m256i dig=_mm256_sub_epi32(_mm256_and_si256(raw,mask255),zero48);
            acc=_mm256_add_epi32(_mm256_mullo_epi32(acc,ten),dig);
        }
        u32 r[8];
        _mm256_storeu_si256((__m256i*)r,acc);
        for(int b=0;b<8;b++)out[(*idx)++]=r[b];
        i-=8*DIG;
    }
    for(;i>lo;i-=DIG){
        int l=max(lo,i-DIG);
        u32 r=0;
        for(int j=l;j<i;j++)r=r*10+(buf[j]-'0');
        out[(*idx)++]=r;
    }
}
#if BARRET==1
static __attribute__((always_inline))__inline __m128i mulmod_simd_epu32x4(__m128i av,__m128i bv,u32 mod,u32 mu_lo,u32 mu_hi){
    __m256i awide=_mm256_cvtepu32_epi64(av);
    __m256i bwide=_mm256_cvtepu32_epi64(bv);
    __m256i prod=_mm256_mul_epu32(awide,bwide);
    __m256i mask32=_mm256_set1_epi64x(0xFFFFFFFFULL);
    __m256i xl=_mm256_and_si256(prod,mask32);
    __m256i xh=_mm256_srli_epi64(prod,32);
    __m256i ylv=_mm256_set1_epi64x(mu_lo),yhv=_mm256_set1_epi64x(mu_hi);
    __m256i p0=_mm256_mul_epu32(xl,ylv);
    __m256i p1=_mm256_mul_epu32(xl,yhv);
    __m256i p2=_mm256_mul_epu32(xh,ylv);
    __m256i p3=_mm256_mul_epu32(xh,yhv);
    __m256i mid=_mm256_add_epi64(_mm256_add_epi64(p1,p2),_mm256_srli_epi64(p0,32));
    __m256i q=_mm256_add_epi64(p3,_mm256_srli_epi64(mid,32));
    __m256i modvec=_mm256_set1_epi64x(mod);
    __m256i qmod=_mm256_mul_epu32(q,modvec);
    __m256i r=_mm256_sub_epi64(prod,qmod);
    for(int rep=0;rep<3;rep++){
        __m256i ge=_mm256_cmpgt_epi64(r,_mm256_sub_epi64(modvec,_mm256_set1_epi64x(1)));
        r=_mm256_sub_epi64(r,_mm256_and_si256(ge,modvec));
    }
    const __m256i shuf_idx=_mm256_setr_epi32(0,2,4,6,1,3,5,7);
    __m256i packed=_mm256_permutevar8x32_epi32(r,shuf_idx);
    return _mm256_castsi256_si128(packed);
}
#define MULMOD1(av,bv)mulmod_simd_epu32x4(av,bv,mod1,mu1_lo,mu1_hi)
#define MULMOD2(av,bv)mulmod_simd_epu32x4(av,bv,mod2,mu2_lo,mu2_hi)
#else
static __attribute__((always_inline))inline __m128i mulmod_epu32x4(__m128i av,__m128i bv,u32 mod){
    const __m256d inv_modd=_mm256_set1_pd(1.0/(double)mod);
    const __m256i modvec64=_mm256_set1_epi64x(mod);
    __m256d da=_mm256_cvtepi32_pd(av);
    __m256d db=_mm256_cvtepi32_pd(bv);
    __m256d prod=_mm256_mul_pd(da,db);
    __m256d q_d=_mm256_floor_pd(_mm256_mul_pd(prod,inv_modd));
    __m128i q=_mm256_cvttpd_epi32(q_d);
    __m256i awide=_mm256_cvtepu32_epi64(av);
    __m256i bwide=_mm256_cvtepu32_epi64(bv);
    __m256i prod_exact=_mm256_mul_epu32(awide,bwide);
    __m256i qwide=_mm256_cvtepu32_epi64(q);
    __m256i qmod=_mm256_mul_epu32(qwide,modvec64);
    __m256i r=_mm256_sub_epi64(prod_exact,qmod);
    for(int rep=0;rep<2;rep++){
        __m256i ge=_mm256_cmpgt_epi64(r,_mm256_sub_epi64(modvec64,_mm256_set1_epi64x(1)));
        r=_mm256_sub_epi64(r,_mm256_and_si256(ge,modvec64));
    }
    for(int rep=0;rep<2;rep++){
        __m256i lt0=_mm256_cmpgt_epi64(_mm256_setzero_si256(),r);
        r=_mm256_add_epi64(r,_mm256_and_si256(lt0,modvec64));
    }
    const __m256i shuf_idx=_mm256_setr_epi32(0,2,4,6,1,3,5,7);
    __m256i packed=_mm256_permutevar8x32_epi32(r,shuf_idx);
    return _mm256_castsi256_si128(packed);
}
#define MULMOD1(av,bv)mulmod_epu32x4(av,bv,mod1)
#define MULMOD2(av,bv)mulmod_epu32x4(av,bv,mod2)
#endif
static inline __m256i div10_epu32(__m256i x,__m256i*rem){
    const __m256i magic=_mm256_set1_epi32((int)0xCCCCCCCD);
    const __m256i ten=_mm256_set1_epi32(10);
    __m256i odd=_mm256_srli_epi64(x,32);
    __m256i pe=_mm256_srli_epi64(_mm256_mul_epu32(x,magic),35);
    __m256i po=_mm256_srli_epi64(_mm256_mul_epu32(odd,magic),35);
    __m256i q=_mm256_or_si256(pe,_mm256_slli_epi64(po,32));
    *rem=_mm256_sub_epi32(x,_mm256_mullo_epi32(q,ten));
    return q;
}
#endif
static __attribute__((always_inline))inline u32 powmod1(u32 n,u32 e){
    u32 ret=1;
    while(e){
        if(e&1)ret=(u64)ret*n%mod1;
        e>>=1;
        n=(u64)n*n%mod1;
    }
    return ret;
}
static __attribute__((always_inline))inline u32 powmod2(u32 n,u32 e){
    u32 ret=1;
    while(e){
        if(e&1)ret=(u64)ret*n%mod2;
        e>>=1;
        n=(u64)n*n%mod2;
    }
    return ret;
}
static __attribute__((always_inline))inline void init_root1(int n,unsigned char inv){
    u32 ang=powmod1(w1,(mod1-1)/n);
    if(inv)ang=powmod1(ang,mod1-2);
    root[0]=1;
    for(int i=1;i<(n>>1);i++){
        root[i]=(u64)root[i-1]*ang%mod1;
    }
}
static __attribute__((always_inline))inline void init_root2(int n,unsigned char inv){
    u32 ang=powmod2(w2,(mod2-1)/n);
    if(inv)ang=powmod2(ang,mod2-2);
    root[0]=1;
    for(int i=1;i<(n>>1);i++){
        root[i]=(u64)root[i-1]*ang%mod2;
    }
}
static inline void ntt1(u32*f,int n){
    #pragma omp parallel for
    for(int i=1;i<n-1;i+=2){
        if(rev[i]<i){
            u32 tmp=f[i];
            f[i]=f[rev[i]];
            f[rev[i]]=tmp;
        }
        if(rev[i+1]<i+1){
            u32 tmp=f[i+1];
            f[i+1]=f[rev[i+1]];
            f[rev[i+1]]=tmp;
        }
    }
    int step=n>>1;
    for(int len=2;len<=n;len<<=1){
        int half=len>>1;
#if SIMD==1&&NTT_SIMD==1
        for(int k=0;k<half;k++)twid1[k]=root[step*k];
#endif
        for(int j=0;j<n;j+=len){
            int k=0;
#if SIMD==1&&NTT_SIMD==1
            const __m128i modvec=_mm_set1_epi32(mod1);
            const __m128i one=_mm_set1_epi32(1);
            for(;k+4<=half;k+=4){
                __m128i rootv=_mm_load_si128((__m128i*)&twid1[k]);
                __m128i uvec=_mm_load_si128((__m128i*)&f[j+k]);
                __m128i vraw=_mm_load_si128((__m128i*)&f[j+k+half]);
                __m128i vvec=MULMOD1(vraw,rootv);
                __m128i sum=_mm_add_epi32(uvec,vvec);
                __m128i ge1=_mm_cmpgt_epi32(sum,_mm_sub_epi32(modvec,one));
                sum=_mm_sub_epi32(sum,_mm_and_si128(ge1,modvec));
                __m128i diff=_mm_add_epi32(_mm_sub_epi32(uvec,vvec),modvec);
                __m128i ge2=_mm_cmpgt_epi32(diff,_mm_sub_epi32(modvec,one));
                diff=_mm_sub_epi32(diff,_mm_and_si128(ge2,modvec));
                _mm_store_si128((__m128i*)&f[j+k],sum);
                _mm_store_si128((__m128i*)&f[j+k+half],diff);
            }
#endif
            for(;k<half;k++){
                u32 u=f[j+k],v=(u64)f[j+k+half]*root[step*k]%mod1;
                u+=v;
                v=(u64)u+mod1-((u64)v<<1);
                if(u>=mod1)u-=mod1;
                if(v>=mod1)v-=mod1;
                f[j+k]=u;
                f[j+k+half]=v;
            }
        }
        step>>=1;
    }
}
static inline void ntt2(u32*f,int n){
    #pragma omp parallel for
    for(int i=1;i<n-1;i+=2){
        if(rev[i]<i){
            u32 tmp=f[i];
            f[i]=f[rev[i]];
            f[rev[i]]=tmp;
        }
        if(rev[i+1]<i+1){
            u32 tmp=f[i+1];
            f[i+1]=f[rev[i+1]];
            f[rev[i+1]]=tmp;
        }
    }
    int step=n>>1;
    for(int len=2;len<=n;len<<=1){
        int half=len>>1;
#if SIMD==1&&NTT_SIMD==1
        for(int k=0;k<half;k++)twid2[k]=root[step*k];
#endif
        for(int j=0;j<n;j+=len){
            int k=0;
#if SIMD==1&&NTT_SIMD==1
            const __m128i modvec=_mm_set1_epi32(mod2);
            const __m128i one=_mm_set1_epi32(1);
            for(;k+4<=half;k+=4){
                __m128i rootv=_mm_load_si128((__m128i*)&twid2[k]);
                __m128i uvec=_mm_load_si128((__m128i*)&f[j+k]);
                __m128i vraw=_mm_load_si128((__m128i*)&f[j+k+half]);
                __m128i vvec=MULMOD2(vraw,rootv);
                __m128i sum=_mm_add_epi32(uvec,vvec);
                __m128i ge1=_mm_cmpgt_epi32(sum,_mm_sub_epi32(modvec,one));
                sum=_mm_sub_epi32(sum,_mm_and_si128(ge1,modvec));
                __m128i diff=_mm_add_epi32(_mm_sub_epi32(uvec,vvec),modvec);
                __m128i ge2=_mm_cmpgt_epi32(diff,_mm_sub_epi32(modvec,one));
                diff=_mm_sub_epi32(diff,_mm_and_si128(ge2,modvec));
                _mm_store_si128((__m128i*)&f[j+k],sum);
                _mm_store_si128((__m128i*)&f[j+k+half],diff);
            }
#endif
            for(;k<half;k++){
                u32 u=f[j+k],v=(u64)f[j+k+half]*root[step*k]%mod2;
                u+=v;
                v=(u64)u+mod2-((u64)v<<1);
                if(u>=mod2)u-=mod2;
                if(v>=mod2)v-=mod2;
                f[j+k]=u;
                f[j+k+half]=v;
            }
        }
        step>>=1;
    }
}
static inline void conv1(u32*c,int n){
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
#if SIMD==1
    int i=0;
    for(;i+4<=n;i+=4){
        __m128i av=_mm_loadu_si128((__m128i*)&a[i]);
        __m128i bv=_mm_loadu_si128((__m128i*)&b[i]);
        __m128i cv=MULMOD1(av,bv);
        _mm_storeu_si128((__m128i*)&c[i],cv);
    }
    for(;i<n;i++)c[i]=(u64)a[i]*b[i]%mod1;
#else
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod1;
    }
#endif
    init_root1(n,1);
    ntt1(c,n);
    u32 t=powmod1(n,mod1-2);
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)c[i]*t%mod1;
    }
}
static inline void conv2(u32*c,int n){
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
#if SIMD==1
    int i=0;
    for(;i+4<=n;i+=4){
        __m128i av=_mm_loadu_si128((__m128i*)&a[i]);
        __m128i bv=_mm_loadu_si128((__m128i*)&b[i]);
        __m128i cv=MULMOD2(av,bv);
        _mm_storeu_si128((__m128i*)&c[i],cv);
    }
    for(;i<n;i++)c[i]=(u64)a[i]*b[i]%mod2;
#else
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod2;
    }
#endif
    init_root2(n,1);
    ntt2(c,n);
    u32 t=powmod2(n,mod2-2);
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        c[i]=(u64)c[i]*t%mod2;
    }
}
int main(){
    debug("SMID=%d,NTT_SIMD=%d,BARRET=%d\n",SIMD,NTT_SIMD,BARRET);
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
#if SIMD==1
    idx=0;
    parse_blocks_simd(buf,la,0,A,&idx);
    idx=0;
    parse_blocks_simd(buf,len,p,B,&idx);
#else
    for(i=la;i>0;i-=DIG){
        l=max(0,i-DIG);r=0;
        for(j=l;j<i;j++)r=r*10+(buf[j]-'0');
        A[idx++]=r;
    }
    idx=0;
    for(i=len;i>p;i-=DIG){
        l=max(p,i-DIG);r=0;
        for(j=l;j<i;j++)r=r*10+(buf[j]-'0');
        B[idx++]=r;
    }
#endif
#if DEBUG==1
    debug("A=");
    for(i=0;i<na;i++)debug("%d ",A[i]);
    debug("\nB=");
    for(i=0;i<nb;i++)debug("%d ",B[i]);
    debug("\n");
#endif
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
#if DEBUG==1
        debug("c1=");
        for(i=0;i<na;i++)debug("%d ",c1[i]);
        debug("\nc2=");
        for(i=0;i<nb;i++)debug("%d ",c2[i]);
        debug("\n");
#endif
        debug("C=");
        for(int i=0;i<nc;i++){
            u32 x=c1[i],y=c2[i];
            u32 z=y+mod2-x;
            if(z>=mod2)z-=mod2;
            u32 k=(u64)z*inv_mod1%mod2;
            carry+=(u64)x+(u64)mod1*k;
            C[i]=carry%BASE;
            carry/=BASE;
            debug("%d ",C[i]);
        }
        while(carry)
            C[nc++]=carry%BASE,carry/=BASE,debug("%d ",C[nc-1]);
        debug("\n");
    }
    while(nc>1&&C[nc-1]==0)nc--;
#if SIMD==1
    idx=0;
    i=nc-1;
    for(;i-7>=0;i-=8){
        __m256i x=_mm256_loadu_si256((__m256i*)&C[i-7]);
        u32 digits[8][DIG];
        for(j=DIG-1;j>=0;j--){
            __m256i rem;
            x=div10_epu32(x,&rem);
            u32 r[8];
            _mm256_storeu_si256((__m256i*)r,rem);
            for(int b=0;b<8;b++)digits[b][j]=r[b]|48;
        }
        for(int b=7;b>=0;b--)
            for(j=0;j<DIG;j++)
                io_buf[idx++]=digits[b][j];
    }
    for(;i>=0;--i){
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