#ifdef _WIN32
#include<windows.h>
#else
#include<unistd.h>
#include<sys/mman.h>
#include<sys/stat.h>
#include<fcntl.h>
#endif
#include<stdlib.h>
#include<string.h>
#include<bit>
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,sse4,avx,avx2,fma")

#define u32 unsigned int
#define u64 unsigned long long
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
u32 w1=3,w2=3,mod1=998244353,mod2=1004535809,inv_mod1=669690699;
static u32 root[MAX>>1],rev[MAX],lastRev;
static u32 a[MAX],b[MAX],c1[MAX],c2[MAX];
static u32 A[MAX],B[MAX],C[MAX];

// ============ FAST I/O BUFFERS ============
static char io_read_buf[NUMLEN << 1];
static char io_write_buf[(NUMLEN << 1) + 10];
static char *pr = io_read_buf, *pw = io_write_buf;
static char tmp[20];

// Cross-platform mmap
static char *mmap_buf = NULL;
static size_t mmap_size = 0;

#ifdef _WIN32
static HANDLE hFile = INVALID_HANDLE_VALUE;
static HANDLE hMapping = INVALID_HANDLE_VALUE;

static int mmap_stdin() {
    hFile = GetStdHandle(STD_INPUT_HANDLE);
    if (hFile == INVALID_HANDLE_VALUE) return -1;
    LARGE_INTEGER fileSize;
    if (!GetFileSizeEx(hFile, &fileSize)) return -1;
    mmap_size = (size_t)fileSize.QuadPart;
    if (mmap_size == 0) return -1;
    hMapping = CreateFileMappingA(hFile, NULL, PAGE_READONLY, 
                                   fileSize.HighPart, fileSize.LowPart, NULL);
    if (hMapping == NULL) return -1;
    mmap_buf = (char *)MapViewOfFile(hMapping, FILE_MAP_READ, 0, 0, mmap_size);
    if (mmap_buf == NULL) return -1;
    pr = mmap_buf;
    return 0;
}

static void cleanup_mmap() {
    if (mmap_buf) UnmapViewOfFile(mmap_buf), mmap_buf = NULL;
    if (hMapping != INVALID_HANDLE_VALUE) CloseHandle(hMapping), hMapping = INVALID_HANDLE_VALUE;
}

static void flush_write() {
    DWORD bytesWritten;
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), io_write_buf, pw - io_write_buf, &bytesWritten, NULL);
    pw = io_write_buf;
}
#else
static int mmap_stdin() {
    struct stat sb;
    if (fstat(0, &sb) == -1) return -1;
    mmap_size = sb.st_size;
    if (mmap_size == 0) return -1;
    mmap_buf = (char *)mmap(NULL, mmap_size, PROT_READ, MAP_PRIVATE, 0, 0);
    if (mmap_buf == MAP_FAILED) return -1;
    madvise(mmap_buf, mmap_size, MADV_SEQUENTIAL);
    pr = mmap_buf;
    return 0;
}

static void cleanup_mmap() {
    if (mmap_buf && mmap_buf != MAP_FAILED) munmap(mmap_buf, mmap_size), mmap_buf = NULL;
}

static void flush_write() {
    write(1, io_write_buf, pw - io_write_buf);
    pw = io_write_buf;
}
#endif

// ============ FAST INPUT: 8-Byte Parsing ============
static __inline u32 read_int() {
    u32 ret = 0, flag = 0;
    if (*pr == '-') flag = 1, pr++;
    
    u64 x;
    memcpy(&x, pr, 8);
    x ^= 0x3030303030303030ULL;
    
    if (!(x & 0xf0f0f0f0f0f0f0f0ULL)) {
        x = ((x * 10) + (x >> 8)) & 0x00ff00ff00ff00ffULL;
        x = ((x * 100) + (x >> 16)) & 0x0000ffff0000ffffULL;
        x = ((x * 10000) + (x >> 32)) & 0x00000000ffffffffULL;
        ret = (u32)x;
        pr += 8;
    } else {
        while (*pr & 16) ret = 10 * ret + (*pr++ & 15);
    }
    
    pr++;
    return flag ? -ret : ret;
}

// ============ FAST OUTPUT: 4-Digit Lookup Table ============
constexpr auto lut_digit = [] {
    std::array<std::array<char, 4>, 10000> res;
    for (int i = 0; i < 10000; i++) {
        res[i][0] = (i / 1000) | 48;
        res[i][1] = (i / 100 % 10) | 48;
        res[i][2] = (i / 10 % 10) | 48;
        res[i][3] = (i % 10) | 48;
    }
    return res;
}();

constexpr auto lut_digit_trimmed = [] {
    std::array<std::array<char, 4>, 10000> res{};
    for (int i = 0; i < 10; i++) {
        res[i][0] = (i | 48);
    }
    for (int i = 10; i < 100; i++) {
        res[i][0] = (i / 10 | 48);
        res[i][1] = (i % 10 | 48);
    }
    for (int i = 100; i < 1000; i++) {
        res[i][0] = (i / 100 | 48);
        res[i][1] = (i / 10 % 10 | 48);
        res[i][2] = (i % 10 | 48);
    }
    for (int i = 1000; i < 10000; i++) {
        res[i][0] = (i / 1000 | 48);
        res[i][1] = (i / 100 % 10 | 48);
        res[i][2] = (i / 10 % 10 | 48);
        res[i][3] = (i % 10 | 48);
    }
    return res;
}();

constexpr auto lut_sz = [] {
    std::array<char, 10000> res;
    for (int i = 0; i < 10; i++) res[i] = 1;
    for (int i = 10; i < 100; i++) res[i] = 2;
    for (int i = 100; i < 1000; i++) res[i] = 3;
    for (int i = 1000; i < 10000; i++) res[i] = 4;
    return res;
}();

constexpr int p10[] = {0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};

static __inline int count_digit(u32 n) {
    int t = std::bit_width(n) * 1233 >> 12;
    return t - (n < p10[t]) + 1;
}

static __inline void write_4digit(u32 x) {
    if (pw - io_write_buf + 4 > (NUMLEN << 1)) flush_write();
    memcpy(pw, &lut_digit[x], 4);
    pw += 4;
}

static __inline void write_4digit_trimmed(u32 x) {
    if (pw - io_write_buf + 4 > (NUMLEN << 1)) flush_write();
    memcpy(pw, &lut_digit_trimmed[x], 4);
    pw += lut_sz[x];
}

static __inline void write_int(u64 x) {
    if (pw - io_write_buf + 40 > (NUMLEN << 1)) flush_write();
    
    if (x == 0) {
        *pw++ = '0';
        return;
    }
    
    if (x > 99999999) {
        write_4digit_trimmed(x / 100000000);
        x %= 100000000;
        write_4digit(x / 10000);
        write_4digit(x % 10000);
    } else if (x > 9999) {
        write_4digit_trimmed(x / 10000);
        write_4digit(x % 10000);
    } else {
        write_4digit_trimmed(x);
    }
}

// ============ NTT IMPLEMENTATION ============
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
    int step = n >> 1;
    for(int len=2;len<=n;len<<=1){
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
        step >>= 1;
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
    int step = n >> 1;
    for(int len=2;len<=n;len<<=1){
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
        step >>= 1;
    }
    if(inv){
        u32 t=powmod2(n,mod2-2);
        for(int i=0;i<n;i++){
            f[i]=(u64)f[i]*t%mod2;
        }
    }
}

static __inline void conv1(u32*_a,int na,u32*_b,int nb,u32*c,int n){
    init_root1(n,0);
    memset(a, 0, n * sizeof(u32));
    memset(b, 0, n * sizeof(u32));
    memcpy(a, _a, na * sizeof(u32));
    memcpy(b, _b, nb * sizeof(u32));
    ntt1(a,n,0);
    ntt1(b,n,0);
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod1;
    }
    init_root1(n,1);
    ntt1(c,n,1);
}

static __inline void conv2(u32*_a,int na,u32*_b,int nb,u32*c,int n){
    init_root2(n,0);
    memset(a, 0, n * sizeof(u32));
    memset(b, 0, n * sizeof(u32));
    memcpy(a, _a, na * sizeof(u32));
    memcpy(b, _b, nb * sizeof(u32));
    ntt2(a,n,0);
    ntt2(b,n,0);
    for(int i=0;i<n;i++){
        c[i]=(u64)a[i]*b[i]%mod2;
    }
    init_root2(n,1);
    ntt2(c,n,1);
}

static __inline void conv(u32*_a,int na,u32*_b,int nb,u32*c){
    int n=na+nb-1,clz=__builtin_clz(n-1);
    int N=1<<(32-clz);
    if(lastRev!=N){
        int L=clz-1;
        for(int i=1;i<N;i++){
            rev[i]=(rev[i>>1]|(i&1)<<L)>>1;
        }
        lastRev=N;
    }
    conv1(_a,na,_b,nb,c1,N);
    conv2(_a,na,_b,nb,c2,N);
    for(int i=0;i<n;i++){
        u32 x=c1[i],y=c2[i];
        u32 z=y+mod2-x;
        if(z>=mod2)z-=mod2;
        u32 k=(u64)z*inv_mod1%mod2;
        c[i]=(u64)x+(u64)mod1*k;
    }
}

int main(){
    if (mmap_stdin() != 0) {
        int len;
#ifdef _WIN32
        DWORD bytesRead = 0;
        ReadFile(GetStdHandle(STD_INPUT_HANDLE), io_read_buf, (NUMLEN << 1) + 3, &bytesRead, NULL);
        len = (int)bytesRead;
#else
        len = read(0, io_read_buf, (NUMLEN << 1) + 3);
#endif
        io_read_buf[len] = 0;
        pr = io_read_buf;
    }
    
    int na = 0, nb = 0;
    
    // Parse A
    while (*pr <= ' ') pr++;
    while (*pr > ' ') {
        u32 r = 0;
        for (int i = 0; i < DIG && *pr > ' '; i++) {
            r = r * 10 + (*pr++ & 15);
        }
        A[na++] = r;
    }
    
    // Parse B
    while (*pr <= ' ') pr++;
    while (*pr > ' ') {
        u32 r = 0;
        for (int i = 0; i < DIG && *pr > ' '; i++) {
            r = r * 10 + (*pr++ & 15);
        }
        B[nb++] = r;
    }
    
    if ((na == 1 && A[0] == 0) || (nb == 1 && B[0] == 0)) {
        write_int(0);
        flush_write();
        cleanup_mmap();
        return 0;
    }
    
    int nc = na + nb - 1;
    u64 carry = 0;
    conv(A, na, B, nb, C);
    
    for (int i = 0; i < nc; ++i) {
        carry += C[i];
        C[i] = carry % BASE;
        carry /= BASE;
    }
    while (carry) C[nc++] = carry % BASE, carry /= BASE;
    while (nc > 1 && C[nc - 1] == 0) nc--;
    
    for (int i = nc - 1; i >= 0; --i) {
        if (i == nc - 1) {
            write_int(C[i]);
        } else {
            write_4digit(C[i]);
        }
    }
    
    flush_write();
    cleanup_mmap();
    return 0;
}
