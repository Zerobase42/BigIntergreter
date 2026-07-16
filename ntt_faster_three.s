	.file	"ntt_faster_three.c"
	.text
	.globl	w1
	.section .rdata,"dr"
	.align 4
w1:
	.long	3
	.globl	w2
	.align 4
w2:
	.long	3
	.globl	w3
	.align 4
w3:
	.long	3
	.globl	mod1
	.align 4
mod1:
	.long	998244353
	.globl	mod2
	.align 4
mod2:
	.long	1004535809
	.globl	mod3
	.align 4
mod3:
	.long	469762049
	.globl	inv_mod1
	.align 4
inv_mod1:
	.long	669690699
	.globl	mod12_INV_mod3
	.align 4
mod12_INV_mod3:
	.long	354521948
	.globl	mod12
	.align 16
mod12:
	.quad	1002772198720536577
	.quad	0
	.globl	k
	.align 4
k:
	.long	93
	.align 8
mu1:
	.quad	18479187002
	.align 8
mu2:
	.quad	18363450967
	.align 8
mu3:
	.quad	39268272336
	.align 4
mu1_lo:
	.long	1299317818
	.align 4
mu1_hi:
	.long	4
	.align 4
mu2_lo:
	.long	1183581783
	.align 4
mu2_hi:
	.long	4
	.align 4
mu3_lo:
	.long	613566672
	.align 4
mu3_hi:
	.long	9
.lcomm root,2097152,32
.lcomm rev,4194304,32
.lcomm lastRev,4,32
.lcomm a,4194304,32
.lcomm b,4194304,32
.lcomm c1,4194304,32
.lcomm c2,4194304,32
.lcomm c3,4194304,32
.lcomm A,4194304,32
.lcomm B,4194304,32
.lcomm C,4194304,32
.lcomm io_buf,2000005,32
.lcomm tmp,20,16
	.globl	na
	.bss
	.align 4
na:
	.space 4
	.globl	nb
	.align 4
nb:
	.space 4
	.text
	.p2align 4
	.def	parse_blocks_simd;	.scl	3;	.type	32;	.endef
	.seh_proc	parse_blocks_simd
parse_blocks_simd:
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$168, %rsp
	.seh_stackalloc	168
	vmovaps	%xmm6, (%rsp)
	.seh_savexmm	%xmm6, 0
	vmovaps	%xmm7, 16(%rsp)
	.seh_savexmm	%xmm7, 16
	vmovaps	%xmm8, 32(%rsp)
	.seh_savexmm	%xmm8, 32
	vmovaps	%xmm9, 48(%rsp)
	.seh_savexmm	%xmm9, 48
	vmovaps	%xmm10, 64(%rsp)
	.seh_savexmm	%xmm10, 64
	vmovaps	%xmm11, 80(%rsp)
	.seh_savexmm	%xmm11, 80
	vmovaps	%xmm12, 96(%rsp)
	.seh_savexmm	%xmm12, 96
	vmovaps	%xmm13, 112(%rsp)
	.seh_savexmm	%xmm13, 112
	vmovaps	%xmm14, 128(%rsp)
	.seh_savexmm	%xmm14, 128
	vmovaps	%xmm15, 144(%rsp)
	.seh_savexmm	%xmm15, 144
	.seh_endprologue
	movl	%edx, %ebx
	movq	%rcx, %r11
	movq	240(%rsp), %rdx
	leal	-71(%rbx), %r10d
	cmpl	%r10d, %r8d
	jge	.L53
	movl	$-48, %eax
	vmovdqa	.LC0(%rip), %ymm14
	vpcmpeqd	%ymm2, %ymm2, %ymm2
	vpsrld	$24, %ymm2, %ymm3
	vmovd	%eax, %xmm1
	vmovdqa	.LC1(%rip), %ymm13
	vpbroadcastd	%xmm1, %ymm1
	.p2align 4,,10
	.p2align 3
.L4:
	movl	%ebx, %eax
	vmovd	%ebx, %xmm0
	vmovdqa	%ymm2, %ymm7
	leal	-26(%rax), %edi
	vpbroadcastd	%xmm0, %ymm0
	subl	$72, %ebx
	leal	-44(%rax), %ebp
	vmovd	%edi, %xmm6
	vpaddd	%ymm14, %ymm0, %ymm4
	leal	-8(%rax), %ecx
	vmovd	%ebp, %xmm5
	vpgatherdd	%ymm7, (%r11,%ymm4,1), %ymm8
	leal	-17(%rax), %esi
	vmovd	%ecx, %xmm4
	vpand	%ymm3, %ymm8, %ymm8
	leal	-35(%rax), %edi
	vpinsrd	$1, %esi, %xmm4, %xmm7
	leal	-53(%rax), %ebp
	subl	$62, %eax
	vpinsrd	$1, %edi, %xmm6, %xmm15
	vpunpcklqdq	%xmm15, %xmm7, %xmm6
	vmovd	%eax, %xmm9
	vpinsrd	$1, %ebp, %xmm5, %xmm11
	vmovdqa	%ymm2, %ymm15
	movslq	(%rdx), %rax
	vpinsrd	$1, %r10d, %xmm9, %xmm10
	vmovdqa	%ymm2, %ymm9
	vpunpcklqdq	%xmm10, %xmm11, %xmm12
	vpaddd	%ymm13, %ymm0, %ymm10
	vinserti128	$0x1, %xmm12, %ymm6, %ymm5
	vmovdqa	%ymm2, %ymm11
	subl	$72, %r10d
	vpgatherdd	%ymm9, (%r11,%ymm5,1), %ymm7
	vpaddd	.LC3(%rip), %ymm0, %ymm9
	vpaddd	.LC2(%rip), %ymm0, %ymm12
	vpand	%ymm3, %ymm7, %ymm7
	cmpl	%r10d, %r8d
	vpgatherdd	%ymm11, (%r11,%ymm10,1), %ymm6
	vmovdqa	%ymm2, %ymm10
	vpaddd	.LC4(%rip), %ymm0, %ymm11
	vpgatherdd	%ymm10, (%r11,%ymm9,1), %ymm4
	vpaddd	.LC5(%rip), %ymm0, %ymm9
	vmovdqa	%ymm2, %ymm10
	vpand	%ymm3, %ymm6, %ymm6
	vpgatherdd	%ymm15, (%r11,%ymm12,1), %ymm5
	vmovdqa	%ymm2, %ymm15
	vpand	%ymm3, %ymm4, %ymm4
	vpgatherdd	%ymm15, (%r11,%ymm11,1), %ymm12
	vmovdqa	%ymm2, %ymm15
	vpand	%ymm3, %ymm5, %ymm5
	vpgatherdd	%ymm10, (%r11,%ymm9,1), %ymm11
	vpaddd	.LC6(%rip), %ymm0, %ymm9
	vpaddd	.LC7(%rip), %ymm0, %ymm0
	vpand	%ymm3, %ymm12, %ymm12
	vpand	%ymm3, %ymm11, %ymm11
	leal	1(%rax), %ecx
	vpgatherdd	%ymm15, (%r11,%ymm9,1), %ymm10
	vmovdqa	%ymm2, %ymm15
	vpgatherdd	%ymm15, (%r11,%ymm0,1), %ymm9
	vpaddd	%ymm1, %ymm8, %ymm15
	vpand	%ymm3, %ymm10, %ymm10
	movl	%ecx, (%rdx)
	vpslld	$2, %ymm15, %ymm0
	vpand	%ymm3, %ymm9, %ymm9
	vpaddd	%ymm15, %ymm0, %ymm8
	vpaddd	%ymm1, %ymm7, %ymm0
	vpslld	$1, %ymm8, %ymm15
	vpaddd	%ymm0, %ymm15, %ymm8
	vpslld	$2, %ymm8, %ymm15
	vpaddd	%ymm8, %ymm15, %ymm7
	vpaddd	%ymm1, %ymm6, %ymm8
	vpslld	$1, %ymm7, %ymm0
	vpaddd	%ymm8, %ymm0, %ymm15
	vpaddd	%ymm1, %ymm5, %ymm8
	vpslld	$2, %ymm15, %ymm7
	vpaddd	%ymm1, %ymm4, %ymm5
	vpaddd	%ymm15, %ymm7, %ymm0
	vpslld	$1, %ymm0, %ymm6
	vpaddd	%ymm8, %ymm6, %ymm15
	vpslld	$2, %ymm15, %ymm7
	vpaddd	%ymm15, %ymm7, %ymm0
	vpslld	$1, %ymm0, %ymm6
	vpaddd	%ymm5, %ymm6, %ymm8
	vpaddd	%ymm1, %ymm12, %ymm6
	vpslld	$2, %ymm8, %ymm15
	vpaddd	%ymm8, %ymm15, %ymm7
	vpslld	$1, %ymm7, %ymm0
	vpaddd	%ymm1, %ymm11, %ymm7
	vpaddd	%ymm6, %ymm0, %ymm5
	vpslld	$2, %ymm5, %ymm4
	vpaddd	%ymm5, %ymm4, %ymm8
	vpaddd	%ymm1, %ymm10, %ymm4
	vpslld	$1, %ymm8, %ymm15
	vpaddd	%ymm7, %ymm15, %ymm0
	vpslld	$2, %ymm0, %ymm12
	vpaddd	%ymm0, %ymm12, %ymm6
	vpaddd	%ymm1, %ymm9, %ymm0
	vpslld	$1, %ymm6, %ymm5
	vpaddd	%ymm4, %ymm5, %ymm8
	vpslld	$2, %ymm8, %ymm15
	vpaddd	%ymm8, %ymm15, %ymm11
	vpslld	$1, %ymm11, %ymm7
	vpaddd	%ymm0, %ymm7, %ymm12
	vmovd	%xmm12, (%r9,%rax,4)
	movslq	(%rdx), %rsi
	vextracti128	$0x1, %ymm12, %xmm5
	leal	1(%rsi), %edi
	movl	%edi, (%rdx)
	vpextrd	$1, %xmm12, (%r9,%rsi,4)
	movslq	(%rdx), %rbp
	leal	1(%rbp), %eax
	movl	%eax, (%rdx)
	vpextrd	$2, %xmm12, (%r9,%rbp,4)
	movslq	(%rdx), %rsi
	leal	1(%rsi), %ecx
	movl	%ecx, (%rdx)
	vpextrd	$3, %xmm12, (%r9,%rsi,4)
	movslq	(%rdx), %rdi
	leal	1(%rdi), %ebp
	movl	%ebp, (%rdx)
	vmovd	%xmm5, (%r9,%rdi,4)
	movslq	(%rdx), %rax
	leal	1(%rax), %esi
	movl	%esi, (%rdx)
	vpextrd	$1, %xmm5, (%r9,%rax,4)
	movslq	(%rdx), %rdi
	leal	1(%rdi), %ecx
	movl	%ecx, (%rdx)
	vpextrd	$2, %xmm5, (%r9,%rdi,4)
	movslq	(%rdx), %rbp
	leal	1(%rbp), %eax
	movl	%eax, (%rdx)
	vpextrd	$3, %xmm5, (%r9,%rbp,4)
	jl	.L4
	vzeroupper
.L53:
	cmpl	%ebx, %r8d
	jge	.L55
	.p2align 4,,10
	.p2align 3
.L9:
	movl	%ebx, %r10d
	movl	%r8d, %edi
	subl	$9, %ebx
	leal	-8(%r10), %esi
	cmpl	%esi, %r8d
	cmovl	%ebx, %edi
	cmpl	%r10d, %edi
	jge	.L12
	movslq	%edi, %rbp
	subl	%edi, %r10d
	xorl	%esi, %esi
	addq	%r11, %rbp
	leaq	(%r10,%rbp), %rax
	andl	$7, %r10d
	je	.L8
	cmpq	$1, %r10
	je	.L38
	cmpq	$2, %r10
	je	.L39
	cmpq	$3, %r10
	je	.L40
	cmpq	$4, %r10
	je	.L41
	cmpq	$5, %r10
	je	.L42
	cmpq	$6, %r10
	je	.L43
	movsbl	0(%rbp), %esi
	addq	$1, %rbp
	subl	$48, %esi
.L43:
	movsbl	0(%rbp), %ecx
	leal	(%rsi,%rsi,4), %esi
	addq	$1, %rbp
	leal	-48(%rcx,%rsi,2), %esi
.L42:
	movsbl	0(%rbp), %r10d
	leal	(%rsi,%rsi,4), %edi
	addq	$1, %rbp
	leal	-48(%r10,%rdi,2), %esi
.L41:
	movsbl	0(%rbp), %ecx
	leal	(%rsi,%rsi,4), %esi
	addq	$1, %rbp
	leal	-48(%rcx,%rsi,2), %esi
.L40:
	movsbl	0(%rbp), %r10d
	leal	(%rsi,%rsi,4), %edi
	addq	$1, %rbp
	leal	-48(%r10,%rdi,2), %esi
.L39:
	movsbl	0(%rbp), %ecx
	leal	(%rsi,%rsi,4), %esi
	addq	$1, %rbp
	leal	-48(%rcx,%rsi,2), %esi
.L38:
	movsbl	0(%rbp), %r10d
	leal	(%rsi,%rsi,4), %edi
	addq	$1, %rbp
	cmpq	%rax, %rbp
	leal	-48(%r10,%rdi,2), %esi
	je	.L7
.L8:
	movsbl	0(%rbp), %ecx
	leal	(%rsi,%rsi,4), %edi
	addq	$8, %rbp
	leal	-48(%rcx,%rdi,2), %r10d
	movsbl	-7(%rbp), %edi
	leal	(%r10,%r10,4), %esi
	leal	-48(%rdi,%rsi,2), %ecx
	movsbl	-6(%rbp), %esi
	leal	(%rcx,%rcx,4), %r10d
	movsbl	-5(%rbp), %ecx
	leal	-48(%rsi,%r10,2), %edi
	leal	(%rdi,%rdi,4), %r10d
	leal	-48(%rcx,%r10,2), %esi
	movsbl	-4(%rbp), %r10d
	leal	(%rsi,%rsi,4), %edi
	leal	-48(%r10,%rdi,2), %ecx
	movsbl	-3(%rbp), %edi
	leal	(%rcx,%rcx,4), %esi
	movsbl	-2(%rbp), %ecx
	leal	-48(%rdi,%rsi,2), %r10d
	leal	(%r10,%r10,4), %esi
	leal	-48(%rcx,%rsi,2), %edi
	movsbl	-1(%rbp), %esi
	cmpq	%rax, %rbp
	leal	(%rdi,%rdi,4), %r10d
	leal	-48(%rsi,%r10,2), %esi
	jne	.L8
.L7:
	movslq	(%rdx), %rbp
	leal	1(%rbp), %eax
	movl	%eax, (%rdx)
	movl	%esi, (%r9,%rbp,4)
.L56:
	cmpl	%ebx, %r8d
	jl	.L9
.L55:
	vmovaps	(%rsp), %xmm6
	vmovaps	16(%rsp), %xmm7
	vmovaps	32(%rsp), %xmm8
	vmovaps	48(%rsp), %xmm9
	vmovaps	64(%rsp), %xmm10
	vmovaps	80(%rsp), %xmm11
	vmovaps	96(%rsp), %xmm12
	vmovaps	112(%rsp), %xmm13
	vmovaps	128(%rsp), %xmm14
	vmovaps	144(%rsp), %xmm15
	addq	$168, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	ret
	.p2align 4,,10
	.p2align 3
.L12:
	movslq	(%rdx), %rbp
	xorl	%esi, %esi
	leal	1(%rbp), %eax
	movl	%eax, (%rdx)
	movl	%esi, (%r9,%rbp,4)
	jmp	.L56
	.seh_endproc
	.p2align 4
	.def	div10_epu32;	.scl	3;	.type	32;	.endef
	.seh_proc	div10_epu32
div10_epu32:
	.seh_endprologue
	vmovdqa	(%rdx), %ymm2
	movl	$-858993459, %edx
	vmovd	%edx, %xmm3
	vpbroadcastd	%xmm3, %ymm4
	vpsrlq	$32, %ymm2, %ymm1
	vpmuludq	%ymm4, %ymm1, %ymm1
	vpmuludq	%ymm4, %ymm2, %ymm0
	movq	%rcx, %rax
	vpsrlq	$35, %ymm1, %ymm3
	vpsllq	$32, %ymm3, %ymm4
	vpsrlq	$35, %ymm0, %ymm5
	vpor	%ymm4, %ymm5, %ymm0
	vpslld	$2, %ymm0, %ymm5
	vmovdqa	%ymm0, (%rcx)
	vpaddd	%ymm0, %ymm5, %ymm1
	vpslld	$1, %ymm1, %ymm3
	vpsubd	%ymm3, %ymm2, %ymm2
	vmovdqa	%ymm2, (%r8)
	vzeroupper
	ret
	.seh_endproc
	.p2align 4
	.def	ntt1;	.scl	3;	.type	32;	.endef
	.seh_proc	ntt1
ntt1:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$168, %rsp
	.seh_stackalloc	168
	vmovaps	%xmm6, 32(%rsp)
	.seh_savexmm	%xmm6, 32
	vmovaps	%xmm7, 48(%rsp)
	.seh_savexmm	%xmm7, 48
	vmovaps	%xmm8, 64(%rsp)
	.seh_savexmm	%xmm8, 64
	vmovaps	%xmm9, 80(%rsp)
	.seh_savexmm	%xmm9, 80
	vmovaps	%xmm10, 96(%rsp)
	.seh_savexmm	%xmm10, 96
	vmovaps	%xmm11, 112(%rsp)
	.seh_savexmm	%xmm11, 112
	vmovaps	%xmm12, 128(%rsp)
	.seh_savexmm	%xmm12, 128
	vmovaps	%xmm13, 144(%rsp)
	.seh_savexmm	%xmm13, 144
	.seh_endprologue
	cmpl	$2, %edx
	movq	%rcx, %rbx
	movl	%edx, 248(%rsp)
	jle	.L59
	leal	-1(%rdx), %r9d
	movl	%edx, %esi
	movl	$1, %r14d
	leal	-3(%rsi), %ecx
	leaq	rev(%rip), %rdx
	shrl	%ecx
	addl	$1, %ecx
	andl	$3, %ecx
	je	.L62
	cmpl	$1, %ecx
	je	.L109
	cmpl	$2, %ecx
	je	.L110
	movl	4(%rdx), %eax
	cmpl	$1, %eax
	jnb	.L88
	movl	(%rbx,%rax,4), %ebp
	movl	4(%rbx), %edi
	movl	%ebp, 4(%rbx)
	movl	4(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L88:
	movl	8(%rdx), %r10d
	cmpl	$2, %r10d
	jb	.L118
.L89:
	movl	$3, %r14d
	addq	$8, %rdx
.L110:
	movl	4(%rdx), %r15d
	movl	%r14d, %esi
	cmpl	%r14d, %r15d
	jnb	.L92
	movl	(%rbx,%r15,4), %ecx
	movl	(%rbx,%r14,4), %eax
	movl	%ecx, (%rbx,%r14,4)
	movl	4(%rdx), %edi
	movl	%eax, (%rbx,%rdi,4)
.L92:
	movl	8(%rdx), %ebp
	addl	$1, %esi
	cmpl	%esi, %ebp
	jnb	.L93
	movl	(%rbx,%rbp,4), %r10d
	movl	4(%rbx,%r14,4), %r8d
	movl	%r10d, 4(%rbx,%r14,4)
	movl	8(%rdx), %r11d
	movl	%r8d, (%rbx,%r11,4)
.L93:
	addq	$2, %r14
	addq	$8, %rdx
.L109:
	movl	4(%rdx), %r12d
	movl	%r14d, %r13d
	cmpl	%r14d, %r12d
	jnb	.L96
	movl	(%rbx,%r12,4), %esi
	movl	(%rbx,%r14,4), %r15d
	movl	%esi, (%rbx,%r14,4)
	movl	4(%rdx), %eax
	movl	%r15d, (%rbx,%rax,4)
.L96:
	movl	8(%rdx), %ecx
	addl	$1, %r13d
	cmpl	%r13d, %ecx
	jnb	.L97
	movl	(%rbx,%rcx,4), %ebp
	movl	4(%rbx,%r14,4), %edi
	movl	%ebp, 4(%rbx,%r14,4)
	movl	8(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L97:
	addq	$2, %r14
	addq	$8, %rdx
	cmpl	%r14d, %r9d
	jle	.L63
.L62:
	movl	4(%rdx), %r11d
	movl	%r14d, %r12d
	cmpl	%r14d, %r11d
	jnb	.L60
	movl	(%rbx,%r11,4), %r13d
	movl	(%rbx,%r14,4), %r10d
	movl	%r13d, (%rbx,%r14,4)
	movl	4(%rdx), %r15d
	movl	%r10d, (%rbx,%r15,4)
.L60:
	movl	8(%rdx), %esi
	addl	$1, %r12d
	cmpl	%r12d, %esi
	jnb	.L61
	movl	(%rbx,%rsi,4), %ecx
	movl	4(%rbx,%r14,4), %eax
	movl	%ecx, 4(%rbx,%r14,4)
	movl	8(%rdx), %edi
	movl	%eax, (%rbx,%rdi,4)
.L61:
	movl	12(%rdx), %ebp
	addq	$2, %r14
	addq	$8, %rdx
	cmpl	%r14d, %ebp
	jnb	.L100
	movl	(%rbx,%rbp,4), %r11d
	movl	(%rbx,%r14,4), %r8d
	movl	%r11d, (%rbx,%r14,4)
	movl	4(%rdx), %r12d
	movl	%r8d, (%rbx,%r12,4)
.L100:
	movl	8(%rdx), %r10d
	leal	1(%r14), %r13d
	cmpl	%r13d, %r10d
	jnb	.L101
	movl	(%rbx,%r10,4), %esi
	movl	4(%rbx,%r14,4), %r15d
	movl	%esi, 4(%rbx,%r14,4)
	movl	8(%rdx), %eax
	movl	%r15d, (%rbx,%rax,4)
.L101:
	movl	12(%rdx), %edi
	leaq	2(%r14), %rcx
	cmpl	%ecx, %edi
	jnb	.L103
	movl	(%rbx,%rdi,4), %r8d
	movl	(%rbx,%rcx,4), %ebp
	movl	%r8d, (%rbx,%rcx,4)
	movl	12(%rdx), %r11d
	movl	%ebp, (%rbx,%r11,4)
.L103:
	movl	16(%rdx), %r12d
	leal	1(%rcx), %r10d
	cmpl	%r10d, %r12d
	jnb	.L104
	movl	(%rbx,%r12,4), %r15d
	movl	4(%rbx,%rcx,4), %r13d
	movl	%r15d, 4(%rbx,%rcx,4)
	movl	16(%rdx), %esi
	movl	%r13d, (%rbx,%rsi,4)
.L104:
	movl	20(%rdx), %ecx
	leaq	4(%r14), %rax
	cmpl	%eax, %ecx
	jnb	.L106
	movl	(%rbx,%rcx,4), %ebp
	movl	(%rbx,%rax,4), %edi
	movl	%ebp, (%rbx,%rax,4)
	movl	20(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L106:
	movl	24(%rdx), %r11d
	leal	1(%rax), %r12d
	cmpl	%r12d, %r11d
	jnb	.L107
	movl	(%rbx,%r11,4), %r13d
	movl	4(%rbx,%rax,4), %r10d
	movl	%r13d, 4(%rbx,%rax,4)
	movl	24(%rdx), %r15d
	movl	%r10d, (%rbx,%r15,4)
.L107:
	addq	$6, %r14
	addq	$24, %rdx
	cmpl	%r14d, %r9d
	jg	.L62
.L63:
	movl	248(%rsp), %ebp
	movl	$4294967295, %r9d
	movl	$2, 28(%rsp)
	vmovq	%r9, %xmm7
	vmovdqa	.LC13(%rip), %ymm5
	vmovdqa	.LC14(%rip), %ymm6
	vpbroadcastq	%xmm7, %ymm7
	vmovdqa	.LC15(%rip), %ymm8
	vmovdqa	.LC18(%rip), %ymm9
	sarl	%ebp
	vpbroadcastq	.LC22(%rip), %ymm3
.L82:
	movslq	28(%rsp), %rdx
	xorl	%r14d, %r14d
	xorl	%esi, %esi
	xorl	%r12d, %r12d
	movl	%edx, %edi
	movq	%rdx, 16(%rsp)
	sarl	%edi
	leal	-4(%rdi), %r13d
	movl	%r13d, 24(%rsp)
	andl	$-4, %r13d
	addl	$4, %r13d
	cmpl	$3, %edi
	cmovle	%r14d, %r13d
	jle	.L65
	.p2align 4,,10
	.p2align 3
.L79:
	movl	24(%rsp), %r10d
	movl	$998244353, %eax
	movslq	%ebp, %r14
	leal	(%rbp,%rbp), %r8d
	vmovd	%eax, %xmm2
	vpbroadcastq	.LC23(%rip), %ymm1
	leal	0(,%rbp,4), %ecx
	movslq	%r8d, %r11
	addl	%ebp, %r8d
	movslq	%r8d, %r9
	movl	$998244352, %r8d
	movslq	%ecx, %r15
	shrl	$2, %r10d
	vmovd	%r8d, %xmm4
	salq	$2, %r15
	leal	0(,%r10,4), %edx
	movslq	%edi, %rcx
	vpbroadcastd	%xmm2, %xmm2
	movq	%rdx, 8(%rsp)
	vpbroadcastd	%xmm4, %xmm4
.L81:
	movq	8(%rsp), %r10
	leaq	root(%rip), %rdx
	leaq	(%rbx,%rsi,4), %rax
	addq	%rsi, %r10
	leaq	16(%rbx,%r10,4), %r8
	.p2align 4,,10
	.p2align 3
.L66:
	vpmovzxdq	(%rax,%rcx,4), %ymm0
	vmovd	(%rdx,%r11,4), %xmm10
	vpinsrd	$1, (%rdx,%r9,4), %xmm10, %xmm11
	vmovd	(%rdx), %xmm12
	vpinsrd	$1, (%rdx,%r14,4), %xmm12, %xmm13
	vpunpcklqdq	%xmm11, %xmm13, %xmm10
	vpmovzxdq	%xmm10, %ymm11
	vpmuludq	%ymm11, %ymm0, %ymm0
	addq	%r15, %rdx
	vpsrlq	$32, %ymm0, %ymm11
	vpand	%ymm7, %ymm0, %ymm13
	vpmuludq	%ymm5, %ymm13, %ymm12
	vpmuludq	%ymm6, %ymm13, %ymm10
	vpmuludq	%ymm5, %ymm11, %ymm13
	vpsrlq	$32, %ymm12, %ymm12
	vpmuludq	%ymm6, %ymm11, %ymm11
	vpaddq	%ymm13, %ymm10, %ymm10
	vpaddq	%ymm12, %ymm10, %ymm13
	vpsrlq	$32, %ymm13, %ymm12
	vpaddq	%ymm12, %ymm11, %ymm11
	vpmuludq	%ymm8, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm0
	vpcmpgtq	%ymm3, %ymm0, %ymm13
	vpand	%ymm1, %ymm13, %ymm12
	vpsubq	%ymm12, %ymm0, %ymm11
	vpcmpgtq	%ymm3, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm0
	vpsubq	%ymm0, %ymm11, %ymm13
	vpcmpgtq	%ymm3, %ymm13, %ymm12
	vpand	%ymm1, %ymm12, %ymm11
	vpsubq	%ymm11, %ymm13, %ymm10
	vmovdqa	(%rax), %xmm13
	vpermd	%ymm10, %ymm9, %ymm0
	vpsubd	%xmm0, %xmm2, %xmm11
	vpaddd	%xmm0, %xmm13, %xmm12
	vpaddd	%xmm13, %xmm11, %xmm0
	vpcmpgtd	%xmm4, %xmm12, %xmm10
	vpcmpgtd	%xmm4, %xmm0, %xmm11
	vpand	%xmm2, %xmm10, %xmm13
	vpsubd	%xmm13, %xmm12, %xmm12
	vpand	%xmm2, %xmm11, %xmm10
	vpsubd	%xmm10, %xmm0, %xmm0
	vmovdqa	%xmm12, (%rax)
	vmovdqa	%xmm0, (%rax,%rcx,4)
	addq	$16, %rax
	cmpq	%rax, %r8
	jne	.L66
	cmpl	%r13d, %edi
	jle	.L71
.L68:
	leal	2(%r13), %r14d
	movl	%r13d, %r11d
	leal	1(%r13), %r9d
	imull	%ebp, %r11d
	movl	%r14d, %ecx
	imull	%ebp, %ecx
	movl	%r9d, %r15d
	imull	%ebp, %r15d
	movslq	%r11d, %r8
	movl	%ecx, 8(%rsp)
.L80:
	leal	0(%r13,%r12), %eax
	movslq	%eax, %rdx
	addl	%edi, %eax
	leaq	(%rbx,%rdx,4), %r11
	cltq
	leaq	(%rbx,%rax,4), %r10
	leaq	root(%rip), %rax
	movl	(%r10), %ecx
	movl	(%rax,%r8,4), %edx
	movabsq	$-8525806094425994177, %rax
	imulq	%rdx, %rcx
	mulq	%rcx
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rax
	subq	%rax, %rcx
	movl	(%r11), %eax
	addl	%ecx, %eax
	addl	%ecx, %ecx
	movl	%eax, %edx
	subl	%ecx, %edx
	cmpl	$998244352, %eax
	leal	-998244353(%rax), %ecx
	cmovbe	%eax, %ecx
	cmpl	$-998244354, %edx
	leal	998244353(%rdx), %eax
	movl	%ecx, (%r11)
	cmovbe	%edx, %eax
	cmpl	%r9d, %edi
	movl	%eax, (%r10)
	jle	.L74
	leal	(%r9,%r12), %eax
	movslq	%r15d, %rdx
	leaq	root(%rip), %rcx
	movslq	%eax, %r11
	addl	%edi, %eax
	movl	(%rcx,%rdx,4), %ecx
	leaq	(%rbx,%r11,4), %r11
	cltq
	leaq	(%rbx,%rax,4), %r10
	movl	(%r10), %eax
	imulq	%rax, %rcx
	movabsq	$-8525806094425994177, %rax
	mulq	%rcx
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rax
	movl	(%r11), %edx
	subq	%rax, %rcx
	leal	(%rcx,%rcx), %eax
	addl	%ecx, %edx
	movl	%edx, %ecx
	subl	%eax, %ecx
	cmpl	$998244352, %edx
	leal	-998244353(%rdx), %eax
	cmova	%eax, %edx
	cmpl	$-998244354, %ecx
	leal	998244353(%rcx), %eax
	movl	%edx, (%r11)
	cmovbe	%ecx, %eax
	cmpl	%edi, %r14d
	movl	%eax, (%r10)
	jge	.L74
	movslq	8(%rsp), %rdx
	leaq	root(%rip), %rcx
	addl	%r14d, %r12d
	movslq	%r12d, %r11
	addl	%edi, %r12d
	leaq	(%rbx,%r11,4), %r11
	movslq	%r12d, %r12
	leaq	(%rbx,%r12,4), %r10
	movl	(%r10), %eax
	movl	(%rcx,%rdx,4), %r12d
	imulq	%rax, %r12
	movabsq	$-8525806094425994177, %rax
	mulq	%r12
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rcx
	movl	(%r11), %edx
	subq	%rcx, %r12
	addl	%r12d, %edx
	leal	(%r12,%r12), %r12d
	movl	%edx, %ecx
	leal	-998244353(%rdx), %eax
	subl	%r12d, %ecx
	cmpl	$998244352, %edx
	cmova	%eax, %edx
	cmpl	$-998244354, %ecx
	leal	998244353(%rcx), %r12d
	movl	%edx, (%r11)
	cmovbe	%ecx, %r12d
	movl	%r12d, (%r10)
.L74:
	addq	16(%rsp), %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L69
	cmpl	$3, %edi
	movl	%esi, %r12d
	jg	.L79
	jmp	.L80
.L65:
	testl	%edi, %edi
	jg	.L68
.L119:
	movq	16(%rsp), %rax
	addq	%rax, %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L69
	addq	%rax, %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L69
	testl	%edi, %edi
	movl	%esi, %r12d
	jle	.L119
	jmp	.L68
.L71:
	addq	16(%rsp), %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L69
	movl	%esi, %r12d
	jmp	.L81
.L69:
	sall	28(%rsp)
	sarl	%ebp
	movl	28(%rsp), %edi
	cmpl	%edi, 248(%rsp)
	jge	.L82
	vzeroupper
.L117:
	vmovaps	32(%rsp), %xmm6
	vmovaps	48(%rsp), %xmm7
	vmovaps	64(%rsp), %xmm8
	vmovaps	80(%rsp), %xmm9
	vmovaps	96(%rsp), %xmm10
	vmovaps	112(%rsp), %xmm11
	vmovaps	128(%rsp), %xmm12
	vmovaps	144(%rsp), %xmm13
	addq	$168, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L118:
	movl	(%rbx,%r10,4), %r12d
	movl	8(%rbx), %r11d
	movl	%r12d, 8(%rbx)
	movl	8(%rdx), %r13d
	movl	%r11d, (%rbx,%r13,4)
	jmp	.L89
.L59:
	jne	.L117
	jmp	.L63
	.seh_endproc
	.p2align 4
	.def	ntt2;	.scl	3;	.type	32;	.endef
	.seh_proc	ntt2
ntt2:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$168, %rsp
	.seh_stackalloc	168
	vmovaps	%xmm6, 32(%rsp)
	.seh_savexmm	%xmm6, 32
	vmovaps	%xmm7, 48(%rsp)
	.seh_savexmm	%xmm7, 48
	vmovaps	%xmm8, 64(%rsp)
	.seh_savexmm	%xmm8, 64
	vmovaps	%xmm9, 80(%rsp)
	.seh_savexmm	%xmm9, 80
	vmovaps	%xmm10, 96(%rsp)
	.seh_savexmm	%xmm10, 96
	vmovaps	%xmm11, 112(%rsp)
	.seh_savexmm	%xmm11, 112
	vmovaps	%xmm12, 128(%rsp)
	.seh_savexmm	%xmm12, 128
	vmovaps	%xmm13, 144(%rsp)
	.seh_savexmm	%xmm13, 144
	.seh_endprologue
	cmpl	$2, %edx
	movq	%rcx, %rbx
	movl	%edx, 248(%rsp)
	jle	.L121
	leal	-1(%rdx), %r9d
	movl	%edx, %esi
	movl	$1, %r14d
	leal	-3(%rsi), %ecx
	leaq	rev(%rip), %rdx
	shrl	%ecx
	addl	$1, %ecx
	andl	$3, %ecx
	je	.L124
	cmpl	$1, %ecx
	je	.L171
	cmpl	$2, %ecx
	je	.L172
	movl	4(%rdx), %eax
	cmpl	$1, %eax
	jnb	.L150
	movl	(%rbx,%rax,4), %ebp
	movl	4(%rbx), %edi
	movl	%ebp, 4(%rbx)
	movl	4(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L150:
	movl	8(%rdx), %r10d
	cmpl	$2, %r10d
	jb	.L180
.L151:
	movl	$3, %r14d
	addq	$8, %rdx
.L172:
	movl	4(%rdx), %r15d
	movl	%r14d, %esi
	cmpl	%r14d, %r15d
	jnb	.L154
	movl	(%rbx,%r15,4), %ecx
	movl	(%rbx,%r14,4), %eax
	movl	%ecx, (%rbx,%r14,4)
	movl	4(%rdx), %edi
	movl	%eax, (%rbx,%rdi,4)
.L154:
	movl	8(%rdx), %ebp
	addl	$1, %esi
	cmpl	%esi, %ebp
	jnb	.L155
	movl	(%rbx,%rbp,4), %r10d
	movl	4(%rbx,%r14,4), %r8d
	movl	%r10d, 4(%rbx,%r14,4)
	movl	8(%rdx), %r11d
	movl	%r8d, (%rbx,%r11,4)
.L155:
	addq	$2, %r14
	addq	$8, %rdx
.L171:
	movl	4(%rdx), %r12d
	movl	%r14d, %r13d
	cmpl	%r14d, %r12d
	jnb	.L158
	movl	(%rbx,%r12,4), %esi
	movl	(%rbx,%r14,4), %r15d
	movl	%esi, (%rbx,%r14,4)
	movl	4(%rdx), %eax
	movl	%r15d, (%rbx,%rax,4)
.L158:
	movl	8(%rdx), %ecx
	addl	$1, %r13d
	cmpl	%r13d, %ecx
	jnb	.L159
	movl	(%rbx,%rcx,4), %ebp
	movl	4(%rbx,%r14,4), %edi
	movl	%ebp, 4(%rbx,%r14,4)
	movl	8(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L159:
	addq	$2, %r14
	addq	$8, %rdx
	cmpl	%r14d, %r9d
	jle	.L125
.L124:
	movl	4(%rdx), %r11d
	movl	%r14d, %r12d
	cmpl	%r14d, %r11d
	jnb	.L122
	movl	(%rbx,%r11,4), %r13d
	movl	(%rbx,%r14,4), %r10d
	movl	%r13d, (%rbx,%r14,4)
	movl	4(%rdx), %r15d
	movl	%r10d, (%rbx,%r15,4)
.L122:
	movl	8(%rdx), %esi
	addl	$1, %r12d
	cmpl	%r12d, %esi
	jnb	.L123
	movl	(%rbx,%rsi,4), %ecx
	movl	4(%rbx,%r14,4), %eax
	movl	%ecx, 4(%rbx,%r14,4)
	movl	8(%rdx), %edi
	movl	%eax, (%rbx,%rdi,4)
.L123:
	movl	12(%rdx), %ebp
	addq	$2, %r14
	addq	$8, %rdx
	cmpl	%r14d, %ebp
	jnb	.L162
	movl	(%rbx,%rbp,4), %r11d
	movl	(%rbx,%r14,4), %r8d
	movl	%r11d, (%rbx,%r14,4)
	movl	4(%rdx), %r12d
	movl	%r8d, (%rbx,%r12,4)
.L162:
	movl	8(%rdx), %r10d
	leal	1(%r14), %r13d
	cmpl	%r13d, %r10d
	jnb	.L163
	movl	(%rbx,%r10,4), %esi
	movl	4(%rbx,%r14,4), %r15d
	movl	%esi, 4(%rbx,%r14,4)
	movl	8(%rdx), %eax
	movl	%r15d, (%rbx,%rax,4)
.L163:
	movl	12(%rdx), %edi
	leaq	2(%r14), %rcx
	cmpl	%ecx, %edi
	jnb	.L165
	movl	(%rbx,%rdi,4), %r8d
	movl	(%rbx,%rcx,4), %ebp
	movl	%r8d, (%rbx,%rcx,4)
	movl	12(%rdx), %r11d
	movl	%ebp, (%rbx,%r11,4)
.L165:
	movl	16(%rdx), %r12d
	leal	1(%rcx), %r10d
	cmpl	%r10d, %r12d
	jnb	.L166
	movl	(%rbx,%r12,4), %r15d
	movl	4(%rbx,%rcx,4), %r13d
	movl	%r15d, 4(%rbx,%rcx,4)
	movl	16(%rdx), %esi
	movl	%r13d, (%rbx,%rsi,4)
.L166:
	movl	20(%rdx), %ecx
	leaq	4(%r14), %rax
	cmpl	%eax, %ecx
	jnb	.L168
	movl	(%rbx,%rcx,4), %ebp
	movl	(%rbx,%rax,4), %edi
	movl	%ebp, (%rbx,%rax,4)
	movl	20(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L168:
	movl	24(%rdx), %r11d
	leal	1(%rax), %r12d
	cmpl	%r12d, %r11d
	jnb	.L169
	movl	(%rbx,%r11,4), %r13d
	movl	4(%rbx,%rax,4), %r10d
	movl	%r13d, 4(%rbx,%rax,4)
	movl	24(%rdx), %r15d
	movl	%r10d, (%rbx,%r15,4)
.L169:
	addq	$6, %r14
	addq	$24, %rdx
	cmpl	%r14d, %r9d
	jg	.L124
.L125:
	movl	248(%rsp), %ebp
	movl	$4294967295, %r9d
	movl	$2, 28(%rsp)
	vmovq	%r9, %xmm7
	vmovdqa	.LC24(%rip), %ymm5
	vmovdqa	.LC14(%rip), %ymm6
	vpbroadcastq	%xmm7, %ymm7
	vmovdqa	.LC25(%rip), %ymm8
	vmovdqa	.LC18(%rip), %ymm9
	sarl	%ebp
	vpbroadcastq	.LC31(%rip), %ymm3
.L144:
	movslq	28(%rsp), %rdx
	xorl	%r14d, %r14d
	xorl	%esi, %esi
	xorl	%r12d, %r12d
	movl	%edx, %edi
	movq	%rdx, 16(%rsp)
	sarl	%edi
	leal	-4(%rdi), %r13d
	movl	%r13d, 24(%rsp)
	andl	$-4, %r13d
	addl	$4, %r13d
	cmpl	$3, %edi
	cmovle	%r14d, %r13d
	jle	.L127
	.p2align 4,,10
	.p2align 3
.L141:
	movl	24(%rsp), %r10d
	movl	$1004535809, %eax
	movslq	%ebp, %r14
	leal	(%rbp,%rbp), %r8d
	vmovd	%eax, %xmm2
	vpbroadcastq	.LC32(%rip), %ymm1
	leal	0(,%rbp,4), %ecx
	movslq	%r8d, %r11
	addl	%ebp, %r8d
	movslq	%r8d, %r9
	movl	$1004535808, %r8d
	movslq	%ecx, %r15
	shrl	$2, %r10d
	vmovd	%r8d, %xmm4
	salq	$2, %r15
	leal	0(,%r10,4), %edx
	movslq	%edi, %rcx
	vpbroadcastd	%xmm2, %xmm2
	movq	%rdx, 8(%rsp)
	vpbroadcastd	%xmm4, %xmm4
.L143:
	movq	8(%rsp), %r10
	leaq	root(%rip), %rdx
	leaq	(%rbx,%rsi,4), %rax
	addq	%rsi, %r10
	leaq	16(%rbx,%r10,4), %r8
	.p2align 4,,10
	.p2align 3
.L128:
	vpmovzxdq	(%rax,%rcx,4), %ymm0
	vmovd	(%rdx,%r11,4), %xmm10
	vpinsrd	$1, (%rdx,%r9,4), %xmm10, %xmm11
	vmovd	(%rdx), %xmm12
	vpinsrd	$1, (%rdx,%r14,4), %xmm12, %xmm13
	vpunpcklqdq	%xmm11, %xmm13, %xmm10
	vpmovzxdq	%xmm10, %ymm11
	vpmuludq	%ymm11, %ymm0, %ymm0
	addq	%r15, %rdx
	vpsrlq	$32, %ymm0, %ymm11
	vpand	%ymm7, %ymm0, %ymm13
	vpmuludq	%ymm5, %ymm13, %ymm12
	vpmuludq	%ymm6, %ymm13, %ymm10
	vpmuludq	%ymm5, %ymm11, %ymm13
	vpsrlq	$32, %ymm12, %ymm12
	vpmuludq	%ymm6, %ymm11, %ymm11
	vpaddq	%ymm13, %ymm10, %ymm10
	vpaddq	%ymm12, %ymm10, %ymm13
	vpsrlq	$32, %ymm13, %ymm12
	vpaddq	%ymm12, %ymm11, %ymm11
	vpmuludq	%ymm8, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm0
	vpcmpgtq	%ymm3, %ymm0, %ymm13
	vpand	%ymm1, %ymm13, %ymm12
	vpsubq	%ymm12, %ymm0, %ymm11
	vpcmpgtq	%ymm3, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm0
	vpsubq	%ymm0, %ymm11, %ymm13
	vpcmpgtq	%ymm3, %ymm13, %ymm12
	vpand	%ymm1, %ymm12, %ymm11
	vpsubq	%ymm11, %ymm13, %ymm10
	vmovdqa	(%rax), %xmm13
	vpermd	%ymm10, %ymm9, %ymm0
	vpsubd	%xmm0, %xmm2, %xmm11
	vpaddd	%xmm0, %xmm13, %xmm12
	vpaddd	%xmm13, %xmm11, %xmm0
	vpcmpgtd	%xmm4, %xmm12, %xmm10
	vpcmpgtd	%xmm4, %xmm0, %xmm11
	vpand	%xmm2, %xmm10, %xmm13
	vpsubd	%xmm13, %xmm12, %xmm12
	vpand	%xmm2, %xmm11, %xmm10
	vpsubd	%xmm10, %xmm0, %xmm0
	vmovdqa	%xmm12, (%rax)
	vmovdqa	%xmm0, (%rax,%rcx,4)
	addq	$16, %rax
	cmpq	%rax, %r8
	jne	.L128
	cmpl	%r13d, %edi
	jle	.L133
.L130:
	leal	2(%r13), %r14d
	movl	%r13d, %r11d
	leal	1(%r13), %r9d
	imull	%ebp, %r11d
	movl	%r14d, %ecx
	imull	%ebp, %ecx
	movl	%r9d, %r15d
	imull	%ebp, %r15d
	movslq	%r11d, %r8
	movl	%ecx, 8(%rsp)
.L142:
	leal	0(%r13,%r12), %eax
	movslq	%eax, %rdx
	addl	%edi, %eax
	leaq	(%rbx,%rdx,4), %r11
	cltq
	leaq	(%rbx,%rax,4), %r10
	leaq	root(%rip), %rax
	movl	(%r10), %ecx
	movl	(%rax,%r8,4), %edx
	movabsq	$1270861263111332585, %rax
	imulq	%rdx, %rcx
	mulq	%rcx
	movq	%rcx, %rax
	subq	%rdx, %rax
	shrq	%rax
	addq	%rax, %rdx
	movl	(%r11), %eax
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rdx
	subq	%rdx, %rcx
	addl	%ecx, %eax
	addl	%ecx, %ecx
	movl	%eax, %edx
	subl	%ecx, %edx
	cmpl	$1004535808, %eax
	leal	-1004535809(%rax), %ecx
	cmovbe	%eax, %ecx
	cmpl	$-1004535810, %edx
	leal	1004535809(%rdx), %eax
	movl	%ecx, (%r11)
	cmovbe	%edx, %eax
	cmpl	%r9d, %edi
	movl	%eax, (%r10)
	jle	.L136
	leal	(%r9,%r12), %eax
	movslq	%r15d, %rdx
	leaq	root(%rip), %rcx
	movslq	%eax, %r11
	addl	%edi, %eax
	movl	(%rcx,%rdx,4), %ecx
	leaq	(%rbx,%r11,4), %r11
	cltq
	leaq	(%rbx,%rax,4), %r10
	movl	(%r10), %eax
	imulq	%rax, %rcx
	movabsq	$1270861263111332585, %rax
	mulq	%rcx
	movq	%rcx, %rax
	subq	%rdx, %rax
	shrq	%rax
	addq	%rax, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rdx
	subq	%rdx, %rcx
	movl	(%r11), %edx
	leal	(%rcx,%rcx), %eax
	addl	%ecx, %edx
	movl	%edx, %ecx
	subl	%eax, %ecx
	cmpl	$1004535808, %edx
	leal	-1004535809(%rdx), %eax
	cmova	%eax, %edx
	cmpl	$-1004535810, %ecx
	leal	1004535809(%rcx), %eax
	movl	%edx, (%r11)
	cmovbe	%ecx, %eax
	cmpl	%edi, %r14d
	movl	%eax, (%r10)
	jge	.L136
	movslq	8(%rsp), %rdx
	leaq	root(%rip), %rcx
	addl	%r14d, %r12d
	movslq	%r12d, %r11
	addl	%edi, %r12d
	leaq	(%rbx,%r11,4), %r11
	movslq	%r12d, %r12
	leaq	(%rbx,%r12,4), %r10
	movl	(%r10), %eax
	movl	(%rcx,%rdx,4), %r12d
	imulq	%rax, %r12
	movabsq	$1270861263111332585, %rax
	mulq	%r12
	movq	%r12, %rcx
	subq	%rdx, %rcx
	shrq	%rcx
	addq	%rcx, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rax
	movl	(%r11), %edx
	subq	%rax, %r12
	addl	%r12d, %edx
	leal	(%r12,%r12), %r12d
	movl	%edx, %ecx
	leal	-1004535809(%rdx), %eax
	subl	%r12d, %ecx
	cmpl	$1004535808, %edx
	cmova	%eax, %edx
	cmpl	$-1004535810, %ecx
	leal	1004535809(%rcx), %r12d
	movl	%edx, (%r11)
	cmovbe	%ecx, %r12d
	movl	%r12d, (%r10)
.L136:
	addq	16(%rsp), %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L131
	cmpl	$3, %edi
	movl	%esi, %r12d
	jg	.L141
	jmp	.L142
.L127:
	testl	%edi, %edi
	jg	.L130
.L181:
	movq	16(%rsp), %rax
	addq	%rax, %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L131
	addq	%rax, %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L131
	testl	%edi, %edi
	movl	%esi, %r12d
	jle	.L181
	jmp	.L130
.L133:
	addq	16(%rsp), %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L131
	movl	%esi, %r12d
	jmp	.L143
.L131:
	sall	28(%rsp)
	sarl	%ebp
	movl	28(%rsp), %edi
	cmpl	%edi, 248(%rsp)
	jge	.L144
	vzeroupper
.L179:
	vmovaps	32(%rsp), %xmm6
	vmovaps	48(%rsp), %xmm7
	vmovaps	64(%rsp), %xmm8
	vmovaps	80(%rsp), %xmm9
	vmovaps	96(%rsp), %xmm10
	vmovaps	112(%rsp), %xmm11
	vmovaps	128(%rsp), %xmm12
	vmovaps	144(%rsp), %xmm13
	addq	$168, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L180:
	movl	(%rbx,%r10,4), %r12d
	movl	8(%rbx), %r11d
	movl	%r12d, 8(%rbx)
	movl	8(%rdx), %r13d
	movl	%r11d, (%rbx,%r13,4)
	jmp	.L151
.L121:
	jne	.L179
	jmp	.L125
	.seh_endproc
	.p2align 4
	.def	ntt3;	.scl	3;	.type	32;	.endef
	.seh_proc	ntt3
ntt3:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$168, %rsp
	.seh_stackalloc	168
	vmovaps	%xmm6, 32(%rsp)
	.seh_savexmm	%xmm6, 32
	vmovaps	%xmm7, 48(%rsp)
	.seh_savexmm	%xmm7, 48
	vmovaps	%xmm8, 64(%rsp)
	.seh_savexmm	%xmm8, 64
	vmovaps	%xmm9, 80(%rsp)
	.seh_savexmm	%xmm9, 80
	vmovaps	%xmm10, 96(%rsp)
	.seh_savexmm	%xmm10, 96
	vmovaps	%xmm11, 112(%rsp)
	.seh_savexmm	%xmm11, 112
	vmovaps	%xmm12, 128(%rsp)
	.seh_savexmm	%xmm12, 128
	vmovaps	%xmm13, 144(%rsp)
	.seh_savexmm	%xmm13, 144
	.seh_endprologue
	cmpl	$2, %edx
	movq	%rcx, %rbx
	movl	%edx, 248(%rsp)
	jle	.L183
	leal	-1(%rdx), %r9d
	movl	%edx, %esi
	movl	$1, %r14d
	leal	-3(%rsi), %ecx
	leaq	rev(%rip), %rdx
	shrl	%ecx
	addl	$1, %ecx
	andl	$3, %ecx
	je	.L186
	cmpl	$1, %ecx
	je	.L233
	cmpl	$2, %ecx
	je	.L234
	movl	4(%rdx), %eax
	cmpl	$1, %eax
	jnb	.L212
	movl	(%rbx,%rax,4), %ebp
	movl	4(%rbx), %edi
	movl	%ebp, 4(%rbx)
	movl	4(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L212:
	movl	8(%rdx), %r10d
	cmpl	$2, %r10d
	jb	.L242
.L213:
	movl	$3, %r14d
	addq	$8, %rdx
.L234:
	movl	4(%rdx), %r15d
	movl	%r14d, %esi
	cmpl	%r14d, %r15d
	jnb	.L216
	movl	(%rbx,%r15,4), %ecx
	movl	(%rbx,%r14,4), %eax
	movl	%ecx, (%rbx,%r14,4)
	movl	4(%rdx), %edi
	movl	%eax, (%rbx,%rdi,4)
.L216:
	movl	8(%rdx), %ebp
	addl	$1, %esi
	cmpl	%esi, %ebp
	jnb	.L217
	movl	(%rbx,%rbp,4), %r10d
	movl	4(%rbx,%r14,4), %r8d
	movl	%r10d, 4(%rbx,%r14,4)
	movl	8(%rdx), %r11d
	movl	%r8d, (%rbx,%r11,4)
.L217:
	addq	$2, %r14
	addq	$8, %rdx
.L233:
	movl	4(%rdx), %r12d
	movl	%r14d, %r13d
	cmpl	%r14d, %r12d
	jnb	.L220
	movl	(%rbx,%r12,4), %esi
	movl	(%rbx,%r14,4), %r15d
	movl	%esi, (%rbx,%r14,4)
	movl	4(%rdx), %eax
	movl	%r15d, (%rbx,%rax,4)
.L220:
	movl	8(%rdx), %ecx
	addl	$1, %r13d
	cmpl	%r13d, %ecx
	jnb	.L221
	movl	(%rbx,%rcx,4), %ebp
	movl	4(%rbx,%r14,4), %edi
	movl	%ebp, 4(%rbx,%r14,4)
	movl	8(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L221:
	addq	$2, %r14
	addq	$8, %rdx
	cmpl	%r14d, %r9d
	jle	.L187
.L186:
	movl	4(%rdx), %r11d
	movl	%r14d, %r12d
	cmpl	%r14d, %r11d
	jnb	.L184
	movl	(%rbx,%r11,4), %r13d
	movl	(%rbx,%r14,4), %r10d
	movl	%r13d, (%rbx,%r14,4)
	movl	4(%rdx), %r15d
	movl	%r10d, (%rbx,%r15,4)
.L184:
	movl	8(%rdx), %esi
	addl	$1, %r12d
	cmpl	%r12d, %esi
	jnb	.L185
	movl	(%rbx,%rsi,4), %ecx
	movl	4(%rbx,%r14,4), %eax
	movl	%ecx, 4(%rbx,%r14,4)
	movl	8(%rdx), %edi
	movl	%eax, (%rbx,%rdi,4)
.L185:
	movl	12(%rdx), %ebp
	addq	$2, %r14
	addq	$8, %rdx
	cmpl	%r14d, %ebp
	jnb	.L224
	movl	(%rbx,%rbp,4), %r11d
	movl	(%rbx,%r14,4), %r8d
	movl	%r11d, (%rbx,%r14,4)
	movl	4(%rdx), %r12d
	movl	%r8d, (%rbx,%r12,4)
.L224:
	movl	8(%rdx), %r10d
	leal	1(%r14), %r13d
	cmpl	%r13d, %r10d
	jnb	.L225
	movl	(%rbx,%r10,4), %esi
	movl	4(%rbx,%r14,4), %r15d
	movl	%esi, 4(%rbx,%r14,4)
	movl	8(%rdx), %eax
	movl	%r15d, (%rbx,%rax,4)
.L225:
	movl	12(%rdx), %edi
	leaq	2(%r14), %rcx
	cmpl	%ecx, %edi
	jnb	.L227
	movl	(%rbx,%rdi,4), %r8d
	movl	(%rbx,%rcx,4), %ebp
	movl	%r8d, (%rbx,%rcx,4)
	movl	12(%rdx), %r11d
	movl	%ebp, (%rbx,%r11,4)
.L227:
	movl	16(%rdx), %r12d
	leal	1(%rcx), %r10d
	cmpl	%r10d, %r12d
	jnb	.L228
	movl	(%rbx,%r12,4), %r15d
	movl	4(%rbx,%rcx,4), %r13d
	movl	%r15d, 4(%rbx,%rcx,4)
	movl	16(%rdx), %esi
	movl	%r13d, (%rbx,%rsi,4)
.L228:
	movl	20(%rdx), %ecx
	leaq	4(%r14), %rax
	cmpl	%eax, %ecx
	jnb	.L230
	movl	(%rbx,%rcx,4), %ebp
	movl	(%rbx,%rax,4), %edi
	movl	%ebp, (%rbx,%rax,4)
	movl	20(%rdx), %r8d
	movl	%edi, (%rbx,%r8,4)
.L230:
	movl	24(%rdx), %r11d
	leal	1(%rax), %r12d
	cmpl	%r12d, %r11d
	jnb	.L231
	movl	(%rbx,%r11,4), %r13d
	movl	4(%rbx,%rax,4), %r10d
	movl	%r13d, 4(%rbx,%rax,4)
	movl	24(%rdx), %r15d
	movl	%r10d, (%rbx,%r15,4)
.L231:
	addq	$6, %r14
	addq	$24, %rdx
	cmpl	%r14d, %r9d
	jg	.L186
.L187:
	movl	248(%rsp), %ebp
	movl	$4294967295, %r9d
	movl	$2, 28(%rsp)
	vmovq	%r9, %xmm7
	vmovdqa	.LC33(%rip), %ymm5
	vmovdqa	.LC34(%rip), %ymm6
	vpbroadcastq	%xmm7, %ymm7
	vmovdqa	.LC35(%rip), %ymm8
	vmovdqa	.LC18(%rip), %ymm9
	sarl	%ebp
	vpbroadcastq	.LC41(%rip), %ymm3
.L206:
	movslq	28(%rsp), %rdx
	xorl	%r14d, %r14d
	xorl	%esi, %esi
	xorl	%r12d, %r12d
	movl	%edx, %edi
	movq	%rdx, 16(%rsp)
	sarl	%edi
	leal	-4(%rdi), %r13d
	movl	%r13d, 24(%rsp)
	andl	$-4, %r13d
	addl	$4, %r13d
	cmpl	$3, %edi
	cmovle	%r14d, %r13d
	jle	.L189
	.p2align 4,,10
	.p2align 3
.L203:
	movl	24(%rsp), %r10d
	movl	$469762049, %eax
	movslq	%ebp, %r14
	leal	(%rbp,%rbp), %r8d
	vmovd	%eax, %xmm2
	vpbroadcastq	.LC42(%rip), %ymm1
	leal	0(,%rbp,4), %ecx
	movslq	%r8d, %r11
	addl	%ebp, %r8d
	movslq	%r8d, %r9
	movl	$469762048, %r8d
	movslq	%ecx, %r15
	shrl	$2, %r10d
	vmovd	%r8d, %xmm4
	salq	$2, %r15
	leal	0(,%r10,4), %edx
	movslq	%edi, %rcx
	vpbroadcastd	%xmm2, %xmm2
	movq	%rdx, 8(%rsp)
	vpbroadcastd	%xmm4, %xmm4
.L205:
	movq	8(%rsp), %r10
	leaq	root(%rip), %rdx
	leaq	(%rbx,%rsi,4), %rax
	addq	%rsi, %r10
	leaq	16(%rbx,%r10,4), %r8
	.p2align 4,,10
	.p2align 3
.L190:
	vpmovzxdq	(%rax,%rcx,4), %ymm0
	vmovd	(%rdx,%r11,4), %xmm10
	vpinsrd	$1, (%rdx,%r9,4), %xmm10, %xmm11
	vmovd	(%rdx), %xmm12
	vpinsrd	$1, (%rdx,%r14,4), %xmm12, %xmm13
	vpunpcklqdq	%xmm11, %xmm13, %xmm10
	vpmovzxdq	%xmm10, %ymm11
	vpmuludq	%ymm11, %ymm0, %ymm0
	addq	%r15, %rdx
	vpsrlq	$32, %ymm0, %ymm11
	vpand	%ymm7, %ymm0, %ymm13
	vpmuludq	%ymm5, %ymm13, %ymm12
	vpmuludq	%ymm6, %ymm13, %ymm10
	vpmuludq	%ymm5, %ymm11, %ymm13
	vpsrlq	$32, %ymm12, %ymm12
	vpmuludq	%ymm6, %ymm11, %ymm11
	vpaddq	%ymm13, %ymm10, %ymm10
	vpaddq	%ymm12, %ymm10, %ymm13
	vpsrlq	$32, %ymm13, %ymm12
	vpaddq	%ymm12, %ymm11, %ymm11
	vpmuludq	%ymm8, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm0
	vpcmpgtq	%ymm3, %ymm0, %ymm13
	vpand	%ymm1, %ymm13, %ymm12
	vpsubq	%ymm12, %ymm0, %ymm11
	vpcmpgtq	%ymm3, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm0
	vpsubq	%ymm0, %ymm11, %ymm13
	vpcmpgtq	%ymm3, %ymm13, %ymm12
	vpand	%ymm1, %ymm12, %ymm11
	vpsubq	%ymm11, %ymm13, %ymm10
	vmovdqa	(%rax), %xmm13
	vpermd	%ymm10, %ymm9, %ymm0
	vpsubd	%xmm0, %xmm2, %xmm11
	vpaddd	%xmm0, %xmm13, %xmm12
	vpaddd	%xmm13, %xmm11, %xmm0
	vpcmpgtd	%xmm4, %xmm12, %xmm10
	vpcmpgtd	%xmm4, %xmm0, %xmm11
	vpand	%xmm2, %xmm10, %xmm13
	vpsubd	%xmm13, %xmm12, %xmm12
	vpand	%xmm2, %xmm11, %xmm10
	vpsubd	%xmm10, %xmm0, %xmm0
	vmovdqa	%xmm12, (%rax)
	vmovdqa	%xmm0, (%rax,%rcx,4)
	addq	$16, %rax
	cmpq	%rax, %r8
	jne	.L190
	cmpl	%r13d, %edi
	jle	.L195
.L192:
	leal	2(%r13), %r14d
	movl	%r13d, %r11d
	leal	1(%r13), %r9d
	imull	%ebp, %r11d
	movl	%r14d, %ecx
	imull	%ebp, %ecx
	movl	%r9d, %r15d
	imull	%ebp, %r15d
	movslq	%r11d, %r8
	movl	%ecx, 8(%rsp)
.L204:
	leal	0(%r13,%r12), %eax
	movslq	%eax, %rdx
	addl	%edi, %eax
	leaq	(%rbx,%rdx,4), %r11
	cltq
	leaq	(%rbx,%rax,4), %r10
	leaq	root(%rip), %rax
	movl	(%r10), %ecx
	movl	(%rax,%r8,4), %edx
	movabsq	$2635249108509053275, %rax
	imulq	%rdx, %rcx
	mulq	%rcx
	movq	%rcx, %rax
	subq	%rdx, %rax
	shrq	%rax
	addq	%rax, %rdx
	movl	(%r11), %eax
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %rdx
	subq	%rdx, %rcx
	addl	%ecx, %eax
	addl	%ecx, %ecx
	movl	%eax, %edx
	subl	%ecx, %edx
	cmpl	$469762048, %eax
	leal	-469762049(%rax), %ecx
	cmovbe	%eax, %ecx
	cmpl	$-469762050, %edx
	leal	469762049(%rdx), %eax
	movl	%ecx, (%r11)
	cmovbe	%edx, %eax
	cmpl	%r9d, %edi
	movl	%eax, (%r10)
	jle	.L198
	leal	(%r9,%r12), %eax
	movslq	%r15d, %rdx
	leaq	root(%rip), %rcx
	movslq	%eax, %r11
	addl	%edi, %eax
	movl	(%rcx,%rdx,4), %ecx
	leaq	(%rbx,%r11,4), %r11
	cltq
	leaq	(%rbx,%rax,4), %r10
	movl	(%r10), %eax
	imulq	%rax, %rcx
	movabsq	$2635249108509053275, %rax
	mulq	%rcx
	movq	%rcx, %rax
	subq	%rdx, %rax
	shrq	%rax
	addq	%rax, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %rdx
	subq	%rdx, %rcx
	movl	(%r11), %edx
	leal	(%rcx,%rcx), %eax
	addl	%ecx, %edx
	movl	%edx, %ecx
	subl	%eax, %ecx
	cmpl	$469762048, %edx
	leal	-469762049(%rdx), %eax
	cmova	%eax, %edx
	cmpl	$-469762050, %ecx
	leal	469762049(%rcx), %eax
	movl	%edx, (%r11)
	cmovbe	%ecx, %eax
	cmpl	%edi, %r14d
	movl	%eax, (%r10)
	jge	.L198
	movslq	8(%rsp), %rdx
	leaq	root(%rip), %rcx
	addl	%r14d, %r12d
	movslq	%r12d, %r11
	addl	%edi, %r12d
	leaq	(%rbx,%r11,4), %r11
	movslq	%r12d, %r12
	leaq	(%rbx,%r12,4), %r10
	movl	(%r10), %eax
	movl	(%rcx,%rdx,4), %r12d
	imulq	%rax, %r12
	movabsq	$2635249108509053275, %rax
	mulq	%r12
	movq	%r12, %rcx
	subq	%rdx, %rcx
	shrq	%rcx
	addq	%rcx, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %rax
	movl	(%r11), %edx
	subq	%rax, %r12
	addl	%r12d, %edx
	leal	(%r12,%r12), %r12d
	movl	%edx, %ecx
	leal	-469762049(%rdx), %eax
	subl	%r12d, %ecx
	cmpl	$469762048, %edx
	cmova	%eax, %edx
	cmpl	$-469762050, %ecx
	leal	469762049(%rcx), %r12d
	movl	%edx, (%r11)
	cmovbe	%ecx, %r12d
	movl	%r12d, (%r10)
.L198:
	addq	16(%rsp), %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L193
	cmpl	$3, %edi
	movl	%esi, %r12d
	jg	.L203
	jmp	.L204
.L189:
	testl	%edi, %edi
	jg	.L192
.L243:
	movq	16(%rsp), %rax
	addq	%rax, %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L193
	addq	%rax, %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L193
	testl	%edi, %edi
	movl	%esi, %r12d
	jle	.L243
	jmp	.L192
.L195:
	addq	16(%rsp), %rsi
	cmpl	%esi, 248(%rsp)
	jle	.L193
	movl	%esi, %r12d
	jmp	.L205
.L193:
	sall	28(%rsp)
	sarl	%ebp
	movl	28(%rsp), %edi
	cmpl	%edi, 248(%rsp)
	jge	.L206
	vzeroupper
.L241:
	vmovaps	32(%rsp), %xmm6
	vmovaps	48(%rsp), %xmm7
	vmovaps	64(%rsp), %xmm8
	vmovaps	80(%rsp), %xmm9
	vmovaps	96(%rsp), %xmm10
	vmovaps	112(%rsp), %xmm11
	vmovaps	128(%rsp), %xmm12
	vmovaps	144(%rsp), %xmm13
	addq	$168, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L242:
	movl	(%rbx,%r10,4), %r12d
	movl	8(%rbx), %r11d
	movl	%r12d, 8(%rbx)
	movl	8(%rdx), %r13d
	movl	%r11d, (%rbx,%r13,4)
	jmp	.L213
.L183:
	jne	.L241
	jmp	.L187
	.seh_endproc
	.p2align 4
	.def	conv1;	.scl	3;	.type	32;	.endef
	.seh_proc	conv1
conv1:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$152, %rsp
	.seh_stackalloc	152
	vmovaps	%xmm6, 48(%rsp)
	.seh_savexmm	%xmm6, 48
	vmovaps	%xmm7, 64(%rsp)
	.seh_savexmm	%xmm7, 64
	vmovaps	%xmm8, 80(%rsp)
	.seh_savexmm	%xmm8, 80
	vmovaps	%xmm9, 96(%rsp)
	.seh_savexmm	%xmm9, 96
	vmovaps	%xmm10, 112(%rsp)
	.seh_savexmm	%xmm10, 112
	vmovaps	%xmm11, 128(%rsp)
	.seh_savexmm	%xmm11, 128
	.seh_endprologue
	movl	$998244352, %eax
	movslq	%edx, %rsi
	xorl	%edx, %edx
	movq	%rcx, %rdi
	divl	%esi
	cmpl	$998244352, %esi
	movl	%esi, %ebx
	movl	%eax, %r12d
	ja	.L271
	movl	%eax, %r9d
	movl	$3, %ecx
	movl	$1, %r8d
	movabsq	$-8525806094425994177, %r10
	testb	$1, %r9b
	je	.L246
	.p2align 4,,10
	.p2align 3
.L247:
	imulq	%rcx, %r8
	movq	%r8, %rax
	mulq	%r10
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subl	%r11d, %r8d
	shrl	%r9d
	je	.L245
	imulq	%rcx, %rcx
	movq	%rcx, %rax
	mulq	%r10
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r15
	subl	%r15d, %ecx
.L270:
	testb	$1, %r9b
	jne	.L247
	imulq	%rcx, %rcx
	shrl	%r9d
	movq	%rcx, %rax
	mulq	%r10
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbp
	subl	%ebp, %ecx
	testb	$1, %r9b
	jne	.L247
.L246:
	imulq	%rcx, %rcx
	shrl	%r9d
	movabsq	$-8525806094425994177, %rax
	mulq	%rcx
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbp
	subl	%ebp, %ecx
	jmp	.L270
.L271:
	movl	$1, %r8d
	.p2align 4,,10
	.p2align 3
.L245:
	movl	$1, root(%rip)
	movl	%esi, %ebp
	sarl	%ebp
	cmpl	$3, %esi
	jle	.L249
	leaq	root(%rip), %r13
	movl	$1, %eax
	movabsq	$-8525806094425994177, %r9
	leal	-2(%rbp), %r14d
	leaq	4(%r13,%r14,4), %r15
	movq	%r15, %rcx
	subq	%r13, %rcx
	subq	$4, %rcx
	shrq	$2, %rcx
	addq	$1, %rcx
	andl	$3, %ecx
	je	.L250
	cmpq	$1, %rcx
	je	.L347
	cmpq	$2, %rcx
	je	.L348
	movq	%r8, %rax
	movq	%r8, %r11
	addq	$4, %r13
	mulq	%r9
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r10
	subq	%r10, %r11
	movl	%r11d, 0(%r13)
	movl	%r11d, %eax
.L348:
	movl	%eax, %r14d
	addq	$4, %r13
	imulq	%r8, %r14
	movq	%r14, %rax
	mulq	%r9
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rcx
	subq	%rcx, %r14
	movl	%r14d, 0(%r13)
	movl	%r14d, %eax
.L347:
	movl	%eax, %r10d
	addq	$4, %r13
	imulq	%r8, %r10
	movq	%r10, %rax
	mulq	%r9
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subq	%r11, %r10
	movl	%r10d, 0(%r13)
	cmpq	%r13, %r15
	movl	%r10d, %eax
	je	.L249
.L250:
	movl	%eax, %r14d
	addq	$16, %r13
	imulq	%r8, %r14
	movq	%r14, %rax
	mulq	%r9
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rcx
	subq	%rcx, %r14
	movl	%r14d, %r10d
	movl	%r14d, -12(%r13)
	imulq	%r8, %r10
	movq	%r10, %rax
	mulq	%r9
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subq	%r11, %r10
	movl	%r10d, %r14d
	movl	%r10d, -8(%r13)
	imulq	%r8, %r14
	movq	%r14, %rax
	mulq	%r9
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rcx
	subq	%rcx, %r14
	movl	%r14d, %r10d
	movl	%r14d, -4(%r13)
	imulq	%r8, %r10
	movq	%r10, %rax
	mulq	%r9
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subq	%r11, %r10
	movl	%r10d, 0(%r13)
	cmpq	%r13, %r15
	movl	%r10d, %eax
	jne	.L250
.L249:
	movl	na(%rip), %r14d
	leaq	a(%rip), %r13
	leaq	A(%rip), %rdx
	movq	%r13, %rcx
	leaq	0(,%r14,4), %r8
	call	memcpy
	leaq	0(%r13,%r14,4), %rcx
	movl	%esi, %r8d
	xorl	%edx, %edx
	subl	%r14d, %r8d
	leaq	0(,%r8,4), %r8
	call	memset
	movl	nb(%rip), %r15d
	leaq	b(%rip), %r14
	leaq	B(%rip), %rdx
	movq	%r14, %rcx
	leaq	0(,%r15,4), %r8
	movl	%r15d, 44(%rsp)
	call	memcpy
	leaq	(%r14,%r15,4), %rcx
	movl	%esi, %r10d
	xorl	%edx, %edx
	subl	44(%rsp), %r10d
	leaq	0(,%r10,4), %r8
	call	memset
	movl	%esi, %edx
	movq	%r13, %rcx
	call	ntt1
	movl	%esi, %edx
	movq	%r14, %rcx
	call	ntt1
	cmpl	$3, %esi
	jle	.L272
	leal	-4(%rsi), %ecx
	movl	$4294967295, %r8d
	xorl	%r15d, %r15d
	vmovdqa	.LC13(%rip), %ymm4
	movl	%ecx, %eax
	vmovq	%r8, %xmm5
	vmovdqa	.LC14(%rip), %ymm3
	shrl	$2, %eax
	vpbroadcastq	%xmm5, %ymm5
	vmovdqa	.LC15(%rip), %ymm7
	leal	1(%rax), %r11d
	vmovdqa	.LC18(%rip), %ymm6
	vpbroadcastq	.LC22(%rip), %ymm2
	movq	%r11, %rdx
	vpbroadcastq	.LC23(%rip), %ymm1
	salq	$4, %rdx
	andl	$1, %r11d
	je	.L252
	vpmovzxdq	(%r14), %ymm8
	movl	$16, %r15d
	vpmovzxdq	0(%r13), %ymm0
	cmpq	%rdx, %r15
	vpmuludq	%ymm8, %ymm0, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm11
	vpmuludq	%ymm4, %ymm11, %ymm10
	vpmuludq	%ymm3, %ymm11, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpsrlq	$32, %ymm10, %ymm10
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, (%rdi)
	je	.L391
	.p2align 4,,10
	.p2align 3
.L252:
	vpmovzxdq	0(%r13,%r15), %ymm11
	vpmovzxdq	(%r14,%r15), %ymm10
	vpmuludq	%ymm10, %ymm11, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpmuludq	%ymm4, %ymm8, %ymm10
	vpmuludq	%ymm3, %ymm8, %ymm8
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpsrlq	$32, %ymm10, %ymm10
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, (%rdi,%r15)
	vpmovzxdq	16(%r13,%r15), %ymm11
	vpmovzxdq	16(%r14,%r15), %ymm10
	vpmuludq	%ymm10, %ymm11, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpmuludq	%ymm4, %ymm8, %ymm10
	vpmuludq	%ymm3, %ymm8, %ymm8
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpsrlq	$32, %ymm10, %ymm10
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, 16(%rdi,%r15)
	addq	$32, %r15
	cmpq	%rdx, %r15
	jne	.L252
.L391:
	andl	$-4, %ecx
	addl	$4, %ecx
	vzeroupper
.L251:
	cmpl	%ecx, %esi
	jle	.L257
	movabsq	$-8525806094425994177, %r15
	movslq	%ecx, %r10
	movl	(%r14,%r10,4), %r11d
	movl	0(%r13,%r10,4), %r9d
	imulq	%r9, %r11
	movq	%r11, %rax
	mulq	%r15
	leal	1(%rcx), %eax
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r8
	subq	%r8, %r11
	cmpl	%eax, %esi
	movl	%r11d, (%rdi,%r10,4)
	jle	.L257
	cltq
	addl	$2, %ecx
	movl	0(%r13,%rax,4), %r11d
	movl	(%r14,%rax,4), %r9d
	imulq	%r9, %r11
	movq	%r11, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r8
	subq	%r8, %r11
	cmpl	%ecx, %esi
	movl	%r11d, 4(%rdi,%r10,4)
	jle	.L257
	movslq	%ecx, %rcx
	movl	0(%r13,%rcx,4), %eax
	movl	(%r14,%rcx,4), %r13d
	imulq	%r13, %rax
	movq	%rax, %r14
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r15
	subq	%r15, %r14
	movl	%r14d, 8(%rdi,%r10,4)
.L257:
	cmpl	$998244352, %esi
	movl	$3, %r10d
	movl	$1, %ecx
	movabsq	$-8525806094425994177, %r11
	ja	.L255
	testb	$1, %r12b
	je	.L258
	.p2align 4,,10
	.p2align 3
.L259:
	imulq	%r10, %rcx
	movq	%rcx, %rax
	mulq	%r11
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r8
	subl	%r8d, %ecx
	shrl	%r12d
	je	.L255
	imulq	%r10, %r10
	movq	%r10, %rax
	mulq	%r11
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r14
	subl	%r14d, %r10d
.L269:
	testb	$1, %r12b
	jne	.L259
	imulq	%r10, %r10
	shrl	%r12d
	movq	%r10, %rax
	mulq	%r11
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r13
	subl	%r13d, %r10d
	testb	$1, %r12b
	jne	.L259
.L258:
	imulq	%r10, %r10
	shrl	%r12d
	movabsq	$-8525806094425994177, %rax
	mulq	%r10
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r9
	subl	%r9d, %r10d
	jmp	.L269
	.p2align 4,,10
	.p2align 3
.L255:
	movl	$30, %r13d
	movl	$1, %r14d
	movl	$998244351, %r10d
	movabsq	$-8525806094425994177, %r15
	.p2align 4,,10
	.p2align 3
.L261:
	testb	$1, %r10b
	je	.L260
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r12
	subl	%r12d, %r14d
.L260:
	imulq	%rcx, %rcx
	movl	%r10d, %r11d
	subl	$1, %r13d
	shrl	%r11d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r9
	subl	%r9d, %ecx
	andl	$2, %r10d
	je	.L327
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r10
	subl	%r10d, %r14d
.L327:
	imulq	%rcx, %rcx
	movl	%r11d, %r8d
	shrl	%r8d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r12
	subl	%r12d, %ecx
	andl	$2, %r11d
	je	.L329
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subl	%r11d, %r14d
.L329:
	imulq	%rcx, %rcx
	movl	%r8d, %r9d
	shrl	%r9d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r10
	subl	%r10d, %ecx
	andl	$2, %r8d
	je	.L331
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r8
	subl	%r8d, %r14d
.L331:
	imulq	%rcx, %rcx
	movl	%r9d, %r10d
	shrl	%r10d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r12
	subl	%r12d, %ecx
	andl	$2, %r9d
	je	.L333
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subl	%r11d, %r14d
.L333:
	imulq	%rcx, %rcx
	shrl	%r10d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r9
	subl	%r9d, %ecx
	subl	$4, %r13d
	jne	.L261
	movl	$1, root(%rip)
	cmpl	$1, %ebp
	jle	.L262
	leaq	root(%rip), %r13
	movl	%r14d, %ecx
	movl	$1, %r10d
	leal	-2(%rbp), %ebp
	movabsq	$-8525806094425994177, %r15
	leaq	4(%r13,%rbp,4), %r14
	movq	%r14, %r8
	subq	%r13, %r8
	subq	$4, %r8
	shrq	$2, %r8
	addq	$1, %r8
	andl	$3, %r8d
	je	.L263
	cmpq	$1, %r8
	je	.L349
	cmpq	$2, %r8
	je	.L350
	movq	%rcx, %rax
	movq	%rcx, %r12
	addq	$4, %r13
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r10
	subq	%r10, %r12
	movl	%r12d, 0(%r13)
	movl	%r12d, %r10d
.L350:
	movl	%r10d, %r11d
	addq	$4, %r13
	imulq	%rcx, %r11
	movq	%r11, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r9
	subq	%r9, %r11
	movl	%r11d, 0(%r13)
	movl	%r11d, %r10d
.L349:
	movl	%r10d, %ebp
	addq	$4, %r13
	imulq	%rcx, %rbp
	movq	%rbp, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r8
	subq	%r8, %rbp
	movl	%ebp, 0(%r13)
	cmpq	%r13, %r14
	movl	%ebp, %r10d
	je	.L262
.L263:
	movl	%r10d, %r12d
	addq	$16, %r13
	imulq	%rcx, %r12
	movq	%r12, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subq	%r11, %r12
	movl	%r12d, %r9d
	movl	%r12d, -12(%r13)
	imulq	%rcx, %r9
	movq	%r9, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbp
	subq	%rbp, %r9
	movl	%r9d, %r8d
	movl	%r9d, -8(%r13)
	imulq	%rcx, %r8
	movq	%r8, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r10
	subq	%r10, %r8
	movl	%r8d, %r12d
	movl	%r8d, -4(%r13)
	imulq	%rcx, %r12
	movq	%r12, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subq	%r11, %r12
	movl	%r12d, 0(%r13)
	cmpq	%r13, %r14
	movl	%r12d, %r10d
	jne	.L263
.L262:
	movl	%esi, %edx
	movq	%rdi, %rcx
	movl	$30, %r13d
	call	ntt1
	movl	$1, %r14d
	movl	$998244351, %r8d
	movabsq	$-8525806094425994177, %r15
	.p2align 4,,10
	.p2align 3
.L265:
	testb	$1, %r8b
	movl	%ebx, %ecx
	je	.L264
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbx
	subl	%ebx, %r14d
.L264:
	imulq	%rcx, %rcx
	movl	%r8d, %r9d
	subl	$1, %r13d
	shrl	%r9d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbp
	subl	%ebp, %ecx
	andl	$2, %r8d
	je	.L310
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r8
	subl	%r8d, %r14d
.L310:
	imulq	%rcx, %rcx
	movl	%r9d, %r10d
	shrl	%r10d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r12
	subl	%r12d, %ecx
	andl	$2, %r9d
	je	.L312
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subl	%r11d, %r14d
.L312:
	imulq	%rcx, %rcx
	movl	%r10d, %ebx
	shrl	%ebx
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r9
	subl	%r9d, %ecx
	andl	$2, %r10d
	je	.L314
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbp
	subl	%ebp, %r14d
.L314:
	imulq	%rcx, %rcx
	movl	%ebx, %r8d
	shrl	%r8d
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r10
	subl	%r10d, %ecx
	andl	$2, %ebx
	je	.L316
	imulq	%rcx, %r14
	movq	%r14, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r12
	subl	%r12d, %r14d
.L316:
	imulq	%rcx, %rcx
	shrl	%r8d
	movq	%rcx, %rax
	movl	%ecx, %ebx
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subl	%r11d, %ebx
	subl	$4, %r13d
	jne	.L265
	testl	%esi, %esi
	jle	.L392
	leaq	(%rdi,%rsi,4), %r13
	movabsq	$-8525806094425994177, %r15
	leaq	-4(,%rsi,4), %rsi
	shrq	$2, %rsi
	addq	$1, %rsi
	andl	$7, %esi
	je	.L267
	cmpq	$1, %rsi
	je	.L351
	cmpq	$2, %rsi
	je	.L352
	cmpq	$3, %rsi
	je	.L353
	cmpq	$4, %rsi
	je	.L354
	cmpq	$5, %rsi
	je	.L355
	cmpq	$6, %rsi
	jne	.L393
.L356:
	movl	(%rdi), %r9d
	addq	$4, %rdi
	imulq	%r14, %r9
	movq	%r9, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbp
	subq	%rbp, %r9
	movl	%r9d, -4(%rdi)
.L355:
	movl	(%rdi), %r8d
	addq	$4, %rdi
	imulq	%r14, %r8
	movq	%r8, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r10
	subq	%r10, %r8
	movl	%r8d, -4(%rdi)
.L354:
	movl	(%rdi), %r12d
	addq	$4, %rdi
	imulq	%r14, %r12
	movq	%r12, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r11
	subq	%r11, %r12
	movl	%r12d, -4(%rdi)
.L353:
	movl	(%rdi), %esi
	addq	$4, %rdi
	imulq	%r14, %rsi
	movq	%rsi, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rcx
	subq	%rcx, %rsi
	movl	%esi, -4(%rdi)
.L352:
	movl	(%rdi), %ebx
	addq	$4, %rdi
	imulq	%r14, %rbx
	movq	%rbx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r9
	subq	%r9, %rbx
	movl	%ebx, -4(%rdi)
.L351:
	movl	(%rdi), %ebp
	addq	$4, %rdi
	imulq	%r14, %rbp
	movq	%rbp, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r8
	subq	%r8, %rbp
	movl	%ebp, -4(%rdi)
	cmpq	%r13, %rdi
	je	.L392
.L267:
	movl	(%rdi), %r10d
	movl	4(%rdi), %r11d
	movl	8(%rdi), %ecx
	movl	12(%rdi), %r9d
	imulq	%r14, %r10
	movl	16(%rdi), %r8d
	imulq	%r14, %r11
	imulq	%r14, %rcx
	imulq	%r14, %r9
	imulq	%r14, %r8
	movq	%r10, %rax
	mulq	%r15
	movq	%r11, %rax
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r12
	mulq	%r15
	movq	%rcx, %rax
	subq	%r12, %r10
	movl	20(%rdi), %r12d
	shrq	$29, %rdx
	movl	%r10d, (%rdi)
	imulq	$998244353, %rdx, %rsi
	mulq	%r15
	movq	%r9, %rax
	imulq	%r14, %r12
	subq	%rsi, %r11
	movl	24(%rdi), %esi
	shrq	$29, %rdx
	movl	%r11d, 4(%rdi)
	imulq	$998244353, %rdx, %rbx
	mulq	%r15
	movq	%r8, %rax
	imulq	%r14, %rsi
	subq	%rbx, %rcx
	movl	28(%rdi), %ebx
	shrq	$29, %rdx
	movl	%ecx, 8(%rdi)
	imulq	$998244353, %rdx, %rbp
	mulq	%r15
	movq	%r12, %rax
	subq	%rbp, %r9
	shrq	$29, %rdx
	movl	%r9d, 12(%rdi)
	imulq	$998244353, %rdx, %r10
	mulq	%r15
	movq	%rsi, %rax
	subq	%r10, %r8
	shrq	$29, %rdx
	movl	%r8d, 16(%rdi)
	imulq	$998244353, %rdx, %r11
	mulq	%r15
	subq	%r11, %r12
	shrq	$29, %rdx
	movl	%r12d, 20(%rdi)
	addq	$32, %rdi
	imulq	%r14, %rbx
	imulq	$998244353, %rdx, %rcx
	movq	%rbx, %rax
	mulq	%r15
	subq	%rcx, %rsi
	movl	%esi, -8(%rdi)
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %r9
	subq	%r9, %rbx
	movl	%ebx, -4(%rdi)
	cmpq	%r13, %rdi
	jne	.L267
.L392:
	vmovaps	48(%rsp), %xmm6
	vmovaps	64(%rsp), %xmm7
	vmovaps	80(%rsp), %xmm8
	vmovaps	96(%rsp), %xmm9
	vmovaps	112(%rsp), %xmm10
	vmovaps	128(%rsp), %xmm11
	addq	$152, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L393:
	movl	(%rdi), %ecx
	addq	$4, %rdi
	imulq	%r14, %rcx
	movq	%rcx, %rax
	mulq	%r15
	shrq	$29, %rdx
	imulq	$998244353, %rdx, %rbx
	subq	%rbx, %rcx
	movl	%ecx, -4(%rdi)
	jmp	.L356
.L272:
	xorl	%ecx, %ecx
	jmp	.L251
	.seh_endproc
	.p2align 4
	.def	conv2;	.scl	3;	.type	32;	.endef
	.seh_proc	conv2
conv2:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$152, %rsp
	.seh_stackalloc	152
	vmovaps	%xmm6, 48(%rsp)
	.seh_savexmm	%xmm6, 48
	vmovaps	%xmm7, 64(%rsp)
	.seh_savexmm	%xmm7, 64
	vmovaps	%xmm8, 80(%rsp)
	.seh_savexmm	%xmm8, 80
	vmovaps	%xmm9, 96(%rsp)
	.seh_savexmm	%xmm9, 96
	vmovaps	%xmm10, 112(%rsp)
	.seh_savexmm	%xmm10, 112
	vmovaps	%xmm11, 128(%rsp)
	.seh_savexmm	%xmm11, 128
	.seh_endprologue
	movl	$1004535808, %eax
	movslq	%edx, %rsi
	xorl	%edx, %edx
	movq	%rcx, %rdi
	divl	%esi
	cmpl	$1004535808, %esi
	movl	%esi, %ebx
	movl	%eax, %r12d
	ja	.L421
	movl	%eax, %r9d
	movl	$3, %ecx
	movl	$1, %r8d
	movabsq	$1270861263111332585, %r10
	testb	$1, %r9b
	je	.L396
	.p2align 4,,10
	.p2align 3
.L397:
	imulq	%rcx, %r8
	movq	%r8, %rax
	movq	%r8, %r13
	mulq	%r10
	subq	%rdx, %r13
	shrq	%r13
	addq	%r13, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subl	%r14d, %r8d
	shrl	%r9d
	je	.L395
	imulq	%rcx, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r11
	mulq	%r10
	subq	%rdx, %r11
	shrq	%r11
	addq	%r11, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rbp
	subl	%ebp, %ecx
.L420:
	testb	$1, %r9b
	jne	.L397
	imulq	%rcx, %rcx
	shrl	%r9d
	movq	%rcx, %rax
	movq	%rcx, %r14
	mulq	%r10
	subq	%rdx, %r14
	shrq	%r14
	addq	%r14, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r13
	subl	%r13d, %ecx
	testb	$1, %r9b
	jne	.L397
.L396:
	imulq	%rcx, %rcx
	shrl	%r9d
	movabsq	$1270861263111332585, %rax
	mulq	%rcx
	movq	%rcx, %rbp
	subq	%rdx, %rbp
	shrq	%rbp
	addq	%rbp, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r11
	subl	%r11d, %ecx
	jmp	.L420
.L421:
	movl	$1, %r8d
	.p2align 4,,10
	.p2align 3
.L395:
	movl	$1, root(%rip)
	movl	%esi, %ebp
	sarl	%ebp
	cmpl	$3, %esi
	jle	.L399
	leaq	root(%rip), %r15
	movl	$1, %r14d
	movabsq	$1270861263111332585, %r10
	leal	-2(%rbp), %r9d
	leaq	4(%r15,%r9,4), %r11
	movq	%r11, %rcx
	subq	%r15, %rcx
	subq	$4, %rcx
	shrq	$2, %rcx
	addq	$1, %rcx
	andl	$3, %ecx
	je	.L400
	cmpq	$1, %rcx
	je	.L477
	cmpq	$2, %rcx
	je	.L478
	movq	%r8, %rax
	movq	%r8, %r13
	movq	%r8, %r9
	mulq	%r10
	addq	$4, %r15
	subq	%rdx, %r13
	shrq	%r13
	addq	%r13, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subq	%r14, %r9
	movl	%r9d, (%r15)
	movl	%r9d, %r14d
.L478:
	movl	%r14d, %ecx
	addq	$4, %r15
	imulq	%r8, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r13
	mulq	%r10
	subq	%rdx, %r13
	shrq	%r13
	addq	%r13, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subq	%r14, %rcx
	movl	%ecx, (%r15)
	movl	%ecx, %r14d
.L477:
	movl	%r14d, %r9d
	addq	$4, %r15
	imulq	%r8, %r9
	movq	%r9, %rax
	movq	%r9, %rcx
	mulq	%r10
	subq	%rdx, %rcx
	shrq	%rcx
	addq	%rcx, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r13
	subq	%r13, %r9
	movl	%r9d, (%r15)
	cmpq	%r15, %r11
	movl	%r9d, %r14d
	je	.L399
.L400:
	movl	%r14d, %r9d
	imulq	%r8, %r9
	movq	%r9, %rax
	movq	%r9, %rcx
	mulq	%r10
	subq	%rdx, %rcx
	shrq	%rcx
	addq	%rcx, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r13
	subq	%r13, %r9
	movl	%r9d, %r14d
	movl	%r9d, 4(%r15)
	imulq	%r8, %r14
	movq	%r14, %rax
	movq	%r14, %r9
	mulq	%r10
	subq	%rdx, %r9
	shrq	%r9
	addq	%r9, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rcx
	subq	%rcx, %r14
	movl	%r14d, %r13d
	movl	%r14d, 8(%r15)
	imulq	%r8, %r13
	movq	%r13, %rax
	movq	%r13, %r14
	mulq	%r10
	subq	%rdx, %r14
	shrq	%r14
	addq	%r14, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r9
	subq	%r9, %r13
	movl	%r13d, %ecx
	movl	%r13d, 12(%r15)
	imulq	%r8, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r13
	mulq	%r10
	subq	%rdx, %r13
	shrq	%r13
	addq	%r13, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subq	%r14, %rcx
	addq	$16, %r15
	movl	%ecx, (%r15)
	cmpq	%r15, %r11
	movl	%ecx, %r14d
	jne	.L400
.L399:
	movl	na(%rip), %r14d
	leaq	a(%rip), %r13
	leaq	A(%rip), %rdx
	movq	%r13, %rcx
	leaq	0(,%r14,4), %r8
	call	memcpy
	leaq	0(%r13,%r14,4), %rcx
	movl	%esi, %r8d
	xorl	%edx, %edx
	subl	%r14d, %r8d
	leaq	0(,%r8,4), %r8
	call	memset
	movl	nb(%rip), %r15d
	leaq	b(%rip), %r14
	leaq	B(%rip), %rdx
	movq	%r14, %rcx
	leaq	0(,%r15,4), %r8
	movl	%r15d, 44(%rsp)
	call	memcpy
	leaq	(%r14,%r15,4), %rcx
	movl	%esi, %r10d
	xorl	%edx, %edx
	subl	44(%rsp), %r10d
	leaq	0(,%r10,4), %r8
	call	memset
	movl	%esi, %edx
	movq	%r13, %rcx
	call	ntt2
	movl	%esi, %edx
	movq	%r14, %rcx
	call	ntt2
	cmpl	$3, %esi
	jle	.L422
	leal	-4(%rsi), %ecx
	movl	$4294967295, %r15d
	xorl	%eax, %eax
	vmovdqa	.LC24(%rip), %ymm4
	movl	%ecx, %r9d
	vmovq	%r15, %xmm5
	vmovdqa	.LC14(%rip), %ymm3
	shrl	$2, %r9d
	vpbroadcastq	%xmm5, %ymm5
	vmovdqa	.LC25(%rip), %ymm7
	leal	1(%r9), %r8d
	vmovdqa	.LC18(%rip), %ymm6
	vpbroadcastq	.LC31(%rip), %ymm2
	movq	%r8, %rdx
	vpbroadcastq	.LC32(%rip), %ymm1
	salq	$4, %rdx
	andl	$1, %r8d
	je	.L402
	vpmovzxdq	(%r14), %ymm8
	movl	$16, %eax
	vpmovzxdq	0(%r13), %ymm0
	cmpq	%rdx, %rax
	vpmuludq	%ymm8, %ymm0, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm11
	vpmuludq	%ymm4, %ymm11, %ymm10
	vpmuludq	%ymm3, %ymm11, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpsrlq	$32, %ymm10, %ymm10
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, (%rdi)
	je	.L509
	.p2align 4,,10
	.p2align 3
.L402:
	vpmovzxdq	0(%r13,%rax), %ymm11
	vpmovzxdq	(%r14,%rax), %ymm10
	vpmuludq	%ymm10, %ymm11, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpmuludq	%ymm4, %ymm8, %ymm10
	vpmuludq	%ymm3, %ymm8, %ymm8
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpsrlq	$32, %ymm10, %ymm10
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, (%rdi,%rax)
	vpmovzxdq	16(%r13,%rax), %ymm11
	vpmovzxdq	16(%r14,%rax), %ymm10
	vpmuludq	%ymm10, %ymm11, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpmuludq	%ymm4, %ymm8, %ymm10
	vpmuludq	%ymm3, %ymm8, %ymm8
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpsrlq	$32, %ymm10, %ymm10
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, 16(%rdi,%rax)
	addq	$32, %rax
	cmpq	%rdx, %rax
	jne	.L402
.L509:
	andl	$-4, %ecx
	addl	$4, %ecx
	vzeroupper
.L401:
	cmpl	%ecx, %esi
	jle	.L407
	movabsq	$1270861263111332585, %r15
	movslq	%ecx, %r11
	movl	0(%r13,%r11,4), %r9d
	movl	(%r14,%r11,4), %r10d
	imulq	%r9, %r10
	movq	%r10, %rax
	movq	%r10, %r8
	mulq	%r15
	leal	1(%rcx), %eax
	subq	%rdx, %r8
	shrq	%r8
	addq	%r8, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r9
	subq	%r9, %r10
	cmpl	%eax, %esi
	movl	%r10d, (%rdi,%r11,4)
	jle	.L407
	cltq
	addl	$2, %ecx
	movl	(%r14,%rax,4), %edx
	movl	0(%r13,%rax,4), %r10d
	imulq	%rdx, %r10
	movq	%r10, %rax
	movq	%r10, %r8
	mulq	%r15
	subq	%rdx, %r8
	shrq	%r8
	addq	%r8, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r9
	subq	%r9, %r10
	cmpl	%ecx, %esi
	movl	%r10d, 4(%rdi,%r11,4)
	jle	.L407
	movslq	%ecx, %rcx
	movl	0(%r13,%rcx,4), %r13d
	movl	(%r14,%rcx,4), %r14d
	movq	%r13, %r10
	imulq	%r14, %r10
	movq	%r10, %rax
	mulq	%r15
	movq	%r10, %r15
	subq	%rdx, %r15
	shrq	%r15
	addq	%r15, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r8
	subq	%r8, %r10
	movl	%r10d, 8(%rdi,%r11,4)
.L407:
	cmpl	$1004535808, %esi
	movl	$3, %r11d
	movl	$1, %ecx
	movabsq	$1270861263111332585, %r9
	ja	.L405
	testb	$1, %r12b
	je	.L408
	.p2align 4,,10
	.p2align 3
.L409:
	imulq	%r11, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r10
	mulq	%r9
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r15
	subl	%r15d, %ecx
	shrl	%r12d
	je	.L405
	imulq	%r11, %r11
	movq	%r11, %rax
	movq	%r11, %r8
	mulq	%r9
	subq	%rdx, %r8
	shrq	%r8
	addq	%r8, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r13
	subl	%r13d, %r11d
.L419:
	testb	$1, %r12b
	jne	.L409
	imulq	%r11, %r11
	shrl	%r12d
	movq	%r11, %rax
	movq	%r11, %r15
	mulq	%r9
	subq	%rdx, %r15
	shrq	%r15
	addq	%r15, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r10
	subl	%r10d, %r11d
	testb	$1, %r12b
	jne	.L409
.L408:
	imulq	%r11, %r11
	shrl	%r12d
	movabsq	$1270861263111332585, %rax
	mulq	%r11
	movq	%r11, %r13
	subq	%rdx, %r13
	shrq	%r13
	addq	%r13, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subl	%r14d, %r11d
	jmp	.L419
	.p2align 4,,10
	.p2align 3
.L405:
	movl	$30, %r13d
	movl	$1, %r8d
	movl	$1004535807, %r9d
	movabsq	$1270861263111332585, %r11
	.p2align 4,,10
	.p2align 3
.L411:
	testb	$1, %r9b
	je	.L410
	imulq	%rcx, %r8
	movq	%r8, %rax
	movq	%r8, %r12
	mulq	%r11
	subq	%rdx, %r12
	shrq	%r12
	addq	%r12, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subl	%r14d, %r8d
.L410:
	imulq	%rcx, %rcx
	movl	%r9d, %r10d
	subl	$1, %r13d
	shrl	%r10d
	movq	%rcx, %rax
	movq	%rcx, %r15
	mulq	%r11
	subq	%rdx, %r15
	shrq	%r15
	addq	%r15, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r12
	subl	%r12d, %ecx
	andl	$2, %r9d
	je	.L461
	imulq	%rcx, %r8
	movq	%r8, %rax
	movq	%r8, %r9
	mulq	%r11
	subq	%rdx, %r9
	shrq	%r9
	addq	%r9, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subl	%r14d, %r8d
.L461:
	imulq	%rcx, %rcx
	movl	%r10d, %r9d
	shrl	%r9d
	movq	%rcx, %rax
	movq	%rcx, %r15
	mulq	%r11
	subq	%rdx, %r15
	shrq	%r15
	addq	%r15, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r12
	subl	%r12d, %ecx
	andl	$2, %r10d
	je	.L463
	imulq	%rcx, %r8
	movq	%r8, %rax
	movq	%r8, %r10
	mulq	%r11
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subl	%r14d, %r8d
.L463:
	imulq	%rcx, %rcx
	shrl	%r9d
	movq	%rcx, %rax
	movq	%rcx, %r15
	mulq	%r11
	subq	%rdx, %r15
	shrq	%r15
	addq	%r15, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r12
	subl	%r12d, %ecx
	subl	$2, %r13d
	jne	.L411
	movl	$1, root(%rip)
	cmpl	$1, %ebp
	jle	.L412
	leaq	root(%rip), %r9
	movabsq	$1270861263111332585, %r11
	leal	-2(%rbp), %ecx
	leaq	4(%r9), %rbp
	leaq	0(%rbp,%rcx,4), %r13
	movl	$1, %ecx
	movq	%r13, %r10
	subq	%r9, %r10
	subq	$4, %r10
	shrq	$2, %r10
	addq	$1, %r10
	andl	$3, %r10d
	je	.L413
	cmpq	$1, %r10
	je	.L479
	cmpq	$2, %r10
	je	.L480
	movq	%r8, %rax
	movq	%r8, %r14
	movq	%r8, %r12
	mulq	%r11
	subq	%rdx, %r14
	shrq	%r14
	addq	%r14, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r15
	subq	%r15, %r12
	movl	%r12d, 4(%r9)
	movl	%r12d, %ecx
	movq	%rbp, %r9
.L480:
	imulq	%r8, %rcx
	addq	$4, %r9
	movq	%rcx, %rax
	movq	%rcx, %rbp
	mulq	%r11
	subq	%rdx, %rbp
	shrq	%rbp
	addq	%rbp, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r10
	subq	%r10, %rcx
	movq	%rcx, %r14
	movl	%ecx, %ecx
	movl	%r14d, (%r9)
.L479:
	imulq	%r8, %rcx
	addq	$4, %r9
	movq	%rcx, %rax
	movq	%rcx, %r15
	mulq	%r11
	subq	%rdx, %r15
	shrq	%r15
	addq	%r15, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r12
	subq	%r12, %rcx
	movl	%ecx, (%r9)
	cmpq	%r9, %r13
	je	.L412
.L413:
	movl	%ecx, %r10d
	imulq	%r8, %r10
	movq	%r10, %rax
	movq	%r10, %r14
	mulq	%r11
	subq	%rdx, %r14
	shrq	%r14
	addq	%r14, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r15
	subq	%r15, %r10
	movl	%r10d, %r12d
	movl	%r10d, 4(%r9)
	imulq	%r8, %r12
	movq	%r12, %rax
	movq	%r12, %rbp
	mulq	%r11
	subq	%rdx, %rbp
	shrq	%rbp
	addq	%rbp, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rcx
	subq	%rcx, %r12
	movl	%r12d, %r10d
	movl	%r12d, 8(%r9)
	imulq	%r8, %r10
	movq	%r10, %rax
	movq	%r10, %r14
	mulq	%r11
	subq	%rdx, %r14
	shrq	%r14
	addq	%r14, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r15
	subq	%r15, %r10
	movl	%r10d, %r12d
	movl	%r10d, 12(%r9)
	imulq	%r8, %r12
	movq	%r12, %rax
	movq	%r12, %rbp
	movq	%r12, %r10
	mulq	%r11
	subq	%rdx, %rbp
	shrq	%rbp
	addq	%rbp, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rcx
	subq	%rcx, %r10
	addq	$16, %r9
	movl	%r10d, (%r9)
	cmpq	%r9, %r13
	movl	%r10d, %ecx
	jne	.L413
.L412:
	movl	%esi, %edx
	movq	%rdi, %rcx
	movl	$30, %r13d
	call	ntt2
	movl	$1, %r8d
	movl	$1004535807, %r9d
	movabsq	$1270861263111332585, %r11
	.p2align 4,,10
	.p2align 3
.L415:
	testb	$1, %r9b
	movl	%ebx, %ebx
	je	.L414
	imulq	%rbx, %r8
	movq	%r8, %rax
	movq	%r8, %r14
	mulq	%r11
	subq	%rdx, %r14
	shrq	%r14
	addq	%r14, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r15
	subl	%r15d, %r8d
.L414:
	imulq	%rbx, %rbx
	movl	%r9d, %r12d
	subl	$1, %r13d
	shrl	%r12d
	movq	%rbx, %rax
	movq	%rbx, %rbp
	mulq	%r11
	subq	%rdx, %rbp
	shrq	%rbp
	addq	%rbp, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rcx
	subl	%ecx, %ebx
	andl	$2, %r9d
	je	.L448
	imulq	%rbx, %r8
	movq	%r8, %rax
	movq	%r8, %r9
	mulq	%r11
	subq	%rdx, %r9
	shrq	%r9
	addq	%r9, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r10
	subl	%r10d, %r8d
.L448:
	imulq	%rbx, %rbx
	movl	%r12d, %r9d
	shrl	%r9d
	movq	%rbx, %rax
	movq	%rbx, %r14
	mulq	%r11
	subq	%rdx, %r14
	shrq	%r14
	addq	%r14, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r15
	subl	%r15d, %ebx
	andl	$2, %r12d
	je	.L450
	imulq	%rbx, %r8
	movq	%r8, %rax
	movq	%r8, %r12
	mulq	%r11
	subq	%rdx, %r12
	shrq	%r12
	addq	%r12, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rbp
	subl	%ebp, %r8d
.L450:
	imulq	%rbx, %rbx
	shrl	%r9d
	movq	%rbx, %rax
	movq	%rbx, %rcx
	mulq	%r11
	subq	%rdx, %rcx
	shrq	%rcx
	addq	%rcx, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r10
	subl	%r10d, %ebx
	subl	$2, %r13d
	jne	.L415
	testl	%esi, %esi
	jle	.L510
	leaq	(%rdi,%rsi,4), %r13
	movabsq	$1270861263111332585, %r11
	leaq	-4(,%rsi,4), %rsi
	shrq	$2, %rsi
	addq	$1, %rsi
	andl	$3, %esi
	je	.L417
	cmpq	$1, %rsi
	je	.L481
	cmpq	$2, %rsi
	je	.L482
	movl	(%rdi), %ebx
	addq	$4, %rdi
	imulq	%r8, %rbx
	movq	%rbx, %rax
	movq	%rbx, %r9
	mulq	%r11
	subq	%rdx, %r9
	shrq	%r9
	addq	%r9, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subq	%r14, %rbx
	movl	%ebx, -4(%rdi)
.L482:
	movl	(%rdi), %r15d
	addq	$4, %rdi
	imulq	%r8, %r15
	movq	%r15, %rax
	movq	%r15, %r12
	mulq	%r11
	subq	%rdx, %r12
	shrq	%r12
	addq	%r12, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rbp
	subq	%rbp, %r15
	movl	%r15d, -4(%rdi)
.L481:
	movl	(%rdi), %ecx
	addq	$4, %rdi
	imulq	%r8, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r10
	mulq	%r11
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rsi
	subq	%rsi, %rcx
	movl	%ecx, -4(%rdi)
	cmpq	%r13, %rdi
	je	.L510
.L417:
	movl	(%rdi), %ebx
	movl	4(%rdi), %r15d
	movl	8(%rdi), %ecx
	imulq	%r8, %rbx
	imulq	%r8, %r15
	imulq	%r8, %rcx
	movq	%rbx, %rax
	movq	%rbx, %r9
	mulq	%r11
	movq	%r15, %rax
	movq	%r15, %r12
	movq	%rcx, %r10
	subq	%rdx, %r9
	shrq	%r9
	addq	%r9, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	mulq	%r11
	movq	%rcx, %rax
	subq	%r14, %rbx
	subq	%rdx, %r12
	movl	%ebx, (%rdi)
	movl	12(%rdi), %ebx
	shrq	%r12
	addq	%r12, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rbp
	mulq	%r11
	imulq	%r8, %rbx
	subq	%rbp, %r15
	subq	%rdx, %r10
	movl	%r15d, 4(%rdi)
	shrq	%r10
	movq	%rbx, %rax
	movq	%rbx, %r9
	addq	%r10, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %rsi
	mulq	%r11
	subq	%rsi, %rcx
	subq	%rdx, %r9
	movl	%ecx, 8(%rdi)
	shrq	%r9
	addq	%r9, %rdx
	shrq	$29, %rdx
	imulq	$1004535809, %rdx, %r14
	subq	%r14, %rbx
	addq	$16, %rdi
	movl	%ebx, -4(%rdi)
	cmpq	%r13, %rdi
	jne	.L417
.L510:
	vmovaps	48(%rsp), %xmm6
	vmovaps	64(%rsp), %xmm7
	vmovaps	80(%rsp), %xmm8
	vmovaps	96(%rsp), %xmm9
	vmovaps	112(%rsp), %xmm10
	vmovaps	128(%rsp), %xmm11
	addq	$152, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L422:
	xorl	%ecx, %ecx
	jmp	.L401
	.seh_endproc
	.p2align 4
	.def	conv3;	.scl	3;	.type	32;	.endef
	.seh_proc	conv3
conv3:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$136, %rsp
	.seh_stackalloc	136
	vmovaps	%xmm6, 32(%rsp)
	.seh_savexmm	%xmm6, 32
	vmovaps	%xmm7, 48(%rsp)
	.seh_savexmm	%xmm7, 48
	vmovaps	%xmm8, 64(%rsp)
	.seh_savexmm	%xmm8, 64
	vmovaps	%xmm9, 80(%rsp)
	.seh_savexmm	%xmm9, 80
	vmovaps	%xmm10, 96(%rsp)
	.seh_savexmm	%xmm10, 96
	vmovaps	%xmm11, 112(%rsp)
	.seh_savexmm	%xmm11, 112
	.seh_endprologue
	movl	$469762048, %eax
	movslq	%edx, %rbx
	xorl	%edx, %edx
	movq	%rcx, %rsi
	divl	%ebx
	cmpl	$469762048, %ebx
	movl	%eax, %ebp
	ja	.L538
	movl	%eax, %r9d
	movl	$3, %ecx
	movl	$1, %r8d
	movabsq	$5270498295554651223, %r10
	testb	$1, %r9b
	je	.L513
	.p2align 4,,10
	.p2align 3
.L514:
	imulq	%rcx, %r8
	movq	%r8, %rax
	movq	%r8, %r12
	imulq	%r10
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r12, %rdx
	imulq	$469762049, %rdx, %r13
	subl	%r13d, %r8d
	shrl	%r9d
	je	.L512
	imulq	%rcx, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r14
	imulq	%r10
	sarq	$63, %r14
	sarq	$27, %rdx
	subq	%r14, %rdx
	imulq	$469762049, %rdx, %rdi
	subl	%edi, %ecx
.L537:
	testb	$1, %r9b
	jne	.L514
	imulq	%rcx, %rcx
	shrl	%r9d
	movq	%rcx, %rax
	movq	%rcx, %r15
	imulq	%r10
	sarq	$63, %r15
	sarq	$27, %rdx
	subq	%r15, %rdx
	imulq	$469762049, %rdx, %r13
	subl	%r13d, %ecx
	testb	$1, %r9b
	jne	.L514
.L513:
	imulq	%rcx, %rcx
	shrl	%r9d
	movabsq	$5270498295554651223, %rax
	imulq	%rcx
	movq	%rcx, %rdi
	sarq	$63, %rdi
	sarq	$27, %rdx
	subq	%rdi, %rdx
	imulq	$469762049, %rdx, %r11
	subl	%r11d, %ecx
	jmp	.L537
.L538:
	movl	$1, %r8d
	.p2align 4,,10
	.p2align 3
.L512:
	movl	$1, root(%rip)
	movl	%ebx, %edi
	sarl	%edi
	cmpl	$3, %ebx
	jle	.L516
	leaq	root(%rip), %r14
	movabsq	$5270498295554651223, %r9
	leal	-2(%rdi), %r15d
	leaq	4(%r14,%r15,4), %rcx
	movl	$1, %r15d
	movq	%rcx, %r10
	subq	%r14, %r10
	subq	$4, %r10
	shrq	$2, %r10
	addq	$1, %r10
	andl	$3, %r10d
	je	.L517
	cmpq	$1, %r10
	je	.L602
	cmpq	$2, %r10
	je	.L603
	movq	%r8, %rax
	movq	%r8, %r12
	addq	$4, %r14
	imulq	%r9
	sarq	$27, %rdx
	imulq	$469762049, %rdx, %r11
	subq	%r11, %r12
	movl	%r12d, (%r14)
	movl	%r12d, %r15d
.L603:
	movl	%r15d, %r13d
	addq	$4, %r14
	imulq	%r8, %r13
	movq	%r13, %rax
	movq	%r13, %r15
	imulq	%r9
	sarq	$63, %r15
	sarq	$27, %rdx
	subq	%r15, %rdx
	imulq	$469762049, %rdx, %r10
	subq	%r10, %r13
	movl	%r13d, (%r14)
	movl	%r13d, %r15d
.L602:
	movl	%r15d, %r11d
	addq	$4, %r14
	imulq	%r8, %r11
	movq	%r11, %rax
	movq	%r11, %r12
	imulq	%r9
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r12, %rdx
	imulq	$469762049, %rdx, %r13
	subq	%r13, %r11
	movl	%r11d, (%r14)
	cmpq	%r14, %rcx
	movl	%r11d, %r15d
	je	.L516
.L517:
	movl	%r15d, %eax
	addq	$16, %r14
	imulq	%r8, %rax
	movq	%rax, %r10
	imulq	%r9
	movq	%r10, %r11
	movq	%r10, %r13
	sarq	$63, %r11
	sarq	$27, %rdx
	subq	%r11, %rdx
	imulq	$469762049, %rdx, %r12
	subq	%r12, %r13
	movl	%r13d, %eax
	movl	%r13d, -12(%r14)
	imulq	%r8, %rax
	movq	%rax, %r15
	imulq	%r9
	movq	%r15, %r10
	movq	%r15, %r12
	sarq	$63, %r10
	sarq	$27, %rdx
	subq	%r10, %rdx
	imulq	$469762049, %rdx, %r11
	subq	%r11, %r12
	movl	%r12d, %eax
	movl	%r12d, -8(%r14)
	imulq	%r8, %rax
	movq	%rax, %r13
	imulq	%r9
	movq	%r13, %r15
	movq	%r13, %r11
	sarq	$63, %r15
	sarq	$27, %rdx
	subq	%r15, %rdx
	imulq	$469762049, %rdx, %r10
	subq	%r10, %r11
	movl	%r11d, %eax
	movl	%r11d, -4(%r14)
	imulq	%r8, %rax
	movq	%rax, %r12
	imulq	%r9
	movq	%r12, %r13
	movq	%r12, %r10
	sarq	$63, %r13
	sarq	$27, %rdx
	subq	%r13, %rdx
	imulq	$469762049, %rdx, %r15
	subq	%r15, %r10
	movl	%r10d, (%r14)
	cmpq	%r14, %rcx
	movl	%r10d, %r15d
	jne	.L517
.L516:
	movl	na(%rip), %r13d
	leaq	a(%rip), %r12
	leaq	A(%rip), %rdx
	movq	%r12, %rcx
	leaq	0(,%r13,4), %r8
	call	memcpy
	leaq	(%r12,%r13,4), %rcx
	movl	%ebx, %r8d
	xorl	%edx, %edx
	subl	%r13d, %r8d
	leaq	0(,%r8,4), %r8
	call	memset
	movl	nb(%rip), %r14d
	leaq	b(%rip), %r13
	leaq	B(%rip), %rdx
	movq	%r13, %rcx
	leaq	0(,%r14,4), %r8
	call	memcpy
	leaq	0(%r13,%r14,4), %rcx
	movl	%ebx, %r9d
	xorl	%edx, %edx
	subl	%r14d, %r9d
	leaq	0(,%r9,4), %r8
	call	memset
	movl	%ebx, %edx
	movq	%r12, %rcx
	call	ntt3
	movl	%ebx, %edx
	movq	%r13, %rcx
	call	ntt3
	cmpl	$3, %ebx
	jle	.L539
	leal	-4(%rbx), %ecx
	movl	$4294967295, %r8d
	xorl	%eax, %eax
	vmovdqa	.LC33(%rip), %ymm4
	movl	%ecx, %r11d
	vmovq	%r8, %xmm5
	vmovdqa	.LC34(%rip), %ymm3
	shrl	$2, %r11d
	vpbroadcastq	%xmm5, %ymm5
	vmovdqa	.LC35(%rip), %ymm7
	leal	1(%r11), %r10d
	vmovdqa	.LC18(%rip), %ymm6
	vpbroadcastq	.LC41(%rip), %ymm2
	movq	%r10, %rdx
	vpbroadcastq	.LC42(%rip), %ymm1
	salq	$4, %rdx
	andl	$1, %r10d
	je	.L519
	vpmovzxdq	0(%r13), %ymm8
	movl	$16, %eax
	vpmovzxdq	(%r12), %ymm0
	cmpq	%rdx, %rax
	vpmuludq	%ymm8, %ymm0, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm11
	vpmuludq	%ymm4, %ymm11, %ymm10
	vpmuludq	%ymm3, %ymm11, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpsrlq	$32, %ymm10, %ymm10
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, (%rsi)
	je	.L636
	.p2align 4,,10
	.p2align 3
.L519:
	vpmovzxdq	(%r12,%rax), %ymm11
	vpmovzxdq	0(%r13,%rax), %ymm10
	vpmuludq	%ymm10, %ymm11, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpmuludq	%ymm4, %ymm8, %ymm10
	vpmuludq	%ymm3, %ymm8, %ymm8
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpsrlq	$32, %ymm10, %ymm10
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, (%rsi,%rax)
	vpmovzxdq	16(%r12,%rax), %ymm11
	vpmovzxdq	16(%r13,%rax), %ymm10
	vpmuludq	%ymm10, %ymm11, %ymm0
	vpsrlq	$32, %ymm0, %ymm9
	vpand	%ymm5, %ymm0, %ymm8
	vpmuludq	%ymm4, %ymm9, %ymm11
	vpmuludq	%ymm4, %ymm8, %ymm10
	vpmuludq	%ymm3, %ymm8, %ymm8
	vpmuludq	%ymm3, %ymm9, %ymm9
	vpsrlq	$32, %ymm10, %ymm10
	vpaddq	%ymm11, %ymm8, %ymm8
	vpaddq	%ymm10, %ymm8, %ymm11
	vpsrlq	$32, %ymm11, %ymm10
	vpaddq	%ymm10, %ymm9, %ymm9
	vpmuludq	%ymm7, %ymm9, %ymm8
	vpsubq	%ymm8, %ymm0, %ymm0
	vpcmpgtq	%ymm2, %ymm0, %ymm11
	vpand	%ymm1, %ymm11, %ymm10
	vpsubq	%ymm10, %ymm0, %ymm9
	vpcmpgtq	%ymm2, %ymm9, %ymm8
	vpand	%ymm1, %ymm8, %ymm0
	vpsubq	%ymm0, %ymm9, %ymm11
	vpcmpgtq	%ymm2, %ymm11, %ymm10
	vpand	%ymm1, %ymm10, %ymm9
	vpsubq	%ymm9, %ymm11, %ymm8
	vpermd	%ymm8, %ymm6, %ymm0
	vmovdqu	%xmm0, 16(%rsi,%rax)
	addq	$32, %rax
	cmpq	%rdx, %rax
	jne	.L519
.L636:
	andl	$-4, %ecx
	addl	$4, %ecx
	vzeroupper
.L518:
	cmpl	%ecx, %ebx
	jle	.L524
	movabsq	$2635249108509053275, %r11
	movslq	%ecx, %r14
	movl	0(%r13,%r14,4), %r15d
	movl	(%r12,%r14,4), %r9d
	imulq	%r9, %r15
	movq	%r15, %rax
	movq	%r15, %r10
	mulq	%r11
	leal	1(%rcx), %eax
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %r8
	subq	%r8, %r15
	cmpl	%eax, %ebx
	movl	%r15d, (%rsi,%r14,4)
	jle	.L524
	cltq
	addl	$2, %ecx
	movl	(%r12,%rax,4), %r15d
	movl	0(%r13,%rax,4), %r9d
	imulq	%r9, %r15
	movq	%r15, %rax
	movq	%r15, %r10
	mulq	%r11
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %r8
	subq	%r8, %r15
	cmpl	%ecx, %ebx
	movl	%r15d, 4(%rsi,%r14,4)
	jle	.L524
	movslq	%ecx, %rcx
	movl	(%r12,%rcx,4), %r12d
	movl	0(%r13,%rcx,4), %r13d
	movq	%r12, %r15
	imulq	%r13, %r15
	movq	%r15, %rax
	mulq	%r11
	movq	%r15, %r11
	subq	%rdx, %r11
	shrq	%r11
	addq	%r11, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %r9
	subq	%r9, %r15
	movl	%r15d, 8(%rsi,%r14,4)
.L524:
	cmpl	$469762048, %ebx
	movl	$3, %r14d
	movl	$1, %ecx
	movabsq	$5270498295554651223, %r10
	ja	.L522
	testb	$1, %bpl
	je	.L525
	.p2align 4,,10
	.p2align 3
.L526:
	imulq	%r14, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r13
	imulq	%r10
	sarq	$63, %r13
	sarq	$27, %rdx
	subq	%r13, %rdx
	imulq	$469762049, %rdx, %r15
	subl	%r15d, %ecx
	shrl	%ebp
	je	.L522
	imulq	%r14, %r14
	movq	%r14, %rax
	movq	%r14, %r15
	imulq	%r10
	sarq	$63, %r15
	sarq	$27, %rdx
	subq	%r15, %rdx
	imulq	$469762049, %rdx, %r13
	subl	%r13d, %r14d
.L536:
	testb	$1, %bpl
	jne	.L526
	imulq	%r14, %r14
	shrl	%ebp
	movq	%r14, %rax
	movq	%r14, %r12
	imulq	%r10
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r12, %rdx
	imulq	$469762049, %rdx, %r11
	subl	%r11d, %r14d
	testb	$1, %bpl
	jne	.L526
.L525:
	imulq	%r14, %r14
	shrl	%ebp
	movabsq	$5270498295554651223, %rax
	imulq	%r14
	movq	%r14, %r8
	sarq	$63, %r8
	sarq	$27, %rdx
	subq	%r8, %rdx
	imulq	$469762049, %rdx, %r12
	subl	%r12d, %r14d
	jmp	.L536
	.p2align 4,,10
	.p2align 3
.L522:
	movq	%rcx, %rax
	movl	%ecx, %r14d
	movabsq	$5270498295554651223, %r9
	imulq	%rcx, %rcx
	movl	$234881023, %r10d
	movl	$28, %r13d
	imulq	%r9
	movq	%rcx, %rax
	movq	%rcx, %r11
	sarq	$27, %rdx
	sarq	$63, %r11
	imulq	$469762049, %rdx, %rbp
	imulq	%r9
	subl	%ebp, %r14d
	sarq	$27, %rdx
	subq	%r11, %rdx
	imulq	$469762049, %rdx, %r8
	subl	%r8d, %ecx
	.p2align 4,,10
	.p2align 3
.L528:
	testb	$1, %r10b
	je	.L527
	imulq	%rcx, %r14
	movq	%r14, %rax
	movq	%r14, %r12
	imulq	%r9
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r12, %rdx
	imulq	$469762049, %rdx, %r15
	subl	%r15d, %r14d
.L527:
	imulq	%rcx, %rcx
	movl	%r10d, %ebp
	subl	$1, %r13d
	shrl	%ebp
	movq	%rcx, %rax
	movq	%rcx, %r11
	imulq	%r9
	sarq	$63, %r11
	sarq	$27, %rdx
	subq	%r11, %rdx
	imulq	$469762049, %rdx, %r8
	subl	%r8d, %ecx
	andl	$2, %r10d
	je	.L584
	imulq	%rcx, %r14
	movq	%r14, %rax
	movq	%r14, %r10
	imulq	%r9
	sarq	$63, %r10
	sarq	$27, %rdx
	subq	%r10, %rdx
	imulq	$469762049, %rdx, %r12
	subl	%r12d, %r14d
.L584:
	imulq	%rcx, %rcx
	movl	%ebp, %r15d
	shrl	%r15d
	movq	%rcx, %rax
	movq	%rcx, %r11
	imulq	%r9
	sarq	$63, %r11
	sarq	$27, %rdx
	subq	%r11, %rdx
	imulq	$469762049, %rdx, %r8
	subl	%r8d, %ecx
	andl	$2, %ebp
	je	.L586
	imulq	%rcx, %r14
	movq	%r14, %rax
	movq	%r14, %rbp
	imulq	%r9
	sarq	$63, %rbp
	sarq	$27, %rdx
	subq	%rbp, %rdx
	imulq	$469762049, %rdx, %r10
	subl	%r10d, %r14d
.L586:
	imulq	%rcx, %rcx
	movl	%r15d, %r10d
	shrl	%r10d
	movq	%rcx, %rax
	movq	%rcx, %r12
	imulq	%r9
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r12, %rdx
	imulq	$469762049, %rdx, %r11
	subl	%r11d, %ecx
	andl	$2, %r15d
	je	.L588
	imulq	%rcx, %r14
	movq	%r14, %rax
	movq	%r14, %r15
	imulq	%r9
	sarq	$63, %r15
	sarq	$27, %rdx
	subq	%r15, %rdx
	imulq	$469762049, %rdx, %r8
	subl	%r8d, %r14d
.L588:
	imulq	%rcx, %rcx
	shrl	%r10d
	movq	%rcx, %rax
	movq	%rcx, %rbp
	imulq	%r9
	sarq	$63, %rbp
	sarq	$27, %rdx
	subq	%rbp, %rdx
	imulq	$469762049, %rdx, %r12
	subl	%r12d, %ecx
	subl	$3, %r13d
	jne	.L528
	movl	$1, root(%rip)
	cmpl	$1, %edi
	jle	.L529
	leaq	root(%rip), %r9
	movl	$1, %ebp
	movabsq	$5270498295554651223, %r10
	leal	-2(%rdi), %edi
	leaq	4(%r9,%rdi,4), %r13
	movq	%r13, %rcx
	subq	%r9, %rcx
	subq	$4, %rcx
	shrq	$2, %rcx
	addq	$1, %rcx
	andl	$3, %ecx
	je	.L530
	cmpq	$1, %rcx
	je	.L604
	cmpq	$2, %rcx
	je	.L605
	movq	%r14, %rax
	movq	%r14, %r15
	addq	$4, %r9
	imulq	%r10
	sarq	$27, %rdx
	imulq	$469762049, %rdx, %r11
	subq	%r11, %r15
	movl	%r15d, (%r9)
	movl	%r15d, %ebp
.L605:
	movl	%ebp, %r8d
	addq	$4, %r9
	imulq	%r14, %r8
	movq	%r8, %rax
	movq	%r8, %rbp
	imulq	%r10
	sarq	$63, %rbp
	sarq	$27, %rdx
	subq	%rbp, %rdx
	imulq	$469762049, %rdx, %r12
	subq	%r12, %r8
	movl	%r8d, (%r9)
	movl	%r8d, %ebp
.L604:
	movl	%ebp, %edi
	addq	$4, %r9
	imulq	%r14, %rdi
	movq	%rdi, %rax
	movq	%rdi, %rcx
	imulq	%r10
	sarq	$63, %rcx
	sarq	$27, %rdx
	subq	%rcx, %rdx
	imulq	$469762049, %rdx, %r11
	subq	%r11, %rdi
	movl	%edi, (%r9)
	cmpq	%r9, %r13
	movl	%edi, %ebp
	je	.L529
.L530:
	movl	%ebp, %r15d
	addq	$16, %r9
	imulq	%r14, %r15
	movq	%r15, %rax
	movq	%r15, %r8
	imulq	%r10
	sarq	$63, %r8
	sarq	$27, %rdx
	subq	%r8, %rdx
	imulq	$469762049, %rdx, %r12
	subq	%r12, %r15
	movl	%r15d, %edi
	movl	%r15d, -12(%r9)
	imulq	%r14, %rdi
	movq	%rdi, %rax
	movq	%rdi, %rcx
	imulq	%r10
	sarq	$63, %rcx
	sarq	$27, %rdx
	subq	%rcx, %rdx
	imulq	$469762049, %rdx, %r11
	subq	%r11, %rdi
	movl	%edi, %ebp
	movl	%edi, -8(%r9)
	imulq	%r14, %rbp
	movq	%rbp, %rax
	movq	%rbp, %r15
	imulq	%r10
	sarq	$63, %r15
	sarq	$27, %rdx
	subq	%r15, %rdx
	imulq	$469762049, %rdx, %r8
	subq	%r8, %rbp
	movl	%ebp, %r12d
	movl	%ebp, -4(%r9)
	imulq	%r14, %r12
	movq	%r12, %rax
	movq	%r12, %rdi
	imulq	%r10
	sarq	$63, %rdi
	sarq	$27, %rdx
	subq	%rdi, %rdx
	imulq	$469762049, %rdx, %rcx
	subq	%rcx, %r12
	movl	%r12d, (%r9)
	cmpq	%r9, %r13
	movl	%r12d, %ebp
	jne	.L530
.L529:
	movabsq	$5270498295554651223, %r14
	movl	%ebx, %edx
	movq	%rsi, %rcx
	call	ntt3
	movl	%ebx, %r11d
	movabsq	$2635249108509053275, %r13
	movl	$28, %r8d
	movq	%r11, %rax
	movl	%r11d, %r15d
	imulq	%r14
	imulq	%r11, %r11
	sarq	$27, %rdx
	imulq	$469762049, %rdx, %r9
	movq	%r11, %rax
	movq	%r11, %r10
	mulq	%r13
	subl	%r9d, %r15d
	movl	$234881023, %r9d
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %rbp
	subl	%ebp, %r11d
	.p2align 4,,10
	.p2align 3
.L532:
	testb	$1, %r9b
	je	.L531
	imulq	%r11, %r15
	movq	%r15, %rax
	movq	%r15, %r12
	imulq	%r14
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r12, %rdx
	imulq	$469762049, %rdx, %rdi
	subl	%edi, %r15d
.L531:
	imulq	%r11, %r11
	movl	%r9d, %ecx
	subl	$1, %r8d
	shrl	%ecx
	movq	%r11, %rax
	movq	%r11, %r10
	mulq	%r13
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %rbp
	subl	%ebp, %r11d
	andl	$2, %r9d
	je	.L567
	imulq	%r11, %r15
	movq	%r15, %rax
	movq	%r15, %r9
	imulq	%r14
	sarq	$63, %r9
	sarq	$27, %rdx
	subq	%r9, %rdx
	imulq	$469762049, %rdx, %r12
	subl	%r12d, %r15d
.L567:
	imulq	%r11, %r11
	movl	%ecx, %edi
	shrl	%edi
	movq	%r11, %rax
	movq	%r11, %r10
	mulq	%r13
	subq	%rdx, %r10
	shrq	%r10
	addq	%r10, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %rbp
	subl	%ebp, %r11d
	andl	$2, %ecx
	je	.L569
	imulq	%r11, %r15
	movq	%r15, %rax
	movq	%r15, %rcx
	imulq	%r14
	sarq	$63, %rcx
	sarq	$27, %rdx
	subq	%rcx, %rdx
	imulq	$469762049, %rdx, %r9
	subl	%r9d, %r15d
.L569:
	imulq	%r11, %r11
	movl	%edi, %r9d
	shrl	%r9d
	movq	%r11, %rax
	movq	%r11, %r12
	mulq	%r13
	subq	%rdx, %r12
	shrq	%r12
	addq	%r12, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %r10
	subl	%r10d, %r11d
	andl	$2, %edi
	je	.L571
	imulq	%r11, %r15
	movq	%r15, %rax
	movq	%r15, %rdi
	imulq	%r14
	sarq	$63, %rdi
	sarq	$27, %rdx
	subq	%rdi, %rdx
	imulq	$469762049, %rdx, %rbp
	subl	%ebp, %r15d
.L571:
	imulq	%r11, %r11
	shrl	%r9d
	movq	%r11, %rax
	movq	%r11, %rcx
	mulq	%r13
	subq	%rdx, %rcx
	shrq	%rcx
	addq	%rcx, %rdx
	shrq	$28, %rdx
	imulq	$469762049, %rdx, %r12
	subl	%r12d, %r11d
	subl	$3, %r8d
	jne	.L532
	testl	%ebx, %ebx
	jle	.L637
	leaq	(%rsi,%rbx,4), %r14
	movabsq	$5270498295554651223, %r13
	leaq	-4(,%rbx,4), %rbx
	shrq	$2, %rbx
	addq	$1, %rbx
	andl	$3, %ebx
	je	.L534
	cmpq	$1, %rbx
	je	.L606
	cmpq	$2, %rbx
	je	.L607
	movl	(%rsi), %r11d
	addq	$4, %rsi
	imulq	%r15, %r11
	movq	%r11, %rax
	movq	%r11, %r8
	imulq	%r13
	sarq	$63, %r8
	sarq	$27, %rdx
	subq	%r8, %rdx
	imulq	$469762049, %rdx, %r9
	subq	%r9, %r11
	movl	%r11d, -4(%rsi)
.L607:
	movl	(%rsi), %r10d
	addq	$4, %rsi
	imulq	%r15, %r10
	movq	%r10, %rax
	movq	%r10, %rdi
	imulq	%r13
	sarq	$63, %rdi
	sarq	$27, %rdx
	subq	%rdi, %rdx
	imulq	$469762049, %rdx, %rbp
	subq	%rbp, %r10
	movl	%r10d, -4(%rsi)
.L606:
	movl	(%rsi), %ecx
	addq	$4, %rsi
	imulq	%r15, %rcx
	movq	%rcx, %rax
	movq	%rcx, %r12
	imulq	%r13
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r12, %rdx
	imulq	$469762049, %rdx, %rbx
	subq	%rbx, %rcx
	movl	%ecx, -4(%rsi)
	cmpq	%r14, %rsi
	je	.L637
.L534:
	movl	(%rsi), %r11d
	addq	$16, %rsi
	movl	-12(%rsi), %r10d
	movl	-8(%rsi), %ecx
	imulq	%r15, %r11
	imulq	%r15, %r10
	imulq	%r15, %rcx
	movq	%r11, %rax
	movq	%r11, %r8
	imulq	%r13
	sarq	$63, %r8
	movq	%r10, %rax
	movq	%r10, %rdi
	movq	%rcx, %r12
	sarq	$63, %rdi
	sarq	$63, %r12
	sarq	$27, %rdx
	subq	%r8, %rdx
	imulq	$469762049, %rdx, %r9
	imulq	%r13
	movq	%rcx, %rax
	subq	%r9, %r11
	movl	%r11d, -16(%rsi)
	movl	-4(%rsi), %r11d
	sarq	$27, %rdx
	subq	%rdi, %rdx
	imulq	$469762049, %rdx, %rbp
	imulq	%r13
	imulq	%r15, %r11
	subq	%rbp, %r10
	sarq	$27, %rdx
	movl	%r10d, -12(%rsi)
	subq	%r12, %rdx
	movq	%r11, %rax
	movq	%r11, %r8
	imulq	$469762049, %rdx, %rbx
	sarq	$63, %r8
	imulq	%r13
	subq	%rbx, %rcx
	sarq	$27, %rdx
	movl	%ecx, -8(%rsi)
	subq	%r8, %rdx
	imulq	$469762049, %rdx, %r9
	subq	%r9, %r11
	movl	%r11d, -4(%rsi)
	cmpq	%r14, %rsi
	jne	.L534
.L637:
	vmovaps	32(%rsp), %xmm6
	vmovaps	48(%rsp), %xmm7
	vmovaps	64(%rsp), %xmm8
	vmovaps	80(%rsp), %xmm9
	vmovaps	96(%rsp), %xmm10
	vmovaps	112(%rsp), %xmm11
	addq	$136, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L539:
	xorl	%ecx, %ecx
	jmp	.L518
	.seh_endproc
	.section .rdata,"dr"
.LC43:
	.ascii "0\0"
	.section	.text.startup,"x"
	.p2align 4
	.globl	main
	.def	main;	.scl	2;	.type	32;	.endef
	.seh_proc	main
main:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$616, %rsp
	.seh_stackalloc	616
	vmovaps	%xmm6, 576(%rsp)
	.seh_savexmm	%xmm6, 576
	vmovaps	%xmm7, 592(%rsp)
	.seh_savexmm	%xmm7, 592
	.seh_endprologue
	leaq	io_buf(%rip), %rdi
	call	__main
	movq	__imp___acrt_iob_func(%rip), %rax
	xorl	%ecx, %ecx
	movl	$0, 236(%rsp)
	leaq	559(%rsp), %r15
	andq	$-32, %r15
	movq	%rax, 104(%rsp)
	call	*%rax
	movl	$2000003, %r8d
	movl	$1, %edx
	leaq	io_buf(%rip), %rcx
	movq	%rax, %r9
	call	fread
	cltq
	movb	$0, (%rdi,%rax)
	movzbl	io_buf(%rip), %ebx
	testb	$16, %bl
	je	.L639
	movl	$1, %edx
	.p2align 5
	.p2align 4,,10
	.p2align 3
.L640:
	leaq	io_buf(%rip), %rcx
	movq	%rdx, %rbp
	movzbl	(%rcx,%rdx), %ebx
	addq	$1, %rdx
	testb	$16, %bl
	jne	.L640
	testb	%bl, %bl
	movl	%ebp, %esi
	je	.L641
.L684:
	leaq	1+io_buf(%rip), %r11
	movslq	%esi, %r8
	movl	%esi, %edi
	addq	%r8, %r11
	jmp	.L642
	.p2align 5
	.p2align 4,,10
	.p2align 3
.L644:
	movzbl	(%r11), %ebx
	addq	$1, %r11
	addl	$1, %edi
	testb	%bl, %bl
	je	.L643
.L642:
	testb	$16, %bl
	je	.L644
.L643:
	leaq	io_buf(%rip), %rbx
	movslq	%edi, %r12
	testb	$16, (%rbx,%r12)
	je	.L687
	leal	1(%rdi), %eax
	cltq
	subq	%rax, %r12
	leaq	(%rbx,%r12), %r13
	.p2align 5
	.p2align 4,,10
	.p2align 3
.L645:
	movzbl	1(%r13,%rax), %r8d
	movq	%rax, %r11
	addq	$1, %rax
	andl	$16, %r8d
	jne	.L645
	movl	%r11d, %ecx
	movl	%r11d, %ebp
	xorl	%r8d, %r8d
	leal	8(%rsi), %r12d
	subl	%edi, %ecx
	leal	8(%rcx), %r9d
	imulq	$954437177, %r12, %rbx
	movslq	%r9d, %rdx
	sarl	$31, %r9d
	imulq	$954437177, %rdx, %r14
	movl	%esi, %edx
	leaq	236(%rsp), %r12
	leaq	io_buf(%rip), %rcx
	movq	%r12, 32(%rsp)
	shrq	$33, %rbx
	sarq	$33, %r14
	movl	%ebx, na(%rip)
	movl	%ebx, %r13d
	subl	%r9d, %r14d
	leaq	A(%rip), %r9
	movl	%r14d, nb(%rip)
	call	parse_blocks_simd
	movq	%r12, 32(%rsp)
	movl	%edi, %r8d
	movl	%ebp, %edx
	leaq	B(%rip), %r9
	movl	$0, 236(%rsp)
	leaq	io_buf(%rip), %rcx
	call	parse_blocks_simd
	cmpl	$1, %ebx
	je	.L646
	cmpl	$1, %r14d
	je	.L647
.L681:
	addl	%r14d, %r13d
	leal	-1(%r13), %ebx
.L648:
	subl	$2, %r13d
	movl	$1, %edi
	bsrl	%r13d, %esi
	leal	1(%rsi), %ecx
	sall	%cl, %edi
	cmpl	%edi, lastRev(%rip)
	je	.L654
	cmpl	$1, %edi
	jle	.L657
	leaq	rev(%rip), %r13
	movl	%edi, %r14d
	movl	$1, %ebp
	leaq	-1(%r14), %r10
	andl	$7, %r10d
	je	.L656
	cmpq	$1, %r10
	je	.L782
	cmpq	$2, %r10
	je	.L783
	cmpq	$3, %r10
	je	.L784
	cmpq	$4, %r10
	je	.L785
	cmpq	$5, %r10
	je	.L786
	cmpq	$6, %r10
	je	.L787
	movl	0(%r13), %r8d
	movl	$2, %ebp
	orl	%edi, %r8d
	shrl	%r8d
	movl	%r8d, 4(%r13)
.L787:
	movl	%ebp, %r11d
	movl	%ebp, %edx
	sarl	%r11d
	andl	$1, %edx
	movslq	%r11d, %r9
	sall	%cl, %edx
	orl	0(%r13,%r9,4), %edx
	shrl	%edx
	movl	%edx, 0(%r13,%rbp,4)
	addq	$1, %rbp
.L786:
	movl	%ebp, %r12d
	movl	%ebp, %esi
	sarl	%r12d
	andl	$1, %esi
	movslq	%r12d, %rax
	sall	%cl, %esi
	orl	0(%r13,%rax,4), %esi
	shrl	%esi
	movl	%esi, 0(%r13,%rbp,4)
	addq	$1, %rbp
.L785:
	movl	%ebp, %r10d
	movl	%ebp, %r8d
	sarl	%r10d
	andl	$1, %r8d
	movslq	%r10d, %r11
	sall	%cl, %r8d
	orl	0(%r13,%r11,4), %r8d
	shrl	%r8d
	movl	%r8d, 0(%r13,%rbp,4)
	addq	$1, %rbp
.L784:
	movl	%ebp, %r9d
	movl	%ebp, %r12d
	sarl	%r9d
	andl	$1, %r12d
	movslq	%r9d, %rdx
	sall	%cl, %r12d
	orl	0(%r13,%rdx,4), %r12d
	shrl	%r12d
	movl	%r12d, 0(%r13,%rbp,4)
	addq	$1, %rbp
.L783:
	movl	%ebp, %eax
	movl	%ebp, %r10d
	sarl	%eax
	andl	$1, %r10d
	movslq	%eax, %rsi
	sall	%cl, %r10d
	orl	0(%r13,%rsi,4), %r10d
	shrl	%r10d
	movl	%r10d, 0(%r13,%rbp,4)
	addq	$1, %rbp
.L782:
	movl	%ebp, %r11d
	movl	%ebp, %r8d
	sarl	%r11d
	andl	$1, %r8d
	movslq	%r11d, %r9
	sall	%cl, %r8d
	orl	0(%r13,%r9,4), %r8d
	shrl	%r8d
	movl	%r8d, 0(%r13,%rbp,4)
	addq	$1, %rbp
	cmpq	%r14, %rbp
	je	.L657
.L656:
	leaq	1(%rbp), %rsi
	movl	%ebp, %edx
	movl	%ebp, %eax
	sarl	%edx
	andl	$1, %eax
	movl	%esi, %r10d
	movslq	%edx, %r12
	sall	%cl, %eax
	movl	%esi, %r9d
	orl	0(%r13,%r12,4), %eax
	leaq	2(%rbp), %rdx
	sarl	%r10d
	andl	$1, %r9d
	movslq	%r10d, %r11
	sall	%cl, %r9d
	movl	%edx, %r8d
	sarl	%r8d
	shrl	%eax
	movslq	%r8d, %r12
	movl	%eax, 0(%r13,%rbp,4)
	orl	0(%r13,%r11,4), %r9d
	movl	%edx, %eax
	andl	$1, %eax
	sall	%cl, %eax
	shrl	%r9d
	movl	%r9d, 0(%r13,%rsi,4)
	orl	0(%r13,%r12,4), %eax
	leaq	3(%rbp), %rsi
	movl	%esi, %r10d
	movl	%esi, %r9d
	sarl	%r10d
	andl	$1, %r9d
	movslq	%r10d, %r11
	sall	%cl, %r9d
	shrl	%eax
	movl	%eax, 0(%r13,%rdx,4)
	orl	0(%r13,%r11,4), %r9d
	leaq	4(%rbp), %rdx
	movl	%edx, %r8d
	movl	%edx, %eax
	sarl	%r8d
	andl	$1, %eax
	movslq	%r8d, %r12
	sall	%cl, %eax
	shrl	%r9d
	movl	%r9d, 0(%r13,%rsi,4)
	orl	0(%r13,%r12,4), %eax
	leaq	5(%rbp), %rsi
	movl	%esi, %r10d
	movl	%esi, %r9d
	sarl	%r10d
	andl	$1, %r9d
	movslq	%r10d, %r11
	sall	%cl, %r9d
	shrl	%eax
	movl	%eax, 0(%r13,%rdx,4)
	orl	0(%r13,%r11,4), %r9d
	leaq	6(%rbp), %rdx
	movl	%edx, %r8d
	movl	%edx, %eax
	sarl	%r8d
	andl	$1, %eax
	movslq	%r8d, %r12
	shrl	%r9d
	sall	%cl, %eax
	movl	%r9d, 0(%r13,%rsi,4)
	orl	0(%r13,%r12,4), %eax
	leaq	7(%rbp), %rsi
	addq	$8, %rbp
	movl	%esi, %r10d
	movl	%esi, %r9d
	sarl	%r10d
	andl	$1, %r9d
	movslq	%r10d, %r11
	sall	%cl, %r9d
	shrl	%eax
	movl	%eax, 0(%r13,%rdx,4)
	orl	0(%r13,%r11,4), %r9d
	shrl	%r9d
	cmpq	%r14, %rbp
	movl	%r9d, 0(%r13,%rsi,4)
	jne	.L656
.L657:
	movl	%edi, lastRev(%rip)
.L654:
	leaq	c1(%rip), %rcx
	movl	%edi, %edx
	call	conv1
	leaq	c2(%rip), %rcx
	movl	%edi, %edx
	call	conv2
	leaq	c3(%rip), %rcx
	movl	%edi, %edx
	call	conv3
	testl	%ebx, %ebx
	jle	.L653
	movslq	%ebx, %rcx
	xorl	%r14d, %r14d
	xorl	%r12d, %r12d
	leaq	192(%rsp), %rdi
	salq	$2, %rcx
	xorl	%r13d, %r13d
	leaq	176(%rsp), %rdx
	movq	%rcx, 88(%rsp)
	leaq	208(%rsp), %rbp
	movq	%rdi, 48(%rsp)
	movq	%rdx, 96(%rsp)
	.p2align 4,,10
	.p2align 3
.L664:
	leaq	c2(%rip), %rax
	movl	$669690699, %ecx
	movl	$998244353, %edi
	movl	(%rax,%r14), %r11d
	leaq	c1(%rip), %r8
	movl	(%r8,%r14), %esi
	movq	$1004535809, 192(%rsp)
	movq	$0, 200(%rsp)
	movq	%r11, %r9
	leal	1004535809(%r9), %eax
	subq	%rsi, %r11
	movq	%rsi, 80(%rsp)
	subq	%rsi, %rax
	cmpl	%esi, %r9d
	cmovnb	%r11, %rax
	mulq	%rcx
	movq	%rbp, %rcx
	movq	%rdx, 216(%rsp)
	movq	48(%rsp), %rdx
	movq	%rax, 208(%rsp)
	call	__umodti3
	movq	%rbp, %rcx
	movq	$469762049, 192(%rsp)
	vmovq	%xmm0, %rax
	movq	$0, 200(%rsp)
	mulq	%rdi
	addq	80(%rsp), %rax
	adcq	$0, %rdx
	movq	%rax, %rsi
	movq	%rax, 208(%rsp)
	movq	%rdx, 216(%rsp)
	movq	%rdx, %rdi
	movq	48(%rsp), %rdx
	call	__umodti3
	leaq	c3(%rip), %r8
	movl	(%r8,%r14), %eax
	vmovq	%xmm0, %rdx
	cmpq	%rdx, %rax
	jb	.L661
	subq	%rdx, %rax
	movl	$354521948, %ecx
	mulq	%rcx
	movq	%rbp, %rcx
	movq	%rdx, 216(%rsp)
	movq	48(%rsp), %rdx
	movq	%rax, 208(%rsp)
	call	__umodti3
	movq	48(%rsp), %rcx
	movq	%rbp, %r8
	movabsq	$1002772198720536577, %rdx
	vmovq	%xmm0, %rax
	movq	$1000000000, 176(%rsp)
	movq	$0, 184(%rsp)
	mulq	%rdx
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	movq	96(%rsp), %rdx
	addq	%r12, %rsi
	adcq	%r13, %rdi
	movq	%rsi, 192(%rsp)
	movq	%rdi, 200(%rsp)
	call	__udivmodti4
	movq	208(%rsp), %r8
	leaq	C(%rip), %r10
	vmovq	%xmm0, %r12
	vpextrq	$1, %xmm0, %r13
	movl	%r8d, (%r10,%r14)
	addq	$4, %r14
	cmpq	%r14, 88(%rsp)
	jne	.L664
.L663:
	xorl	%r14d, %r14d
	movl	$999999999, %r11d
	cmpq	%rsi, %r11
	movq	%r14, %rsi
	sbbq	%rdi, %rsi
	jnc	.L653
	movq	48(%rsp), %rcx
	xorl	%r9d, %r9d
	movq	%rbp, %r8
	movq	%r12, 192(%rsp)
	movq	96(%rsp), %rdx
	movq	%r13, 200(%rsp)
	leaq	C(%rip), %rdi
	movq	88(%rsp), %rsi
	movq	%r9, 184(%rsp)
	movq	$1000000000, 176(%rsp)
	call	__udivmodti4
	movq	208(%rsp), %rcx
	movl	$999999999, %eax
	addq	%rdi, %rsi
	cmpq	%r12, %rax
	sbbq	%r13, %r14
	movl	%ecx, (%rsi)
	jnc	.L839
	addl	$2, %ebx
	vmovd	%xmm0, 4(%rsi)
.L653:
	leal	-1(%rbx), %r10d
	leaq	4+C(%rip), %r14
	movslq	%r10d, %r11
	andl	$7, %r10d
	je	.L667
	leal	1(%r11), %esi
	movl	%r11d, %ebx
	movl	%r11d, %edi
	cmpl	$1, %esi
	jle	.L666
	subq	$1, %r11
	movl	(%r14,%r11,4), %r9d
	testl	%r9d, %r9d
	jne	.L833
	cmpl	$1, %r10d
	je	.L667
	cmpl	$2, %r10d
	je	.L795
	cmpl	$3, %r10d
	je	.L796
	cmpl	$4, %r10d
	je	.L797
	cmpl	$5, %r10d
	je	.L798
	cmpl	$6, %r10d
	je	.L799
	movl	%r11d, %ebx
	movl	%r11d, %edi
	subq	$1, %r11
	cmpl	$0, (%r14,%r11,4)
	jne	.L833
.L799:
	movl	%r11d, %ebx
	movl	%r11d, %edi
	subq	$1, %r11
	cmpl	$0, (%r14,%r11,4)
	jne	.L833
.L798:
	movl	%r11d, %ebx
	movl	%r11d, %edi
	subq	$1, %r11
	cmpl	$0, (%r14,%r11,4)
	jne	.L833
.L797:
	movl	%r11d, %ebx
	movl	%r11d, %edi
	subq	$1, %r11
	cmpl	$0, (%r14,%r11,4)
	jne	.L833
.L796:
	movl	%r11d, %ebx
	movl	%r11d, %edi
	subq	$1, %r11
	cmpl	$0, (%r14,%r11,4)
	jne	.L833
.L795:
	movl	%r11d, %ebx
	movl	%r11d, %edi
	subq	$1, %r11
	cmpl	$0, (%r14,%r11,4)
	jne	.L833
.L667:
	leal	1(%r11), %ebp
	movl	%r11d, %ebx
	movl	%r11d, %edi
	cmpl	$1, %ebp
	jle	.L666
	movl	-4(%r14,%r11,4), %ecx
	testl	%ecx, %ecx
	jne	.L833
	cmpl	$0, -8(%r14,%r11,4)
	leal	-1(%r11), %ebx
	leal	-1(%r11), %edi
	jne	.L833
	cmpl	$0, -12(%r14,%r11,4)
	leal	-2(%r11), %ebx
	leal	-2(%r11), %edi
	jne	.L833
	cmpl	$0, -16(%r14,%r11,4)
	leal	-3(%r11), %ebx
	leal	-3(%r11), %edi
	jne	.L833
	cmpl	$0, -20(%r14,%r11,4)
	leal	-4(%r11), %ebx
	leal	-4(%r11), %edi
	jne	.L833
	cmpl	$0, -24(%r14,%r11,4)
	leal	-5(%r11), %ebx
	leal	-5(%r11), %edi
	jne	.L833
	cmpl	$0, -28(%r14,%r11,4)
	leal	-6(%r11), %ebx
	leal	-6(%r11), %edi
	jne	.L833
	leal	-7(%r11), %ebx
	leal	-7(%r11), %edi
	subq	$8, %r11
	cmpl	$0, (%r14,%r11,4)
	je	.L667
.L833:
	cmpl	$6, %edi
	jle	.L840
.L682:
	vpcmpeqd	%ymm0, %ymm0, %ymm0
	vpsrlw	$8, %ymm0, %ymm6
	movslq	%edi, %r13
	leaq	io_buf(%rip), %r11
	vmovdqu	%ymm6, 48(%rsp)
	leaq	-28+C(%rip), %r12
	leaq	204(%rsp), %r14
	leaq	144(%rsp), %rcx
	leaq	112(%rsp), %r10
	.p2align 4,,10
	.p2align 3
.L671:
	vmovdqu	(%r12,%r13,4), %ymm3
	leaq	240(%rsp), %rsi
.L670:
	vmovdqa	%ymm3, 112(%rsp)
	movq	%r15, %r8
	movq	%r10, %rdx
	vzeroupper
	call	div10_epu32
	movl	(%r15), %edx
	movl	4(%r15), %r9d
	movl	8(%r15), %ebp
	vmovdqa	144(%rsp), %ymm1
	orl	$48, %edx
	movl	12(%r15), %edi
	orl	$48, %r9d
	movl	%edx, 32(%rsi)
	movl	16(%r15), %eax
	orl	$48, %ebp
	movl	%r9d, 68(%rsi)
	movl	20(%r15), %edx
	movl	%ebp, 104(%rsi)
	movl	24(%r15), %r9d
	movl	28(%r15), %ebp
	orl	$48, %edi
	orl	$48, %eax
	movl	%edi, 140(%rsi)
	orl	$48, %edx
	movl	%eax, 176(%rsi)
	orl	$48, %r9d
	movl	%edx, 212(%rsi)
	movq	%r10, %rdx
	orl	$48, %ebp
	movl	%r9d, 248(%rsi)
	movl	%ebp, 284(%rsi)
	vmovdqa	%ymm1, 112(%rsp)
	vzeroupper
	call	div10_epu32
	movl	(%r15), %edi
	movl	4(%r15), %eax
	movl	8(%r15), %edx
	vmovdqa	144(%rsp), %ymm2
	orl	$48, %edi
	movl	12(%r15), %r9d
	orl	$48, %eax
	movl	%edi, 28(%rsi)
	movl	16(%r15), %ebp
	orl	$48, %edx
	movl	%eax, 64(%rsi)
	movl	20(%r15), %edi
	movl	%edx, 100(%rsi)
	movl	24(%r15), %eax
	movl	28(%r15), %edx
	orl	$48, %r9d
	orl	$48, %ebp
	movl	%r9d, 136(%rsi)
	orl	$48, %edi
	movl	%ebp, 172(%rsi)
	orl	$48, %eax
	movl	%edi, 208(%rsi)
	orl	$48, %edx
	movl	%eax, 244(%rsi)
	movl	%edx, 280(%rsi)
	movq	%r10, %rdx
	vmovdqa	%ymm2, 112(%rsp)
	vzeroupper
	call	div10_epu32
	movl	(%r15), %r8d
	subq	$12, %rsi
	movl	4(%r15), %r9d
	vpxor	%xmm2, %xmm2, %xmm2
	vmovdqa	144(%rsp), %ymm3
	movl	8(%r15), %ebp
	orl	$48, %r8d
	movl	12(%r15), %edi
	orl	$48, %r9d
	movl	%r8d, 36(%rsi)
	movl	16(%r15), %eax
	movl	%r9d, 72(%rsi)
	movl	20(%r15), %edx
	movl	24(%r15), %r8d
	orl	$48, %ebp
	movl	28(%r15), %r9d
	orl	$48, %edi
	movl	%ebp, 108(%rsi)
	orl	$48, %eax
	movl	%edi, 144(%rsi)
	orl	$48, %edx
	movl	%eax, 180(%rsi)
	orl	$48, %r8d
	movl	%edx, 216(%rsi)
	orl	$48, %r9d
	movl	%r8d, 252(%rsi)
	movl	%r9d, 288(%rsi)
	cmpq	%rsi, %r14
	jne	.L670
	vmovd	356(%rsp), %xmm4
	subq	$8, %r13
	addq	$72, %r11
	vpinsrd	$1, 360(%rsp), %xmm4, %xmm5
	vmovd	348(%rsp), %xmm7
	vpinsrd	$1, 352(%rsp), %xmm7, %xmm0
	vpunpcklqdq	%xmm5, %xmm0, %xmm6
	vmovd	412(%rsp), %xmm1
	vpinsrd	$1, 416(%rsp), %xmm1, %xmm3
	vmovd	404(%rsp), %xmm4
	vpinsrd	$1, 408(%rsp), %xmm4, %xmm5
	vpunpcklqdq	%xmm3, %xmm5, %xmm7
	vinserti128	$0x1, %xmm6, %ymm7, %ymm0
	vmovd	380(%rsp), %xmm1
	vpblendw	$85, %ymm0, %ymm2, %ymm0
	vpinsrd	$1, 312(%rsp), %xmm1, %xmm4
	vmovd	316(%rsp), %xmm6
	vpinsrd	$1, 320(%rsp), %xmm6, %xmm3
	vpunpcklqdq	%xmm3, %xmm4, %xmm5
	vmovd	372(%rsp), %xmm7
	vpinsrd	$1, 376(%rsp), %xmm7, %xmm6
	vmovd	364(%rsp), %xmm3
	vpinsrd	$1, 368(%rsp), %xmm3, %xmm1
	vpunpcklqdq	%xmm6, %xmm1, %xmm4
	vinserti128	$0x1, %xmm5, %ymm4, %ymm5
	vmovd	276(%rsp), %xmm1
	vpblendw	$85, %ymm5, %ymm2, %ymm7
	vpinsrd	$1, 280(%rsp), %xmm1, %xmm4
	vmovd	340(%rsp), %xmm5
	vpackusdw	%ymm7, %ymm0, %ymm6
	vpinsrd	$1, 344(%rsp), %xmm5, %xmm0
	vpermq	$216, %ymm6, %ymm3
	vmovd	332(%rsp), %xmm6
	vpunpcklqdq	%xmm4, %xmm0, %xmm7
	vpinsrd	$1, 336(%rsp), %xmm6, %xmm1
	vmovd	324(%rsp), %xmm4
	vpinsrd	$1, 328(%rsp), %xmm4, %xmm5
	vpunpcklqdq	%xmm1, %xmm5, %xmm0
	vinserti128	$0x1, %xmm7, %ymm0, %ymm7
	vmovd	308(%rsp), %xmm6
	vpblendw	$85, %ymm7, %ymm2, %ymm7
	vpinsrd	$1, 240(%rsp), %xmm6, %xmm4
	vmovd	300(%rsp), %xmm1
	vpinsrd	$1, 304(%rsp), %xmm1, %xmm5
	vpunpcklqdq	%xmm4, %xmm5, %xmm0
	vmovd	292(%rsp), %xmm6
	vpinsrd	$1, 296(%rsp), %xmm6, %xmm4
	vmovd	284(%rsp), %xmm1
	vpinsrd	$1, 288(%rsp), %xmm1, %xmm5
	vpunpcklqdq	%xmm4, %xmm5, %xmm6
	vinserti128	$0x1, %xmm0, %ymm6, %ymm4
	vpblendw	$85, %ymm4, %ymm2, %ymm0
	vpackusdw	%ymm0, %ymm7, %ymm1
	vmovdqu	48(%rsp), %ymm7
	vpermq	$216, %ymm1, %ymm5
	vmovd	516(%rsp), %xmm1
	vpand	%ymm7, %ymm5, %ymm6
	vpand	%ymm7, %ymm3, %ymm3
	vpackuswb	%ymm6, %ymm3, %ymm4
	vpermq	$216, %ymm4, %ymm0
	vmovd	508(%rsp), %xmm3
	vpinsrd	$1, 520(%rsp), %xmm1, %xmm5
	vpinsrd	$1, 512(%rsp), %xmm3, %xmm6
	vpunpcklqdq	%xmm5, %xmm6, %xmm5
	vmovd	500(%rsp), %xmm4
	vpinsrd	$1, 504(%rsp), %xmm4, %xmm3
	vmovd	492(%rsp), %xmm1
	vpinsrd	$1, 496(%rsp), %xmm1, %xmm6
	vpunpcklqdq	%xmm3, %xmm6, %xmm4
	vinserti128	$0x1, %xmm5, %ymm4, %ymm1
	vmovd	468(%rsp), %xmm3
	vpblendw	$85, %ymm1, %ymm2, %ymm1
	vpinsrd	$1, 472(%rsp), %xmm3, %xmm4
	vmovd	476(%rsp), %xmm5
	vpinsrd	$1, 480(%rsp), %xmm5, %xmm6
	vpunpcklqdq	%xmm6, %xmm4, %xmm6
	vmovd	460(%rsp), %xmm5
	vpinsrd	$1, 464(%rsp), %xmm5, %xmm4
	vmovd	524(%rsp), %xmm3
	vpinsrd	$1, 456(%rsp), %xmm3, %xmm5
	vpunpcklqdq	%xmm4, %xmm5, %xmm4
	vinserti128	$0x1, %xmm6, %ymm4, %ymm6
	vpblendw	$85, %ymm6, %ymm2, %ymm3
	vmovd	436(%rsp), %xmm6
	vpackusdw	%ymm3, %ymm1, %ymm5
	vpermq	$216, %ymm5, %ymm4
	vpand	%ymm7, %ymm4, %ymm4
	vmovd	428(%rsp), %xmm1
	vpinsrd	$1, 440(%rsp), %xmm6, %xmm3
	vpinsrd	$1, 432(%rsp), %xmm1, %xmm5
	vpunpcklqdq	%xmm3, %xmm5, %xmm6
	vmovd	484(%rsp), %xmm1
	vmovd	420(%rsp), %xmm3
	vpinsrd	$1, 424(%rsp), %xmm3, %xmm5
	vpinsrd	$1, 488(%rsp), %xmm1, %xmm3
	vpunpcklqdq	%xmm5, %xmm3, %xmm5
	vmovd	388(%rsp), %xmm3
	vinserti128	$0x1, %xmm6, %ymm5, %ymm1
	vpblendw	$85, %ymm1, %ymm2, %ymm1
	vmovd	396(%rsp), %xmm6
	vpinsrd	$1, 400(%rsp), %xmm6, %xmm5
	vpinsrd	$1, 392(%rsp), %xmm3, %xmm6
	vpunpcklqdq	%xmm5, %xmm6, %xmm5
	vmovd	452(%rsp), %xmm3
	vpinsrd	$1, 384(%rsp), %xmm3, %xmm6
	vmovd	444(%rsp), %xmm3
	vpinsrd	$1, 448(%rsp), %xmm3, %xmm3
	vpunpcklqdq	%xmm6, %xmm3, %xmm6
	vinserti128	$0x1, %xmm5, %ymm6, %ymm5
	vpblendw	$85, %ymm5, %ymm2, %ymm2
	vmovdqu	%ymm0, -40(%r11)
	vpackusdw	%ymm2, %ymm1, %ymm3
	vpermq	$216, %ymm3, %ymm6
	vpand	%ymm7, %ymm6, %ymm7
	vpackuswb	%ymm7, %ymm4, %ymm5
	vpermq	$216, %ymm5, %ymm1
	vmovdqu	%ymm1, -72(%r11)
	vmovdqu	244(%rsp), %ymm0
	vpshufb	.LC45(%rip), %ymm0, %ymm2
	vpermq	$78, %ymm2, %ymm3
	vpshufb	.LC46(%rip), %ymm0, %ymm6
	vpor	%ymm3, %ymm6, %ymm4
	vmovq	%xmm4, -8(%r11)
	cmpl	$6, %r13d
	jg	.L671
	leal	-7(%rbx), %r15d
	shrl	$3, %r15d
	leal	9(%r15,%r15,8), %ecx
	sall	$3, %r15d
	leal	0(,%rcx,8), %r13d
	negl	%r15d
	movl	%r13d, 88(%rsp)
	leal	-8(%rbx,%r15), %edi
.L669:
	testl	%edi, %edi
	js	.L672
.L683:
	movslq	88(%rsp), %r11
	movslq	%edi, %r13
	movl	$3435973837, %edx
	leaq	io_buf(%rip), %r12
	vmovdqa	.LC45(%rip), %ymm7
	vmovdqa	.LC46(%rip), %ymm3
	leaq	C(%rip), %r15
	vmovq	.LC47(%rip), %xmm2
	leal	8(%r11), %eax
	movq	%r11, 48(%rsp)
	leaq	(%r12,%r11), %r14
	cltq
	movq	%rax, 80(%rsp)
	.p2align 4,,10
	.p2align 3
.L673:
	movl	(%r15,%r13,4), %eax
	movq	%r14, %rsi
	movq	80(%rsp), %r8
	subq	48(%rsp), %rsi
	movq	%rax, %r10
	imulq	%rdx, %rax
	shrq	$35, %rax
	leal	(%rax,%rax,4), %ebp
	movl	%eax, %ecx
	imulq	%rdx, %rcx
	addl	%ebp, %ebp
	subl	%ebp, %r10d
	orl	$48, %r10d
	movb	%r10b, (%rsi,%r8)
	shrq	$35, %rcx
	movl	%ecx, %r8d
	imulq	%rdx, %r8
	shrq	$35, %r8
	movl	%r8d, %r9d
	imulq	%rdx, %r9
	shrq	$35, %r9
	movl	%r9d, %r10d
	imulq	%rdx, %r10
	shrq	$35, %r10
	movl	%r10d, %r11d
	imulq	%rdx, %r11
	shrq	$35, %r11
	movl	%r11d, %ebx
	imulq	%rdx, %rbx
	shrq	$35, %rbx
	movl	%ebx, %esi
	imulq	%rdx, %rsi
	shrq	$35, %rsi
	movl	%esi, %r12d
	movl	%esi, %ebp
	imulq	%rdx, %r12
	leal	(%rsi,%rsi,4), %esi
	addl	%esi, %esi
	shrq	$35, %r12
	leal	(%r12,%r12,4), %r12d
	addl	%r12d, %r12d
	subl	%r12d, %ebp
	movl	%ebx, %r12d
	leal	(%rbx,%rbx,4), %ebx
	vmovd	%ebp, %xmm0
	movl	%r11d, %ebp
	leal	(%r11,%r11,4), %r11d
	addl	%ebx, %ebx
	subl	%esi, %r12d
	subl	%ebx, %ebp
	movl	%r10d, %ebx
	addl	%r11d, %r11d
	vpinsrd	$1, %r12d, %xmm0, %xmm0
	leal	(%r10,%r10,4), %r10d
	vmovd	%ebp, %xmm5
	movl	%r9d, %esi
	addl	%r10d, %r10d
	movl	%r8d, %ebp
	subl	%r11d, %ebx
	leal	(%r9,%r9,4), %r9d
	subl	%r10d, %esi
	movl	%ecx, %r11d
	vpinsrd	$1, %ebx, %xmm5, %xmm5
	leal	(%r8,%r8,4), %r8d
	addl	%r9d, %r9d
	vmovd	%esi, %xmm1
	addl	%r8d, %r8d
	subl	%r9d, %ebp
	subq	$1, %r13
	leal	(%rcx,%rcx,4), %ecx
	subl	%r8d, %r11d
	vpinsrd	$1, %ebp, %xmm1, %xmm1
	addq	$9, %r14
	addl	%ecx, %ecx
	vmovd	%r11d, %xmm6
	subl	%ecx, %eax
	vpinsrd	$1, %eax, %xmm6, %xmm4
	vpunpcklqdq	%xmm4, %xmm1, %xmm6
	vpunpcklqdq	%xmm5, %xmm0, %xmm4
	vinserti128	$0x1, %xmm6, %ymm4, %ymm6
	vpshufb	%ymm7, %ymm6, %ymm1
	vpshufb	%ymm3, %ymm6, %ymm0
	vpermq	$78, %ymm1, %ymm5
	vpor	%ymm5, %ymm0, %ymm4
	vpor	%xmm2, %xmm4, %xmm6
	vmovq	%xmm6, -9(%r14)
	testl	%r13d, %r13d
	jns	.L673
	leal	(%rdi,%rdi,8), %r13d
	movl	88(%rsp), %edi
	leal	9(%rdi,%r13), %r14d
	movl	%r14d, 88(%rsp)
.L672:
	cmpb	$48, io_buf(%rip)
	jne	.L689
	movl	88(%rsp), %r15d
	leaq	io_buf(%rip), %rax
	xorl	%ebx, %ebx
	leal	-1(%r15), %r12d
	movl	%r12d, %edx
	andl	$7, %edx
	je	.L675
	cmpl	$0, %r12d
	jle	.L674
	addq	$1, %rax
	movl	$1, %ebx
	cmpb	$48, (%rax)
	jne	.L674
	cmpl	$1, %edx
	je	.L675
	cmpl	$2, %edx
	je	.L789
	cmpl	$3, %edx
	je	.L790
	cmpl	$4, %edx
	je	.L791
	cmpl	$5, %edx
	je	.L792
	cmpl	$6, %edx
	je	.L793
	addq	$1, %rax
	addl	$1, %ebx
	cmpb	$48, (%rax)
	jne	.L674
.L793:
	addq	$1, %rax
	addl	$1, %ebx
	cmpb	$48, (%rax)
	jne	.L674
.L792:
	addq	$1, %rax
	addl	$1, %ebx
	cmpb	$48, (%rax)
	jne	.L674
.L791:
	addq	$1, %rax
	addl	$1, %ebx
	cmpb	$48, (%rax)
	jne	.L674
.L790:
	addq	$1, %rax
	addl	$1, %ebx
	cmpb	$48, (%rax)
	jne	.L674
.L789:
	addq	$1, %rax
	addl	$1, %ebx
	cmpb	$48, (%rax)
	jne	.L674
.L675:
	cmpl	%ebx, %r12d
	jle	.L674
	cmpb	$48, 1(%rax)
	leal	1(%rbx), %r10d
	movl	%r10d, %ebx
	jne	.L674
	addl	$1, %ebx
	cmpb	$48, 2(%rax)
	jne	.L674
	cmpb	$48, 3(%rax)
	leal	2(%r10), %ebx
	jne	.L674
	cmpb	$48, 4(%rax)
	leal	3(%r10), %ebx
	jne	.L674
	cmpb	$48, 5(%rax)
	leal	4(%r10), %ebx
	jne	.L674
	cmpb	$48, 6(%rax)
	leal	5(%r10), %ebx
	jne	.L674
	cmpb	$48, 7(%rax)
	leal	6(%r10), %ebx
	jne	.L674
	leal	7(%r10), %ebx
	addq	$8, %rax
	cmpb	$48, (%rax)
	je	.L675
.L674:
	movq	104(%rsp), %rsi
	movl	$1, %ecx
	vzeroupper
	call	*%rsi
	movl	88(%rsp), %ebp
	movslq	%ebx, %rcx
	movl	$1, %edx
	leaq	io_buf(%rip), %r8
	movq	%rax, %r9
	addq	%r8, %rcx
	subl	%ebx, %ebp
	movslq	%ebp, %r8
	call	fwrite
	jmp	.L830
.L647:
	cmpl	$0, B(%rip)
	jne	.L841
.L649:
	movl	$1, %ecx
	call	*104(%rsp)
	movl	$1, %r8d
	movl	$1, %edx
	leaq	.LC43(%rip), %rcx
	movq	%rax, %r9
	call	fwrite
	nop
.L830:
	vmovaps	576(%rsp), %xmm6
	xorl	%eax, %eax
	vmovaps	592(%rsp), %xmm7
	addq	$616, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L661:
	leal	469762049(%rax), %eax
	movq	%rbp, %rcx
	movl	$354521948, %r11d
	movq	$469762049, 192(%rsp)
	movq	$0, 200(%rsp)
	subq	%rdx, %rax
	mulq	%r11
	movq	%rdx, 216(%rsp)
	movq	48(%rsp), %rdx
	movq	%rax, 208(%rsp)
	call	__umodti3
	movq	48(%rsp), %rcx
	movq	%rbp, %r8
	movabsq	$1002772198720536577, %r9
	vmovq	%xmm0, %rax
	movq	$1000000000, 176(%rsp)
	movq	$0, 184(%rsp)
	mulq	%r9
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	movq	96(%rsp), %rdx
	addq	%r12, %rsi
	adcq	%r13, %rdi
	movq	%rsi, 192(%rsp)
	movq	%rdi, 200(%rsp)
	leaq	C(%rip), %r12
	call	__udivmodti4
	movq	208(%rsp), %r13
	movl	%r13d, (%r12,%r14)
	addq	$4, %r14
	cmpq	88(%rsp), %r14
	vmovq	%xmm0, %r12
	vpextrq	$1, %xmm0, %r13
	jne	.L664
	jmp	.L663
.L666:
	cmpl	$6, %r11d
	jg	.L682
	xorl	%r15d, %r15d
	movl	%r15d, 88(%rsp)
	jmp	.L669
.L646:
	movl	A(%rip), %eax
	testl	%eax, %eax
	je	.L649
	cmpl	$1, %r14d
	je	.L842
.L651:
	leal	1(%r14), %r13d
	movl	%r14d, %ebx
	jmp	.L648
.L639:
	xorl	%esi, %esi
	testb	%bl, %bl
	jne	.L684
	xorl	%r9d, %r9d
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	236(%rsp), %rbp
	xorl	%r14d, %r14d
	xorl	%r13d, %r13d
	movl	%r9d, na(%rip)
	movl	%r9d, nb(%rip)
	leaq	io_buf(%rip), %rcx
	leaq	A(%rip), %r9
	movq	%rbp, 32(%rsp)
	call	parse_blocks_simd
	xorl	%r10d, %r10d
	movq	%rbp, 32(%rsp)
	xorl	%edx, %edx
	leaq	B(%rip), %r9
	movl	%r10d, 236(%rsp)
	leaq	io_buf(%rip), %rcx
	call	parse_blocks_simd
	jmp	.L681
.L687:
	movl	%esi, %ebp
	movl	%edi, %esi
.L641:
	leal	8(%rbp), %eax
	movl	$9, %r9d
	xorl	%r10d, %r10d
	cltd
	xorl	%r8d, %r8d
	movl	%r10d, nb(%rip)
	idivl	%r9d
	movl	%ebp, %edx
	leaq	236(%rsp), %r14
	leaq	A(%rip), %r9
	movq	%r14, 32(%rsp)
	leaq	io_buf(%rip), %rcx
	movl	%eax, %r13d
	movl	%eax, na(%rip)
	call	parse_blocks_simd
	xorl	%ecx, %ecx
	movq	%r14, 32(%rsp)
	movl	%esi, %r8d
	movl	%ecx, 236(%rsp)
	leaq	B(%rip), %r9
	movl	%esi, %edx
	leaq	io_buf(%rip), %rcx
	call	parse_blocks_simd
	cmpl	$1, %r13d
	je	.L680
	xorl	%r14d, %r14d
	jmp	.L681
.L842:
	movl	B(%rip), %r12d
	testl	%r12d, %r12d
	je	.L649
	imulq	%r12, %rax
	xorl	%edx, %edx
	movl	$1000000000, %r8d
	movq	%rax, %r13
	divq	%r8
	movl	%edx, C(%rip)
	cmpq	$999999999, %r13
	jbe	.L653
	movl	%eax, 4+C(%rip)
	movl	$2, %ebx
	jmp	.L653
.L841:
	leal	1(%rbx), %r13d
	jmp	.L648
.L840:
	xorl	%eax, %eax
	movl	%eax, 88(%rsp)
	jmp	.L683
.L680:
	cmpl	$0, A(%rip)
	je	.L649
	xorl	%r14d, %r14d
	jmp	.L651
.L689:
	xorl	%ebx, %ebx
	jmp	.L674
.L839:
	addl	$1, %ebx
	jmp	.L653
	.seh_endproc
	.section .rdata,"dr"
	.align 32
.LC0:
	.long	-9
	.long	-18
	.long	-27
	.long	-36
	.long	-45
	.long	-54
	.long	-63
	.long	-72
	.align 32
.LC1:
	.long	-7
	.long	-16
	.long	-25
	.long	-34
	.long	-43
	.long	-52
	.long	-61
	.long	-70
	.align 32
.LC2:
	.long	-6
	.long	-15
	.long	-24
	.long	-33
	.long	-42
	.long	-51
	.long	-60
	.long	-69
	.align 32
.LC3:
	.long	-5
	.long	-14
	.long	-23
	.long	-32
	.long	-41
	.long	-50
	.long	-59
	.long	-68
	.align 32
.LC4:
	.long	-4
	.long	-13
	.long	-22
	.long	-31
	.long	-40
	.long	-49
	.long	-58
	.long	-67
	.align 32
.LC5:
	.long	-3
	.long	-12
	.long	-21
	.long	-30
	.long	-39
	.long	-48
	.long	-57
	.long	-66
	.align 32
.LC6:
	.long	-2
	.long	-11
	.long	-20
	.long	-29
	.long	-38
	.long	-47
	.long	-56
	.long	-65
	.align 32
.LC7:
	.long	-1
	.long	-10
	.long	-19
	.long	-28
	.long	-37
	.long	-46
	.long	-55
	.long	-64
	.align 32
.LC13:
	.long	1299317818
	.long	0
	.long	1299317818
	.long	0
	.long	1299317818
	.long	0
	.long	1299317818
	.long	0
	.align 32
.LC14:
	.long	4
	.long	0
	.long	4
	.long	0
	.long	4
	.long	0
	.long	4
	.long	0
	.align 32
.LC15:
	.long	998244353
	.long	0
	.long	998244353
	.long	0
	.long	998244353
	.long	0
	.long	998244353
	.long	0
	.align 32
.LC18:
	.long	0
	.long	2
	.long	4
	.long	6
	.long	1
	.long	3
	.long	5
	.long	7
	.align 8
.LC22:
	.quad	998244352
	.align 8
.LC23:
	.quad	998244353
	.align 32
.LC24:
	.long	1183581783
	.long	0
	.long	1183581783
	.long	0
	.long	1183581783
	.long	0
	.long	1183581783
	.long	0
	.align 32
.LC25:
	.long	1004535809
	.long	0
	.long	1004535809
	.long	0
	.long	1004535809
	.long	0
	.long	1004535809
	.long	0
	.align 8
.LC31:
	.quad	1004535808
	.align 8
.LC32:
	.quad	1004535809
	.align 32
.LC33:
	.long	613566672
	.long	0
	.long	613566672
	.long	0
	.long	613566672
	.long	0
	.long	613566672
	.long	0
	.align 32
.LC34:
	.long	9
	.long	0
	.long	9
	.long	0
	.long	9
	.long	0
	.long	9
	.long	0
	.align 32
.LC35:
	.long	469762049
	.long	0
	.long	469762049
	.long	0
	.long	469762049
	.long	0
	.long	469762049
	.long	0
	.align 8
.LC41:
	.quad	469762048
	.align 8
.LC42:
	.quad	469762049
	.align 32
.LC45:
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	0
	.byte	4
	.byte	8
	.byte	12
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.align 32
.LC46:
	.byte	0
	.byte	4
	.byte	8
	.byte	12
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	-128
	.byte	8
	.byte	9
	.byte	10
	.byte	11
	.byte	12
	.byte	13
	.byte	14
	.byte	15
	.byte	0
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.byte	6
	.byte	7
	.byte	8
	.byte	9
	.byte	10
	.byte	11
	.byte	12
	.byte	13
	.byte	14
	.byte	15
	.align 8
.LC47:
	.byte	48
	.byte	48
	.byte	48
	.byte	48
	.byte	48
	.byte	48
	.byte	48
	.byte	48
	.def	__udivmodti4;	.scl	2;	.type	32;	.endef
	.def	__umodti3;	.scl	2;	.type	32;	.endef
	.def	__main;	.scl	2;	.type	32;	.endef
	.ident	"GCC: (x86_64-posix-seh-rev0, Built by MinGW-Builds project) 15.2.0"
	.def	memcpy;	.scl	2;	.type	32;	.endef
	.def	memset;	.scl	2;	.type	32;	.endef
	.def	fread;	.scl	2;	.type	32;	.endef
	.def	fwrite;	.scl	2;	.type	32;	.endef
