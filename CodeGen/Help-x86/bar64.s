	.section	.rodata
Hello:
	.string	"hello %d\n"
.text
.globl	f
	.type	f, @function
f:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	
	movl	%eax, %esi
	movl	$Hello, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %eax
	
	call	g
	
	addl	-4(%rbp), %eax
	leave
	ret

