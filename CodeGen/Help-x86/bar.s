.section	.rodata
Hello:
	.string	"hello %d\n"
.text
	.align 4
.globl f
	.type	 f,@function
f:
	pushl %ebp
	movl %esp,%ebp

	pushl %ebx
	movl 8(%ebp),%ebx

	pushl %ebx
	pushl $Hello
	call printf
        addl $8,%esp

	call g

	addl %ebx,%eax

        popl %ebx

	movl %ebp,%esp
	popl %ebp

	ret
