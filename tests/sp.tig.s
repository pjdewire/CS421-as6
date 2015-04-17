.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L2:
movl $1,%ecx
	movl	%ecx, -52(%ebp) # save pseudo-register
movl $2,%ecx
	movl	%ecx, -48(%ebp) # save pseudo-register
movl $3,%ecx
	movl	%ecx, -44(%ebp) # save pseudo-register
movl $4,%ecx
	movl	%ecx, -40(%ebp) # save pseudo-register
movl $5,%ecx
	movl	%ecx, -36(%ebp) # save pseudo-register
movl $6,%ecx
	movl	%ecx, -32(%ebp) # save pseudo-register
movl $7,%ecx
	movl	%ecx, -28(%ebp) # save pseudo-register
movl $8,%ecx
	movl	%ecx, -24(%ebp) # save pseudo-register
movl $9,%ecx
	movl	%ecx, -20(%ebp) # save pseudo-register
movl $10,%ecx
	movl	%ecx, -16(%ebp) # save pseudo-register
movl $11,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl $12,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl $13,%eax
movl $14,%ebx
movl $15,%esi
movl $16,%edi
movl %esi,%esi
addl %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %eax,%esi
addl %edi,%esi
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %esi,%edi
	movl	-12(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
	movl	-16(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %esi,%edi
	movl	-20(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
	movl	-24(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %esi,%edi
	movl	-28(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
	movl	-32(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %esi,%edi
	movl	-36(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
	movl	-40(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %esi,%edi
	movl	-44(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
	movl	-48(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
addl %esi,%edi
	movl	-52(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
movl $1,%edi
movl %edi,%eax
jmp L1
L1:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
