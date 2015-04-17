.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $208,%esp
pushl %ebx
pushl %edi
pushl %esi
L2:
movl $8,%edi
movl %edi,-204(%ebp)
movl $-208,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edi
pushl %edi
movl -204(%ebp),%esi
movl -204(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
pushl %esi
call initArray
addl $8,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl -204(%ebp),%esi
movl -204(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
	movl	-8(%ebp), %edx # load pseudo-register
movl %esi,(%edx)
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%ebx)
movl -204(%ebp),%esi
movl -204(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
movl %esi,%eax
jmp L1
L1:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
