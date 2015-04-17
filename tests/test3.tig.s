.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $204,%esp
pushl %ebx
pushl %edi
pushl %esi
L4:
movl $-204,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%esi
pushl %eax
pushl %ecx
pushl %edx
movl $8,%edi
pushl %edi
call allocRecord
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
leal L1,%edi
movl %edi,0(%ebx)
movl $1000,%edi
movl %edi,4(%ebx)
movl %ebx,(%esi)
leal L2,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -204(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%edi)
movl -204(%ebp),%edi
movl $1,%edi
movl %edi,%eax
jmp L3
L3:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L2:
.4byte 8
.ascii "Somebody"
L1:
.4byte 6
.ascii "Nobody"
