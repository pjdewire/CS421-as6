.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $204,%esp
pushl %ebx
pushl %edi
pushl %esi
L2:
movl $-204,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edi
pushl %edi
movl $10,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
pushl %esi
call initArray
addl $8,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl $10,%edi
movl %edi,(%esi)
movl %esi,(%ebx)
movl -204(%ebp),%edi
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
