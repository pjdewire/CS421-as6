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
movl $0,%edi
movl %edi,0(%ebx)
movl $0,%edi
movl %edi,4(%ebx)
movl %ebx,(%esi)
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
