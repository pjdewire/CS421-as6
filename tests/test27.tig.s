.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $204,%esp
pushl %ebx
pushl %edi
pushl %esi
L3:
movl $0,%edi
movl %edi,-204(%ebp)
pushl %ecx
pushl %edx
movl $2,%edi
pushl %edi
pushl %ebp
call L1
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
movl $1,%edi
movl %edi,%eax
jmp L2
L2:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl L1
.type L1, @function
L1:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L5:
movl 12(%ebp),%edi
movl %edi,%eax
jmp L4
L4:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
