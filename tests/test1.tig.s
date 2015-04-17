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
