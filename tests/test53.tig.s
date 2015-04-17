.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L11:
movl $0,%esi
movl $1,%edi
cmpl %esi,%edi
je L7
L8:
movl $0,%esi
movl $1,%edi
cmpl %esi,%edi
jne L4
L5:
movl $8,%edi
movl %edi,%edi
L6:
movl %edi,%edi
L9:
movl $1,%edi
movl %edi,%eax
jmp L10
L7:
movl $1,%esi
movl $1,%edi
cmpl %esi,%edi
je L1
L2:
movl $6,%edi
movl %edi,%edi
L3:
movl %edi,%edi
jmp L9
L1:
movl $5,%edi
movl %edi,%edi
jmp L3
L4:
movl $7,%edi
movl %edi,%edi
jmp L6
L10:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
