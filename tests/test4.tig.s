.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L6:
pushl %ecx
pushl %edx
movl $10,%edi
pushl %edi
pushl %ebp
call L1
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
movl $1,%edi
movl %edi,%eax
jmp L5
L5:
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
L8:
movl $0,%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
je L2
L3:
movl 12(%ebp),%edi
movl %edi,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
pushl %esi
movl 8(%ebp),%edi
pushl %edi
call L1
addl $8,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl %ebx,%edi
imull %esi,%edi
movl %edi,%edi
L4:
movl %edi,%eax
jmp L7
L2:
movl $1,%edi
movl %edi,%edi
jmp L4
L7:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
