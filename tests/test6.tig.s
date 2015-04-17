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
leal L4,%edi
pushl %edi
movl $0,%edi
pushl %edi
pushl %ebp
call L1
addl $12,%esp
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
L4:
.4byte 4
.ascii "str2"
.globl L2
.type L2, @function
L2:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L8:
pushl %eax
pushl %ecx
pushl %edx
leal L3,%edi
pushl %edi
movl 12(%ebp),%edi
pushl %edi
movl 8(%ebp),%edi
pushl %edi
call L1
addl $12,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L7
L7:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L3:
.4byte 3
.ascii "str"
.globl L1
.type L1, @function
L1:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L10:
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
pushl %esi
movl 8(%ebp),%edi
pushl %edi
call L2
addl $8,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L9
L9:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
