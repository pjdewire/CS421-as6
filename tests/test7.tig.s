.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L7:
pushl %ecx
pushl %edx
leal L5,%edi
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
jmp L6
L6:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L5:
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
L9:
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
movl %eax,%edi
popl %edx
popl %ecx
leal L4,%edi
movl %edi,%eax
jmp L8
L8:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L4:
.4byte 1
.ascii " "
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
L11:
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
movl %eax,%edi
popl %edx
popl %ecx
movl $0,%edi
movl %edi,%eax
jmp L10
L10:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
