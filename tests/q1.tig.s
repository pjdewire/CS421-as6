.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $212,%esp
pushl %ebx
pushl %edi
pushl %esi
L4:
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
movl $-204,%edi
movl %edi,%edi
addl %ebp,%edi
movl (%edi),%edi
movl %edi,(%esi)
movl %esi,(%ebx)
movl $-212,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edi
pushl %edi
movl -204(%ebp),%esi
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
movl $-204,%edi
movl %edi,%edi
addl %ebp,%edi
movl (%edi),%edi
movl %edi,(%esi)
movl %esi,(%ebx)
pushl %ecx
pushl %edx
movl $0,%edi
pushl %edi
pushl %ebp
call L1
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
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
.globl L1
.type L1, @function
L1:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L6:
pushl %eax
pushl %ecx
pushl %edx
leal L2,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
jmp L5
L5:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L2:
.4byte 3
.ascii "hi\n"
