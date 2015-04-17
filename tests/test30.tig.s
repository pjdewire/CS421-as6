.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $204,%esp
pushl %ebx
pushl %edi
pushl %esi
L7:
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
movl -204(%ebp),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ebx
movl -204(%ebp),%edi
movl (%edi),%esi
movl $2,%edi
cmpl %esi,%edi
jl L2
L4:
pushl %ecx
pushl %edx
leal L5,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
pushl %ecx
pushl %edx
call exit
movl %eax,%edi
popl %edx
popl %ecx
L1:
movl (%ebx),%edi
movl $1,%edi
movl %edi,%eax
jmp L6
L2:
movl $0,%esi
movl $2,%edi
cmpl %esi,%edi
jl L4
L3:
movl -204(%ebp),%ebx
movl $2,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ebx
jmp L1
L6:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L5:
.4byte 30
.ascii "Array subscript out of bounds\n"
