.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $204,%esp
pushl %ebx
pushl %edi
pushl %esi
L26:
movl $-204,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%edi
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl %esi,(%edi)
movl %ebp,%esi
pushl %eax
pushl %ecx
pushl %edx
movl $4,%edi
pushl %edi
call allocRecord
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl $0,%edi
movl %edi,0(%ebx)
pushl %eax
pushl %ecx
pushl %edx
pushl %ebx
pushl %esi
call L1
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,%eax
jmp L25
L25:
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
subl $204,%esp
pushl %ebx
pushl %edi
pushl %esi
L28:
movl $0,%edi
movl %edi,-204(%ebp)
pushl %ecx
pushl %edx
pushl %ebp
call L3
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
movl 12(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%esi
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%edi
movl -204(%edi),%edi
pushl %edi
pushl %ebp
call L2
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%esi)
L23:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%edi
movl -204(%edi),%edi
pushl %edi
pushl %ebp
call L2
addl $8,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl $0,%edi
cmpl %edi,%esi
je L21
L24:
movl $-204,%edi
movl %edi,%edi
addl %ebp,%edi
movl %edi,%ebx
movl -204(%ebp),%esi
movl $10,%edi
movl %esi,%esi
imull %edi,%esi
movl %esi,%esi
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%edi
movl -204(%edi),%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl %esi,%edi
	movl	-8(%ebp), %ecx # load pseudo-register
addl %ecx,%edi
movl %edi,%esi
pushl %eax
pushl %ecx
pushl %edx
leal L22,%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl %esi,%edi
	movl	-8(%ebp), %ecx # load pseudo-register
subl %ecx,%edi
movl %edi,(%ebx)
movl 8(%ebp),%esi
movl $-204,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%edi
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl %esi,(%edi)
jmp L23
L21:
movl -204(%ebp),%edi
movl %edi,%eax
jmp L27
L27:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L22:
.4byte 1
.ascii "0"
.globl L3
.type L3, @function
L3:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L19:
pushl %eax
pushl %ecx
pushl %edx
leal L12,%edi
pushl %edi
movl 8(%ebp),%edi
movl 8(%edi),%edi
movl -204(%edi),%edi
pushl %edi
call stringEqual
addl $8,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl $0,%edi
cmpl %edi,%esi
je L17
L16:
movl $1,%edi
movl %edi,%esi
L18:
movl $0,%edi
cmpl %edi,%esi
je L11
L20:
movl 8(%ebp),%edi
movl 8(%edi),%esi
movl $-204,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%edi
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl %esi,(%edi)
jmp L19
L17:
movl $1,%edi
movl %edi,%ebx
pushl %eax
pushl %ecx
pushl %edx
leal L13,%edi
pushl %edi
movl 8(%ebp),%edi
movl 8(%edi),%edi
movl -204(%edi),%edi
pushl %edi
call stringEqual
addl $8,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl $0,%edi
cmpl %edi,%esi
je L15
L14:
movl %ebx,%esi
jmp L18
L15:
movl $0,%edi
movl %edi,%ebx
jmp L14
L11:
movl $0,%edi
movl %edi,%eax
jmp L29
L29:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L13:
.4byte 1
.ascii "\n"
L12:
.4byte 1
.ascii " "
.globl L2
.type L2, @function
L2:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L31:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%edi
movl 8(%edi),%edi
movl -204(%edi),%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,%esi
pushl %eax
pushl %ecx
pushl %edx
leal L4,%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
cmpl %edi,%esi
jge L8
L9:
movl $0,%edi
movl %edi,%edi
L10:
movl %edi,%eax
jmp L30
L8:
movl $1,%edi
movl %edi,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%edi
movl 8(%edi),%edi
movl -204(%edi),%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,%esi
pushl %eax
pushl %ecx
pushl %edx
leal L5,%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
cmpl %edi,%esi
jle L6
L7:
movl $0,%edi
movl %edi,%ebx
L6:
movl %ebx,%edi
jmp L10
L30:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L5:
.4byte 1
.ascii "9"
L4:
.4byte 1
.ascii "0"
