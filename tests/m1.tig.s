.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $212,%esp
pushl %ebx
pushl %edi
pushl %esi
L59:
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
movl $-208,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%edi
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L25
addl $4,%esp
movl %eax,%esi
popl %edx
popl %ecx
popl %eax
movl %esi,(%edi)
movl $-212,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%esi
movl $-204,%edi
movl %edi,%edi
addl %ebp,%edi
movl %edi,%edi
pushl %eax
pushl %ecx
pushl %edx
call getch
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
movl %ebx,(%edi)
pushl %eax
pushl %ecx
pushl %edx
pushl %ebp
call L25
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%esi)
movl %ebp,%esi
pushl %eax
pushl %ecx
pushl %edx
movl -212(%ebp),%edi
pushl %edi
movl -208(%ebp),%edi
pushl %edi
pushl %ebp
call L26
addl $12,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
pushl %ecx
pushl %edx
pushl %edi
pushl %esi
call L28
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
movl $1,%edi
movl %edi,%eax
jmp L58
L58:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl L28
.type L28, @function
L28:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L61:
movl $0,%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
je L55
L56:
pushl %ecx
pushl %edx
movl 12(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
pushl %edi
movl 8(%ebp),%edi
pushl %edi
call L27
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
pushl %ecx
pushl %edx
leal L54,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%ebx
movl $1,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
pushl %edi
movl 8(%ebp),%edi
pushl %edi
call L28
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,%edi
L57:
movl %edi,%eax
jmp L60
L55:
pushl %eax
pushl %ecx
pushl %edx
leal L53,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
jmp L57
L60:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L54:
.4byte 1
.ascii " "
L53:
.4byte 1
.ascii "\n"
.globl L27
.type L27, @function
L27:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L63:
movl $0,%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
jl L50
L51:
movl $0,%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
jg L47
L48:
pushl %eax
pushl %ecx
pushl %edx
leal L46,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
L49:
movl %edi,%edi
L52:
movl %edi,%eax
jmp L62
L50:
pushl %ecx
pushl %edx
leal L45,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
pushl %eax
pushl %ecx
pushl %edx
movl $0,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
pushl %esi
pushl %ebp
call L41
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,%edi
jmp L52
L47:
pushl %eax
pushl %ecx
pushl %edx
movl 12(%ebp),%edi
pushl %edi
pushl %ebp
call L41
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
jmp L49
L62:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L46:
.4byte 1
.ascii "0"
L45:
.4byte 1
.ascii "-"
.globl L41
.type L41, @function
L41:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L65:
movl $0,%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
jg L43
L44:
movl $0,%edi
movl %edi,%eax
jmp L64
L43:
pushl %ecx
pushl %edx
movl $0,%edx
movl 12(%ebp),%edi
movl %edi,%eax
movl $10,%edi
idivl %edi
pushl %eax
movl 8(%ebp),%edi
pushl %edi
call L41
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
movl 12(%ebp),%ebx
movl $0,%edx
movl 12(%ebp),%edi
movl %edi,%eax
movl $10,%edi
idivl %edi
movl $10,%edi
movl %eax,%esi
imull %edi,%esi
movl %ebx,%edi
subl %esi,%edi
movl %edi,%esi
pushl %eax
pushl %ecx
pushl %edx
leal L42,%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%ebx
popl %edx
popl %ecx
popl %eax
pushl %eax
pushl %ecx
pushl %edx
movl %esi,%edi
addl %ebx,%edi
pushl %edi
call chr
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
pushl %eax
pushl %ecx
pushl %edx
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
jmp L44
L64:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L42:
.4byte 1
.ascii "0"
.globl L26
.type L26, @function
L26:
pushl %ebp
movl %esp,%ebp
subl $200,%esp
pushl %ebx
pushl %edi
pushl %esi
L67:
movl $0,%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
je L38
L39:
movl $0,%esi
movl 16(%ebp),%edi
cmpl %esi,%edi
je L35
L36:
movl 16(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%eax
movl 12(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
cmpl %eax,%edi
jl L32
L33:
pushl %eax
pushl %ecx
pushl %edx
movl $8,%edi
pushl %edi
call allocRecord
addl $4,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl 16(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
	movl	-8(%ebp), %edx # load pseudo-register
movl %edi,0(%edx)
movl $4,%edi
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
movl %esi,%eax
pushl %eax
pushl %ecx
pushl %edx
movl 16(%ebp),%ebx
movl $1,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
pushl %edi
movl 12(%ebp),%edi
pushl %edi
movl 8(%ebp),%edi
pushl %edi
call L26
addl $12,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%eax)
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
L34:
movl %edi,%edi
L37:
movl %edi,%edi
L40:
movl %edi,%eax
jmp L66
L38:
movl 16(%ebp),%edi
movl %edi,%edi
jmp L40
L35:
movl 12(%ebp),%edi
movl %edi,%edi
jmp L37
L32:
pushl %eax
pushl %ecx
pushl %edx
movl $8,%edi
pushl %edi
call allocRecord
addl $4,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl 12(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
	movl	-8(%ebp), %edx # load pseudo-register
movl %edi,0(%edx)
movl $4,%edi
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
addl %edi,%esi
movl %esi,%eax
pushl %eax
pushl %ecx
pushl %edx
movl 16(%ebp),%edi
pushl %edi
movl 12(%ebp),%ebx
movl $1,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
pushl %edi
movl 8(%ebp),%edi
pushl %edi
call L26
addl $12,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%eax)
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,%edi
jmp L34
L66:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl L25
.type L25, @function
L25:
pushl %ebp
movl %esp,%ebp
subl $208,%esp
pushl %ebx
pushl %edi
pushl %esi
L69:
movl $-204,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%esi
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
movl %ebx,(%esi)
movl $-208,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%esi
pushl %eax
pushl %ecx
pushl %edx
movl -204(%ebp),%edi
pushl %edi
movl 8(%ebp),%edi
pushl %edi
call L1
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%esi)
movl $0,%eax
movl -204(%ebp),%ebx
movl $0,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl (%edi),%edi
cmpl %eax,%edi
je L30
L29:
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
movl -208(%ebp),%edi
movl %edi,0(%ebx)
movl $4,%edi
movl %ebx,%esi
addl %edi,%esi
movl %esi,%esi
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%edi
pushl %edi
call L25
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,(%esi)
movl %ebx,%edi
L31:
movl %edi,%eax
jmp L68
L30:
movl $0,%edi
movl %edi,%edi
jmp L31
L68:
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
L71:
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
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
movl %esi,%edi
addl %eax,%edi
movl %edi,%esi
pushl %eax
pushl %ecx
pushl %edx
leal L22,%edi
pushl %edi
call ord
addl $4,%esp
movl %eax,%eax
popl %edx
popl %ecx
popl %eax
movl %esi,%edi
subl %eax,%edi
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
jmp L70
L70:
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
jmp L72
L72:
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
L74:
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
jmp L73
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
L73:
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
