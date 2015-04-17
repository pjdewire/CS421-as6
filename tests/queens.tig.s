.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $220,%esp
pushl %ebx
pushl %edi
pushl %esi
L83:
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
movl $-216,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edi
pushl %edi
movl -204(%ebp),%esi
movl -204(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
pushl %esi
call initArray
addl $8,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl -204(%ebp),%esi
movl -204(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
	movl	-8(%ebp), %edx # load pseudo-register
movl %esi,(%edx)
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%ebx)
movl $-220,%edi
movl %ebp,%esi
addl %edi,%esi
movl %esi,%ebx
pushl %eax
pushl %ecx
pushl %edx
movl $0,%edi
pushl %edi
movl -204(%ebp),%esi
movl -204(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
pushl %esi
call initArray
addl $8,%esp
movl %eax,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
popl %edx
popl %ecx
popl %eax
movl -204(%ebp),%esi
movl -204(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
	movl	-8(%ebp), %edx # load pseudo-register
movl %esi,(%edx)
	movl	-8(%ebp), %ecx # load pseudo-register
movl %ecx,(%ebx)
pushl %ecx
pushl %edx
movl $0,%edi
pushl %edi
pushl %ebp
call L2
addl $8,%esp
movl %eax,%edi
popl %edx
popl %ecx
movl $1,%edi
movl %edi,%eax
jmp L82
L82:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
.globl L2
.type L2, @function
L2:
pushl %ebp
movl %esp,%ebp
subl $204,%esp
pushl %ebx
pushl %edi
pushl %esi
L85:
movl 8(%ebp),%edi
movl -204(%edi),%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
je L79
L80:
movl $0,%edi
movl %edi,-204(%ebp)
movl 8(%ebp),%edi
movl -204(%edi),%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -204(%ebp),%edi
	movl	-8(%ebp), %ecx # load pseudo-register
cmpl %ecx,%edi
jle L77
L23:
movl $0,%edi
movl %edi,%edi
L81:
movl %edi,%eax
jmp L84
L79:
pushl %eax
pushl %ecx
pushl %edx
movl 8(%ebp),%edi
pushl %edi
call L1
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
jmp L81
L77:
movl 8(%ebp),%edi
movl -208(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ebx
movl 8(%ebp),%edi
movl -208(%edi),%edi
movl (%edi),%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L25
L27:
pushl %ecx
pushl %edx
leal L9,%edi
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
L24:
movl $0,%esi
movl (%ebx),%edi
cmpl %esi,%edi
je L34
L35:
movl $0,%edi
movl %edi,%esi
L36:
movl $0,%edi
cmpl %edi,%esi
je L44
L43:
movl $1,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -220(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -16(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -220(%edi),%edi
movl (%edi),%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
cmpl %ebx,%esi
jl L38
L40:
pushl %ecx
pushl %edx
leal L9,%edi
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
L37:
movl $0,%esi
	movl	-16(%ebp), %ecx # load pseudo-register
movl (%ecx),%edi
cmpl %esi,%edi
je L41
L42:
movl $0,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
L41:
	movl	-12(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
L45:
movl $0,%edi
cmpl %edi,%esi
je L75
L74:
movl 8(%ebp),%edi
movl -208(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ebx
movl 8(%ebp),%edi
movl -208(%edi),%edi
movl (%edi),%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L47
L49:
pushl %ecx
pushl %edx
leal L9,%edi
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
L46:
movl $1,%edi
movl %edi,(%ebx)
movl 8(%ebp),%edi
movl -216(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -216(%edi),%edi
movl (%edi),%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
cmpl %ebx,%esi
jl L51
L53:
pushl %ecx
pushl %edx
leal L9,%edi
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
L50:
movl $1,%edi
	movl	-12(%ebp), %edx # load pseudo-register
movl %edi,(%edx)
movl 8(%ebp),%edi
movl -220(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -220(%edi),%edi
movl (%edi),%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
cmpl %ebx,%esi
jl L55
L57:
pushl %ecx
pushl %edx
leal L9,%edi
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
L54:
movl $1,%edi
	movl	-12(%ebp), %edx # load pseudo-register
movl %edi,(%edx)
movl 8(%ebp),%edi
movl -212(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ebx
movl 8(%ebp),%edi
movl -212(%edi),%edi
movl (%edi),%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
jl L59
L61:
pushl %ecx
pushl %edx
leal L9,%edi
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
L58:
movl $-204,%edi
movl %edi,%edi
addl %ebp,%edi
movl (%edi),%edi
movl %edi,(%ebx)
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
movl 8(%ebp),%edi
movl -208(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ebx
movl 8(%ebp),%edi
movl -208(%edi),%edi
movl (%edi),%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L63
L65:
pushl %ecx
pushl %edx
leal L9,%edi
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
L62:
movl $0,%edi
movl %edi,(%ebx)
movl 8(%ebp),%edi
movl -216(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -216(%edi),%edi
movl (%edi),%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
cmpl %ebx,%esi
jl L67
L69:
pushl %ecx
pushl %edx
leal L9,%edi
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
L66:
movl $0,%edi
	movl	-12(%ebp), %edx # load pseudo-register
movl %edi,(%edx)
movl 8(%ebp),%edi
movl -220(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -220(%edi),%edi
movl (%edi),%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
cmpl %ebx,%esi
jl L71
L73:
pushl %ecx
pushl %edx
leal L9,%edi
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
L70:
movl $0,%edi
	movl	-12(%ebp), %edx # load pseudo-register
movl %edi,(%edx)
L75:
movl -204(%ebp),%edi
	movl	-8(%ebp), %ecx # load pseudo-register
cmpl %ecx,%edi
jge L23
L78:
movl -204(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,-204(%ebp)
jmp L77
L25:
movl $0,%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L27
L26:
movl 8(%ebp),%edi
movl -208(%edi),%ebx
movl -204(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ebx
jmp L24
L34:
movl $1,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -216(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -16(%ebp) # save pseudo-register
movl 8(%ebp),%edi
movl -216(%edi),%edi
movl (%edi),%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
cmpl %ebx,%esi
jl L29
L31:
pushl %ecx
pushl %edx
leal L9,%edi
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
L28:
movl $0,%esi
	movl	-16(%ebp), %ecx # load pseudo-register
movl (%ecx),%edi
cmpl %esi,%edi
je L32
L33:
movl $0,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
L32:
	movl	-12(%ebp), %ecx # load pseudo-register
movl %ecx,%esi
jmp L36
L29:
movl $0,%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
cmpl %ebx,%esi
jl L31
L30:
movl 8(%ebp),%edi
movl -216(%edi),%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ecx
	movl	%ecx, -16(%ebp) # save pseudo-register
jmp L28
L38:
movl $0,%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
cmpl %ebx,%esi
jl L40
L39:
movl 8(%ebp),%edi
movl -220(%edi),%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ecx
	movl	%ecx, -16(%ebp) # save pseudo-register
jmp L37
L44:
movl $0,%edi
movl %edi,%esi
jmp L45
L47:
movl $0,%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L49
L48:
movl 8(%ebp),%edi
movl -208(%edi),%ebx
movl -204(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ebx
jmp L46
L51:
movl $0,%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
cmpl %ebx,%esi
jl L53
L52:
movl 8(%ebp),%edi
movl -216(%edi),%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
jmp L50
L55:
movl $0,%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
cmpl %ebx,%esi
jl L57
L56:
movl 8(%ebp),%edi
movl -220(%edi),%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
jmp L54
L59:
movl $0,%esi
movl 12(%ebp),%edi
cmpl %esi,%edi
jl L61
L60:
movl 8(%ebp),%edi
movl -212(%edi),%ebx
movl 12(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ebx
jmp L58
L63:
movl $0,%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L65
L64:
movl 8(%ebp),%edi
movl -208(%edi),%ebx
movl -204(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ebx
jmp L62
L67:
movl $0,%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
cmpl %ebx,%esi
jl L69
L68:
movl 8(%ebp),%edi
movl -216(%edi),%ebx
movl -204(%ebp),%esi
movl 12(%ebp),%edi
movl %esi,%esi
addl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
jmp L66
L71:
movl $0,%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
cmpl %ebx,%esi
jl L73
L72:
movl 8(%ebp),%edi
movl -220(%edi),%ebx
movl -204(%ebp),%esi
movl $7,%edi
movl %esi,%esi
addl %edi,%esi
movl 12(%ebp),%edi
movl %esi,%esi
subl %edi,%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
jmp L70
L84:
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
subl $208,%esp
pushl %ebx
pushl %edi
pushl %esi
L87:
movl $0,%edi
movl %edi,-204(%ebp)
movl 8(%ebp),%edi
movl -204(%edi),%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -8(%ebp) # save pseudo-register
movl -204(%ebp),%edi
	movl	-8(%ebp), %ecx # load pseudo-register
cmpl %ecx,%edi
jle L20
L3:
pushl %eax
pushl %ecx
pushl %edx
leal L22,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl %edi,%eax
jmp L86
L20:
movl $0,%edi
movl %edi,-208(%ebp)
movl 8(%ebp),%edi
movl -204(%edi),%esi
movl $1,%edi
movl %esi,%esi
subl %edi,%esi
movl %esi,%ecx
	movl	%ecx, -12(%ebp) # save pseudo-register
movl -208(%ebp),%edi
	movl	-12(%ebp), %ecx # load pseudo-register
cmpl %ecx,%edi
jle L16
L4:
pushl %eax
pushl %ecx
pushl %edx
leal L18,%edi
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
popl %eax
movl -204(%ebp),%edi
	movl	-8(%ebp), %ecx # load pseudo-register
cmpl %ecx,%edi
jge L3
L21:
movl -204(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,-204(%ebp)
jmp L20
L16:
movl 8(%ebp),%edi
movl -212(%edi),%esi
movl $4,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,%ebx
movl 8(%ebp),%edi
movl -212(%edi),%edi
movl (%edi),%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L6
L8:
pushl %ecx
pushl %edx
leal L9,%edi
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
L5:
movl -208(%ebp),%esi
movl (%ebx),%edi
cmpl %esi,%edi
je L12
L13:
leal L11,%edi
movl %edi,%edi
L14:
pushl %ecx
pushl %edx
pushl %edi
call print
addl $4,%esp
movl %eax,%edi
popl %edx
popl %ecx
movl -208(%ebp),%edi
	movl	-12(%ebp), %ecx # load pseudo-register
cmpl %ecx,%edi
jge L4
L17:
movl -208(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,-208(%ebp)
jmp L16
L6:
movl $0,%esi
movl -204(%ebp),%edi
cmpl %esi,%edi
jl L8
L7:
movl 8(%ebp),%edi
movl -212(%edi),%ebx
movl -204(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl $4,%edi
movl %esi,%esi
imull %edi,%esi
movl %ebx,%edi
addl %esi,%edi
movl %edi,%ebx
jmp L5
L12:
leal L10,%edi
movl %edi,%edi
jmp L14
L86:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
L22:
.4byte 1
.ascii "\n"
L18:
.4byte 1
.ascii "\n"
L11:
.4byte 2
.ascii " ."
L10:
.4byte 2
.ascii " O"
L9:
.4byte 30
.ascii "Array subscript out of bounds\n"
