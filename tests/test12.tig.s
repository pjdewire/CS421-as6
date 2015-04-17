.globl tigermain
.type tigermain, @function
tigermain:
pushl %ebp
movl %esp,%ebp
subl $208,%esp
pushl %ebx
pushl %edi
pushl %esi
L6:
movl $0,%edi
movl %edi,-204(%ebp)
movl $0,%edi
movl %edi,-208(%ebp)
movl $100,%edi
movl %edi,%ebx
movl -208(%ebp),%edi
cmpl %ebx,%edi
jle L3
L1:
movl $1,%edi
movl %edi,%eax
jmp L5
L3:
movl -204(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,-204(%ebp)
movl -208(%ebp),%edi
cmpl %ebx,%edi
jge L1
L4:
movl -208(%ebp),%esi
movl $1,%edi
movl %esi,%esi
addl %edi,%esi
movl %esi,-208(%ebp)
jmp L3
L5:
popl %esi
popl %edi
popl %ebx
movl %ebp,%esp
popl %ebp
ret
