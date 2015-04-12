.csect 	.text[PR]
	.extern .printf
	.extern .g
sss:	.byte "hello "
	.byte 10, 0
.toc
sss_disp:
	.tc sss[TC],sss
.csect 	.text[PR]
	.align 2
	.globl .f
.f:
	mflr 0
	st 0,8(1)
	ai 9,1,-64
	st 1,0(9)
	mr 1,9
	l 3,sss_disp(2)
	bl .printf
	nop
	bl .g
	nop
	mr 0,3
	ai 9,0,3
	mr 3,9
	l 1,0(1)
	l 0,8(1)
	mtlr 0
	br
