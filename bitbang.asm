;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bitbang.asm -- msp430f1611 bit-banging test
;;;
;;; exercises:
;;;	all registers
;;;	all instructions
;;;	all addressing modes
;;;
;;; does not exercise:
;;;	any peripherals, SFRs, etc.
;;;

	.text

.global main
	.type main, @function
main:
	mov  #0x5a80, 0x0120(r2); disable wdt
	mov  #0x3900, r1	; set up stack
	
	call #bitbang		; run tests
	
	bis  #0x10, r2		; halt simulator
9:	
	jmp  9b
.Lmain_end:
.size main, .Lmain_end-main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uint16_t bitbang(void) -- gcc abi
;;;
;;; returns:
;;;	   0 = all tests passed
;;;	else = a test failed
;;; 
zztext:	

.global bitbang
	.type bitbang, @function
bitbang:
	push r4			; save regs per gcc abi
	push r5
	push r6
	push r7
	push r8
	push r9
	push r10
	push r11

	call #bb1		; bang on r15 with word instrs
	tst  r15
	jnz  1f

	call #bb2		; bang on r15 with byte instrs
	tst  r15
	jnz  1f

	call #bb3		; bang on r14 and r15 word math
	tst  r15
	jnz  1f

	call #bb4		; bang on other regs
	tst  r15
	jnz  1f

	call #bb5		; various addressing modes
	tst  r15
	jnz  1f

	clr r15
	jmp 2f			; success

1:
	mov #-1, r15		; fail

2:	
	pop r11			; restore regs
	pop r10
	pop r9
	pop r8
	pop r7
	pop r6
	pop r5
	pop r4
	ret			; retval in r15
.Lbitbang_end:
.size bitbang, .Lbitbang_end-bitbang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test r15, mov, jne, jnc, jc, cmp, bic, rrc, rra, swpb, sxt, bis, bit, reti
	.type bb1, @function
bb1:	
	mov  #0xaaaa, r15
	cmp  #0xaaaa, r15
	jne  2f
	clrc
	jc   2f
	rrc  r15
	jc   2f
	cmp  #0x5555, r15
	jne  2f
	clrc
	rrc  r15
	jnc  2f
	cmp  #0x2aaa, r15
	jne  2f
	rra  r15
	cmp  #0x1555, r15
	jne  2f
	mov  #0x8000, r15
	cmp  #0x8000, r15
	jne  2f
	rra  r15
	jc   2f
	cmp  #0xc000, r15
	jne  2f
	mov  #0x4000, r15
	cmp  #0x4000, r15
	jne  2f
	rra  r15
	cmp  #0x2000, r15
	jne  2f
	swpb r15
	cmp  #0x0020, r15
	jne  2f
	sxt  r15
	cmp  #0x0020, r15
	jne  2f
	bis  #0x0080, r15
	cmp  #0x00a0, r15
	jne  2f
	sxt  r15
	cmp  #0xffa0, r15
	jne  2f
	bit  #0x8000, r15
	jz   2f
	bit  #0x0001, r15
	jnz  2f
	mov  r15, r14
	cmp  r15, r14
	jne  2f
	xor  r15, r14		; should set VZ, clr NC
	jn   2f
	jc   2f
	jge  2f
	jnz  2f
1:	
	clr r15
	jmp 3f
2:	
	mov #-1, r15
3:	
	push r2			; test reti
	reti
.Lbb1_end:	
.size bb1, .Lbb1_end-bb1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; same as bb1 with byte instructions
	.type bb2, @function
bb2:	
	mov.b  #0xaa, r15
	cmp    #0x00aa, r15
	jne    2f
	clrc
	jc     2f
	rrc.b  r15
	jc     2f
	cmp.b  #0x55, r15
	jne    2f
	clrc
	rrc.b  r15
	jnc    2f
	cmp    #0x2a, r15
	jne    2f
	rra.b  r15
	cmp    #0x15, r15
	jne    2f
	mov.b  #0x80, r15
	cmp    #0x80, r15
	jne    2f
	rra.b  r15
	jc     2f
	cmp    #0xc0, r15
	jne    2f
	mov    #0x40, r15
	cmp.b  #0x40, r15
	jne    2f
	rra.b  r15
	cmp    #0x20, r15
	jne    2f
	swpb   r15
	cmp    #0x2000, r15
	jne    2f
	swpb   r15
	cmp    #0x0020, r15
	jne    2f
	sxt    r15
	cmp    #0x0020, r15
	jne    2f
	bis.b  #0x80, r15
	cmp.b  #0xa0, r15
	jne    2f
	sxt    r15
	cmp    #0xffa0, r15
	jne    2f
	bit    #0x8000, r15
	jz     2f
	bit    #0x0001, r15
	jnz    2f
	mov.b  r15, r14
	cmp.b  r15, r14
	jne    2f
	xor.b  r15, r14		; should set VZ, clr NC
	jn     2f
	jc     2f
	jge    2f
	jnz    2f
1:	
	clr r15
	jmp 3f
2:	
	mov #-1, r15
3:	
	push r2			; test reti
	reti
.Lbb2_end:	
.size bb2, .Lbb2_end-bb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compare psw VNZC bit to r14 bits 3..0 (VNZC)
;;; Z set on success, clr on fail
	.type xpsw, @function
xpsw:
	push   r14		; save test value
	mov    r2, r14		; grab sr
	bic    #0xfef8, r14	; only V...NZC remain
	bit    #0x0100, r14	; V set?
	jz     1f		; nope
	bic    #0x0100, r14	; yup -- turn V off and set b3
	bis    #8, r14
1:
	cmp    @r1+,r14		; cmp and discard test value
	ret
.Lxpsw:
.size xpsw, .Lxpsw-xpsw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test r14 and r15, add, addc, sub, subc, dadd, xor, and
	.type bb3, @function
bb3:
	clr  r15
	
	mov  #4, r14		; -N--
	add  #-1, r15
	call #xpsw
	jne  2f
	cmp  #-1, r15
	jne  2f

	mov  #9, r14		; V--C
	add  #0x8000, r15
	call #xpsw
	jne  2f
	cmp  #0x7fff, r15
	jne  2f

	mov  #1, r14		; ---C
	add  #-1, r15
	call #xpsw
	jne  2f
	cmp  #0x7ffe, r15
	jne  2f

	mov  #12, r14		; VN--
	add  #2, r15
	call #xpsw
	jne  2f
	cmp  #0x8000, r15
	jne  2f

	mov  #11, r14		; V-ZC
	add  #0x8000, r15
	call #xpsw
	jne  2f
	cmp  #0x0000, r15
	jnz  2f

	mov  #4, r14		; -N--
	decd r15
	call #xpsw
	jne  2f
	cmp  #0xfffe, r15
	jne  2f

	mov  #5, r14		; -N-C
	clrc
	subc #1, r15
	call #xpsw
	jne  2f
	cmp  #0xfffc, r15
	jne  2f

	mov  #5, r14		; -N-C
	setc
	subc #0, r15
	call #xpsw
	jne  2f
	cmp  #0xfffc, r15
	jne  2f

	mov  #4, r14		; -N--
	setc
	addc #0, r15
	call #xpsw
	jne  2f
	cmp  #0xfffd, r15
	jne  2f

	mov  #9, r14		; V--C
	xor  #-1, r15
	call #xpsw
	jne  2f
	cmp  #2, r15
	jne  2f

	mov  #3, r14		; --ZC
	clrc
	subc #1, r15
	call #xpsw
	jne  2f
	cmp  #0, r15
	jne  2f

	mov  #2, r14		; --Z-
	and  r15, r15
	call #xpsw
	jne  2f
	cmp  #0, r15
	jne  2f

	mov  #4, r14		; -N--
	sub  #1, r15
	call #xpsw
	jne  2f
	cmp  #-1, r15
	jne  2f

	mov  #3, r14		; --ZC
	sub  #-1, r15
	call #xpsw
	jne  2f
	cmp  #0, r15
	jne  2f

	bis  #0x5555, r15
	cmp  #0x5555, r15
	jne  2f

	mov  #5, r14		; -N-C
	xor  #0xaaaa, r15
	call #xpsw
	jne  2f
	cmp  #-1, r15
	jne  2f

	mov  #9, r14		; V--C
	xor  #0x8333, r15
	call #xpsw
	jne  2f
	cmp  #0x7ccc, r15
	jne  2f

	mov  #1, r14		; ---C
	and  #0x4444, r15
	call #xpsw
	jne  2f
	cmp  #0x4444, r15
	jne  2f

	mov  #0, r14
	clrc
	dadd #1, r15
	bic  #0x0100, r2	; clear V (undefined after dadd)
	call #xpsw
	jne  2f
	cmp  #0x4445, r15
	jne  2f

	mov  #4, r14		; -N--
	dadd #0x5553, r15
	bic  #0x0100, r2	; clear V (undefined after dadd)
	call #xpsw
	jne  2f
	cmp  #0x9999, r15
	jne  2f

1:	
	clr r15
	jmp 3f
2:	
	mov #-1, r15
3:	
	ret
.Lbb3_end:
.size bb3, .Lbb3_end-bb3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test r4--r13
	.type bb4, @function
bb4:
	clr  r13
	cmp  #0, r13
	jne  2f
	inv  r13
	cmp  #-1, r13
	jne  2f

	clr  r12
	cmp  #0, r12
	jne  2f
	inv  r12
	cmp  #-1, r12
	jne  2f

	clr  r11
	cmp  #0, r11
	jne  2f
	inv  r11
	cmp  #-1, r11
	jne  2f

	clr  r10
	cmp  #0, r10
	jne  2f
	inv  r10
	cmp  #-1, r10
	jne  2f

	clr  r9
	cmp  #0, r9
	jne  2f
	inv  r9
	cmp  #-1, r9
	jne  2f

	clr  r8
	cmp  #0, r8
	jne  2f
	inv  r8
	cmp  #-1, r8
	jne  2f

	clr  r7
	cmp  #0, r7
	jne  2f
	inv  r7
	cmp  #-1, r7
	jne  2f

	clr  r6
	cmp  #0, r6
	jne  2f
	inv  r6
	cmp  #-1, r6
	jne  2f

	clr  r5
	cmp  #0, r5
	jne  2f
	inv  r5
	cmp  #-1, r5
	jne  2f

	clr  r4
	cmp  #0, r4
	jne  2f
	inv  r4
	cmp  #-1, r4
	jne  2f

	clr  r4
	clr  r5
	clr  r6
	clr  r7
	clr  r8
	clr  r9
	clr  r10
	clr  r11
	clr  r12
	clr  r13

	mov  r13, r15
	bis  r12, r15
	bis  r11, r15
	bis  r10, r15
	bis  r9, r15
	bis  r8, r15
	bis  r7, r15
	bis  r6, r15
	bis  r5, r15
	tst  r15
	jnz  2f

1:	
	clr r15
	jmp 3f
2:	
	mov #-1, r15
3:	
	ret
.Lbb4:
.size bb4, .Lbb4-bb4
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bb5 -- test addressing modes
	.type bb5, @function
bb5:
	mov #wtbl, r15
	mov wtbl, r14
	cmp @r15+, r14
	jne 2f

	mov #wtbl, r14
	
	cmp @r15+, 2(r14)
	jne 2f

	sub #4, r15
	cmp @r15, 0(r14)
	jne 2f

	push @r14
	cmp  @r1+, 0(r15)
	jne  2f

	cmp @r15+, 0(r14)
	jne 2f
	
	cmp 2(r14), 0(r15)
	jne 2f

	cmp @r14+, -2(r15)
	jne 2f

	cmp @r15, 0(r14)
	jne 2f

	cmp @r15+, 0(r14)
	jne 2f

	mov @r15+, r14
	xor @r15, r14
	cmp #0x5a5a, r14
	jne 2f

	and @r15+, r14
	cmp #0x5050, r14
	jne 2f

1:
	clr r14
	clr r15
	jmp 3f
2:	
	mov #-1, r15
3:	
	ret
.Lbb5:
.size bb5, .Lbb5-bb5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data table for bb5

.type wtbl, @object
wtbl:
	.short 0xaaaa
	.short 0xf0f0
	.short 0x0f0f
	.short 0x5555
.Lwtbl:
.size wtbl, .Lwtbl-wtbl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; end of test segment

.Lzztext:
.size zztext, .Lzztext-zztext

;;; EOF bitbang.asm

