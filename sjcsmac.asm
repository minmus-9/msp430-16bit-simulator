;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sjcsmac.asm -- skipjack ref impl
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORD-ALIGNED cs state struct:
;;;	skey[20] -- scheduled key       0..19
;;;	r[8]     -- whitening lfsr     20..27
;;;	a[8]     -- auth accumulator   28..35
;;;	h[8]     -- hashed key         36..43
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skipjack code/data
;;; 

	.text
sj_seg:				; start of sj segment
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sj code seg

sj_code_seg:
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sjcs_ksched -- sj/cs key scheduling (gcc abi) (1550 clk incl call)
;;;
;;;	in:	r15 -- ptr to cs struct
;;;		r14 -- ptr to key
;;;
;;;	clob:	r12
;;;		r13
;;;		r14
;;;

	.type sjcs_ksched_load_2, @function
sjcs_ksched_load_2:
	mov.b @r14+, r12	; low byte
	mov.b @r14+, r13	; high byte
	swpb  r13
	bis   r12, r13		; word
	mov   r13, 0(r15)	; add to key schedule
	mov   @r15+, 8(r15)
	ret			; done
.Lsjcs_ksched_load_2_end:	
.size sjcs_ksched_load_2, .Lsjcs_ksched_load_2_end-sjcs_ksched_load_2

.global sjcs_ksched
	.type sjcs_ksched, @function
sjcs_ksched:
	push r7			; save regs per abi
	push r8
	push r9
	push r10
	push r11
	
	call #sjcs_ksched_load_2 ; load possibly unaligned key
	call #sjcs_ksched_load_2
	call #sjcs_ksched_load_2
	call #sjcs_ksched_load_2
	call #sjcs_ksched_load_2
	
	sub  #10, r15		; restore r15

	clr  r7			; clear word regs
	clr  r8
	clr  r9
	clr  r10

	call #sj_enc_T		; crypt nulls
	call #sj_enc_TB		; second half
	sub  #8, r15		; restore r15

	swpb r7			; byte-swap it
	swpb r8
	swpb r9
	swpb r10

	mov  r7,  36(r15)	; save h[]
	mov  r8,  38(r15)
	mov  r9,  40(r15)
	mov  r10, 42(r15)

	pop  r11		; restore regs
	pop  r10
	pop  r9
	pop  r8
	pop  r7

	ret			; done
.Lsjcs_ksched_end:	
.size sjcs_ksched, .Lsjcs_ksched_end-sjcs_ksched

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sjcs_nsched -- nonce scheduling
;;;
;;;	in:	r15 -- ptr to cs struct
;;;		r14 -- ptr to nonce
;;;
;;;	clob:	r7
;;;		r8
;;;		r9
;;;		r10
;;;		r11
;;;		r12
;;;		r13
;;;		r14
;;;

	.type sjcs_nsched, @function
sjcs_nsched:
;;; load possibly unaligned nonce
	mov.b @r14+, r7		; w1 -> r7
	mov.b @r14+, r12
	swpb  r12
	bis   r12, r7

	mov.b @r14+, r8		; w2 -> r8
	mov.b @r14+, r12
	swpb  r12
	bis   r12, r8

	mov.b @r14+, r9		; w3 -> r9
	mov.b @r14+, r12
	swpb  r12
	bis   r12, r9

	mov.b @r14+, r10	; w4 -> r10
	mov.b @r14+, r12
	swpb  r12
	bis   r12, r10

;;; and process it
	mov  r15, r14		; xor with h[]
	add  #36, r14
	xor  @r14+, r7
	xor  @r14+, r8
	xor  @r14+, r9
	xor  @r14+, r10

	call #sj_enc_Ts		; swap and encrypt it
	call #sj_enc_TB		; bottom half
	sub  #8, r15		; restore r15

	swpb r7			; byte-swap it
	swpb r8
	swpb r9
	swpb r10

	mov  r15, r14		; xor with h[]
	add  #36, r14
	xor  @r14+, r7
	xor  @r14+, r8
	xor  @r14+, r9
	xor  @r14+, r10

	mov  r7,  20(r15)	; save r[]
	mov  r8,  22(r15)
	mov  r9,  24(r15)
	mov  r10, 26(r15)

	clr  28(r15)		; clear a[]
	clr  30(r15)
	clr  32(r15)
	clr  34(r15)
	
	ret			; done
.Lsjcs_nsched_end:	
.size sjcs_nsched, .Lsjcs_nsched_end-sjcs_nsched

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sjcs_mac -- calc mac (1443 clks incl call)
;;;
;;;	in:	r15 -- pointer to cs state
;;;		r14 -- pointer to mac buffer
;;;
;;;	clob:	r7
;;;		r8
;;;		r9
;;;		r10
;;;		r11
;;;		r12
;;;		r13
;;;

	.type sjcs_mac, @function
sjcs_mac:
	push r14		; save mac ptr
	
	mov  r15, r14
	add  #20, r14		; load r[]
	mov  @r14+, r7
	mov  @r14+, r8
	mov  @r14+, r9
	mov  @r14+, r10

	xor  @r14+, r7		; xor with a[]
	xor  @r14+, r8
	xor  @r14+, r9
	xor  @r14+, r10
	
	call #sj_enc_Ts		; swap and encrypt it
	call #sj_enc_TB		; bottom half

	swpb r7			; byte-swap it
	swpb r8
	swpb r9
	swpb r10
	
	sub  #8, r15		; restore r15

	mov  r15, r14
	add  #28, r14		; xor with a[]
	xor  @r14+, r7
	xor  @r14+, r8
	xor  @r14+, r9
	xor  @r14+, r10

	pop  r14		; store possibly unaligned mac
	
	mov.b r7, 0(r14)
	swpb  r7
	mov.b r7, 1(r14)
	mov.b r8, 2(r14)
	swpb  r8
	mov.b r8, 3(r14)
	mov.b r9, 4(r14)
	swpb  r9
	mov.b r9, 5(r14)
	mov.b r10, 6(r14)
	swpb  r10
	mov.b r10, 7(r14)

	ret			; done
.Lsjcs_mac_end:	
.size sjcs_mac, .Lsjcs_mac_end-sjcs_mac

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sjcs_add -- process an auth-only block (789 clk incl. call)
;;;
;;;	in:	r15 -- pointer to cs state
;;;		r7  -- w1 (unswapped)
;;;		r8  -- w2
;;;		r9  -- w3
;;;		r10 -- w4
;;;
;;;	clob:	r7
;;;		r8
;;;		r9
;;;		r10
;;;		r11
;;;		r12
;;;		r13
;;;		r14
;;;

	.type sjcs_add, @function
sjcs_add:
	add  #20, r15		; r15 points to r[]
	mov  r15, r14		; xor plaintext with r
	xor  @r14+, r7
	xor  @r14+, r8
	xor  @r14+, r9
	xor  @r14+, r10

;;; r -> x * r in GF(2 ** 64)
	clrc			; NB takes 27 or 32 clocks
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	jnc   .Lsjcs_add_r_mul_out
	xor.b #0xd8, -8(r15)
.Lsjcs_add_r_mul_out:		; r[] is done; r15 points to a[]

;;; a -> x * a in GF(2 ** 64)
	clrc			; NB takes 27 or 32 clocks
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	rrc.b @r15+
	jnc   .Lsjcs_add_a_mul_out
	xor.b #0xd8, -8(r15)
.Lsjcs_add_a_mul_out:		; a[] is done; r15 points to h[]
	
	sub  #36, r15		; r15 points to skey

	call #sj_enc_Ts		; swap and half-crypt it
	sub  #4, r15		; restore r15

	swpb r7			; byte-swap it
	swpb r8
	swpb r9
	swpb r10

	xor  r7,  28(r15)	; xor text into a[]
	xor  r8,  30(r15)
	xor  r9,  32(r15)
	xor  r10, 34(r15)

	ret			; done
.Lsjcs_add_end:	
.size sjcs_add, .Lsjcs_add_end-sjcs_add

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sjcs_process -- eat data
;;;
;;;	in:	r15 -- ptr to cs struct
;;;		r6  -- ptr to data
;;;		r5  -- byte count
;;;
;;;	clob:	r5
;;;		r6
;;;		r7
;;;		r8
;;;		r9
;;;		r10
;;;		r11
;;;		r12
;;;		r13
;;;		r14
;;;

	.type sjcs_process, @function
sjcs_process:
	mov   r15, r14		; salt in-process mac with data len
	add   #28, r14		; get a[]
	mov   @r14, r13		; get a[0]
	and   #7, r13		; a[0] & 7
	add   r14, r13		; &a[a[0] & 7]
	xor.b r5, 0(r13)	; xor it in (carefully)
	swpb  r5
	xor.b r5, 1(r13)
	swpb  r5

;;; process all 8-byte blocks
.Lsjcs_process_blk:
	cmp  #8, r5		; 8 or more?
	jn   .Lsjcs_process_7	; do last if not

;;; load possibly unaligned data
	mov.b @r6+, r7		; w1 -> r7
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r7

	mov.b @r6+, r8		; w2 -> r8
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r8

	mov.b @r6+, r9		; w3 -> r9
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r9

	mov.b @r6+, r10		; w4 -> r10
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r10

	sub  #8, r5		; adjust count

	call #sjcs_add		; add to mac

	jmp  .Lsjcs_process_blk	; repeat

;;; process last 0-7 bytes
.Lsjcs_process_7:		; final (short) block
	
	bit  #4, r5
	jz   .Lsjcs_process_30
	
;;; process last 4-7 bytes
	
	bit  #2, r5
	jz   .Lsjcs_process_54
	
;;; process last 6-7 bytes
	
	mov.b @r6+, r7		; w1 -> r7
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r7

	mov.b @r6+, r8		; w2 -> r8
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r8

	mov.b @r6+, r9		; w3 -> r9
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r9

	bit   #1, r5
	jz    .Lsjcs_process_c10	; 6 bytes
	mov.b @r6+, r10		; 7 bytes
	jmp   .Lsjcs_process_fin

;;; process last 4-5 bytes
	
.Lsjcs_process_54:
	
	mov.b @r6+, r7		; w1 -> r7
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r7

	mov.b @r6+, r8		; w2 -> r8
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r8

	bit   #1, r5
	jz    .Lsjcs_process_c9	; 4 bytes
	mov.b @r6+, r9		; 5 bytes
	jmp   .Lsjcs_process_c10

;;; process last 0-3 bytes

.Lsjcs_process_30:
	
	bit #2, r5
	jz  .Lsjcs_process_10

;;; process last 2-3 bytes

	mov.b @r6+, r7		; w1 -> r7
	mov.b @r6+, r12
	swpb  r12
	bis   r12, r7

	bit   #1, r5
	jz    .Lsjcs_process_c8	; 2 bytes
	mov.b @r6+, r8		; 3 bytes
	jmp   .Lsjcs_process_c9

;;; process last 0-1 bytes

.Lsjcs_process_10:
	bit   #1, r5
	jz    .Lsjcs_process_done
	mov.b @r6+, r7

;;; do the final processing

.Lsjcs_process_c8:	
	clr  r8
.Lsjcs_process_c9:
	clr  r9
.Lsjcs_process_c10:
	clr  r10
	
.Lsjcs_process_fin:
	call #sjcs_add

.Lsjcs_process_done:
	ret			; done
.Lsjcs_process_end:	
.size sjcs_process, .Lsjcs_process_end-sjcs_process

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sjcs_auth -- skipjack auth-only cs mode (gcc abi)
;;;
;;;	in:	r15    -- ptr to cs struct (key already scheduled)
;;;		r14    -- ptr to nonce
;;;		r13    -- ptr to aad
;;;		r12    -- aad length
;;; 		16(r1) -- ptr to ptd (pushed last)
;;;		18(r1) -- ptd len
;;;		20(r1) -- ptr to mac (pushed first)
;;;
;;;	clob:	r12
;;;		r13
;;;		r14
;;; 

.global sjcs_auth
	.type sjcs_auth, @function
	.p2align 1, 0
sjcs_auth:
	push r5			; save regs per abi
	push r6
	push r7
	push r8
	push r9
	push r10
	push r11

	push r12		; save aadlen and aad
	push r13

	call #sjcs_nsched	; process nonce

;;; process aad
	pop  r6			; ptr
	pop  r5			; count
	call #sjcs_process	; add to mac

;;; process ptd
	mov  16(r1), r6		; ptr
	mov  18(r1), r5		; count
	call #sjcs_process	; add to mac

;;; compute mac
	mov  20(r1), r14	; load ptr
	call #sjcs_mac		; finish mac

	pop  r11		; restore regs
	pop  r10
	pop  r9
	pop  r8
	pop  r7
	pop  r6
	pop  r5

	ret			; done
.Lsjcs_auth_end:	
.size sjcs_auth, .Lsjcs_auth_end-sjcs_auth

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convenient top-half of skipjack
;;;
;;;	in:	r15 -- pointer to scheduled key
;;;		r7  -- w1 (unswapped)
;;;		r8  -- w2
;;;		r9  -- w3
;;;		r10 -- w4
;;;
;;;	out:	r15 -- modified (+4)
;;;		r7  -- w1'
;;;		r8  -- w2'
;;;		r9  -- w3'
;;;		r10 -- w4'
;;;
;;;	clob:	r11
;;;		r12
;;;		r13
;;;		r14
;;;
;;; NB calling sj_enc_TB immediately after sj_enc_Ts completes will
;;;    yield a fully encrypted *swapped* result in r7--r10
;;;

	.type sj_enc_Ts, @function
sj_enc_Ts:
	swpb r7
	swpb r8
	swpb r9
	swpb r10
;;; FALL THROUGH to sj_enc_T

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple top-half of skipjack
;;;
;;;	in:	r15 -- pointer to scheduled key
;;;		r7  -- w1 (swapped)
;;;		r8  -- w2
;;;		r9  -- w3
;;;		r10 -- w4
;;;
;;;	out:	r15 -- modified (+4)
;;;		r7  -- w1'
;;;		r8  -- w2'
;;;		r9  -- w3'
;;;		r10 -- w4'
;;;
;;;	clob:	r11
;;;		r12
;;;		r13
;;;		r14
;;;
;;; NB calling sj_enc_TB immediately after sj_enc_T completes will
;;;    yield a fully encrypted *swapped* result in r7--r10
;;; 

	.type sj_enc_T, @function
sj_enc_T:
	clr r13			; clear counter
	mov #sj_enc_jtab, r11	; load jump table ptr
	
;;; FALL THROUGH TO sj_enc_TB...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; top and bottom half of skipjack encryption
;;;
;;;	in:	r15 -- pointer to scheduled key
;;;		r13 -- round counter
;;;		r11 -- jump table pointer
;;;		r7  -- w1
;;;		r8  -- w2
;;;		r9  -- w3
;;;		r10 -- w4
;;;
;;;	out:	r15 -- modified
;;;		r13 -- modified
;;;		r11 -- modified
;;;		r7  -- w1'
;;;		r8  -- w2'
;;;		r9  -- w3'
;;;		r10 -- w4'
;;;
;;;	clob:	r12
;;;		r14
;;; 
	
	.type sj_enc_TB, @function
sj_enc_TB:	
;;; ctr = 1, 17
	inc   r13		; bump ctr
	mov   r7, r12		; w1
	jmp   sj_G
.Lsj_enc_R1:			; sj_G  jumps back to each label by using
.Lsj_enc_R17:			; "mov @r11+,r0". this saves 3 clocks over
				; call/ret (2+3 vs 5+3).
	
	xor   r12, r10		; update w4
	xor   r13, r10
	mov   r12, r7		; w1

;;; ctr = 2, 18
	inc   r13		; bump ctr
	mov   r10, r12		; w4
	jmp   sj_G
.Lsj_enc_R2:
.Lsj_enc_R18:	
	xor   r12, r9		; update w3
	xor   r13, r9
	mov   r12, r10		; w4

;;; ctr = 3, 19
	inc   r13		; bump ctr
	mov   r9, r12		; w3
	jmp   sj_G
.Lsj_enc_R3:
.Lsj_enc_R19:
	xor   r12, r8		; update w2
	xor   r13, r8
	mov   r12, r9		; w3

;;; ctr = 4, *20
	inc   r13		; bump ctr
	mov   r8, r12		; w2
	jmp   sj_G
.Lsj_enc_R20:	
	sub   #20, r15		; reset skey
.Lsj_enc_R4:
	xor   r12, r7		; update w1
	xor   r13, r7
	mov   r12, r8		; w2

;;; ctr = *5, 21
	inc   r13		; bump ctr
	mov   r7, r12		; w1
	jmp   sj_G
.Lsj_enc_R5:	
	sub   #20, r15		; reset skey
.Lsj_enc_R21:
	xor   r12, r10		; update w4
	xor   r13, r10
	mov   r12, r7		; w1

;;; ctr = 6, 22
	inc   r13		; bump ctr
	mov   r10, r12		; w4
	jmp   sj_G
.Lsj_enc_R6:
.Lsj_enc_R22:
	xor   r12, r9		; update w3
	xor   r13, r9
	mov   r12, r10		; w4

;;; ctr = 7, 23
	inc   r13		; bump ctr
	mov   r9, r12		; w3
	jmp   sj_G
.Lsj_enc_R7:
.Lsj_enc_R23:
	xor   r12, r8		; update w2
	xor   r13, r8
	mov   r12, r9		; w3

;;; ctr = 8, 24
	inc   r13		; bump ctr
	mov   r8, r12		; w2
	jmp   sj_G
.Lsj_enc_R8:
.Lsj_enc_R24:
	xor   r12, r7		; update w1
	xor   r13, r7
	mov   r12, r8		; w2

;;; ctr = 9, *25
	inc   r13		; bump ctr
	mov   r7, r12		; w1
	xor   r12, r8		; update w2
	xor   r13, r8
	jmp   sj_G
.Lsj_enc_R25:	
	sub   #20, r15		; reset skey
.Lsj_enc_R9:	
	mov   r12, r7		; w1

;;; ctr = *10, 26
	inc   r13		; bump ctr
	mov   r10, r12		; w4
	xor   r12, r7		; update w1
	xor   r13, r7
	jmp   sj_G
.Lsj_enc_R10:	
	sub   #20, r15		; reset skey
.Lsj_enc_R26:	
	mov   r12, r10		; w4

;;; ctr = 11, 27
	inc   r13		; bump ctr
	mov   r9, r12		; w3
	xor   r12, r10		; update w4
	xor   r13, r10
	jmp   sj_G
.Lsj_enc_R11:
.Lsj_enc_R27:	
	mov   r12, r9		; w3

;;; ctr = 12, 28
	inc   r13		; bump ctr
	mov   r8, r12		; w2
	xor   r12, r9		; update w3
	xor   r13, r9
	jmp   sj_G
.Lsj_enc_R12:
.Lsj_enc_R28:	
	mov   r12, r8		; w2

;;; ctr = 13, 29
	inc   r13		; bump ctr
	mov   r7, r12		; w1
	xor   r12, r8		; update w2
	xor   r13, r8
	jmp   sj_G
.Lsj_enc_R13:
.Lsj_enc_R29:	
	mov   r12, r7		; w1

;;; ctr = 14, *30
	inc   r13		; bump ctr
	mov   r10, r12		; w4
	xor   r12, r7		; update w1
	xor   r13, r7
	jmp   sj_G
.Lsj_enc_R30:	
	sub   #20, r15		; reset skey
.Lsj_enc_R14:	
	mov   r12, r10		; w4

;;; ctr = *15, 31
	inc   r13		; bump ctr
	mov   r9, r12		; w3
	xor   r12, r10		; update w4
	xor   r13, r10
	jmp   sj_G
.Lsj_enc_R15:	
	sub   #20, r15		; reset skey
.Lsj_enc_R31:	
	mov   r12, r9		; w3

;;; ctr = 16, 32
	inc   r13		; bump ctr
	mov   r8, r12		; w2
	xor   r12, r9		; update w3
	xor   r13, r9
	jmp   sj_G
.Lsj_enc_R16:
.Lsj_enc_R32:	
	mov   r12, r8		; w2

	ret			; done
.Lsj_enc_TB_end:	
.size sj_enc_TB, .Lsj_enc_TB_end-sj_enc_TB
.size sj_enc_T, .Lsj_enc_TB_end-sj_enc_T
.size sj_enc_Ts, .Lsj_enc_TB_end-sj_enc_Ts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skipjack G function
;;;
;;;	in:	r15 -- pointer to scheduled key
;;;		r13 -- round counter
;;;		r12 -- word to pass through G()
;;;		r11  -- return jump table pointer
;;;
;;;	out:	r15 -- modified
;;;		r13 -- modified
;;;		r12 -- new word
;;;		r11 -- modified
;;;
;;;	clob:	r14
;;; 

	.type sj_G, @function
	.p2align 1, 0
sj_G:	
	mov.b @r15+, r14	; r14 is scratch *skey++
	xor.b r12, r14		; w1l ^ *skey++
	mov.b ftab(r14),r14	; F[w1l ^ *skey++]
	swpb  r12		; have w1l w1h
	xor   r14, r12		; have w1l w1h'

	mov.b @r15+, r14	; *skey++
	xor.b r12, r14		; w1h' ^ *skey++
	mov.b ftab(r14),r14	; F[w1h' ^ *skey++]
	swpb  r12		; w1h' w1l
	xor   r14, r12		; w1h' w1l'

;;; do the same thing again...
	mov.b @r15+, r14	; r14 is scratch *skey++
	xor.b r12, r14		; w1l ^ *skey++
	mov.b ftab(r14),r14	; F[w1l ^ *skey++]
	swpb  r12		; have w1l w1h
	xor   r14, r12		; have w1l w1h'

	mov.b @r15+, r14	; *skey++
	xor.b r12, r14		; w1h' ^ *skey++
	mov.b ftab(r14),r14	; F[w1h' ^ *skey++]
	swpb  r12		; w1h' w1l
	xor   r14, r12		; w1h' w1l'

	mov   @r11+, r0		; return
.Lsj_G_end:	
.size sj_G, .Lsj_G_end-sj_G

.Lsj_code_seg_end:	
.size sj_code_seg, .Lsj_code_seg_end-sj_code_seg	; end of code segment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skipjack data
;;; 
	
sj_data_seg:			; start of data segment

;;; skipjack F[] table

	.type	ftab,@object
	.size	ftab,256
ftab:	
	.byte	-93
	.byte	-41
	.byte	9
	.byte	-125
	.byte	-8
	.byte	72
	.byte	-10
	.byte	-12
	.byte	-77
	.byte	33
	.byte	21
	.byte	120
	.byte	-103
	.byte	-79
	.byte	-81
	.byte	-7
	.byte	-25
	.byte	45
	.byte	77
	.byte	-118
	.byte	-50
	.byte	76
	.byte	-54
	.byte	46
	.byte	82
	.byte	-107
	.byte	-39
	.byte	30
	.byte	78
	.byte	56
	.byte	68
	.byte	40
	.byte	10
	.byte	-33
	.byte	2
	.byte	-96
	.byte	23
	.byte	-15
	.byte	96
	.byte	104
	.byte	18
	.byte	-73
	.byte	122
	.byte	-61
	.byte	-23
	.byte	-6
	.byte	61
	.byte	83
	.byte	-106
	.byte	-124
	.byte	107
	.byte	-70
	.byte	-14
	.byte	99
	.byte	-102
	.byte	25
	.byte	124
	.byte	-82
	.byte	-27
	.byte	-11
	.byte	-9
	.byte	22
	.byte	106
	.byte	-94
	.byte	57
	.byte	-74
	.byte	123
	.byte	15
	.byte	-63
	.byte	-109
	.byte	-127
	.byte	27
	.byte	-18
	.byte	-76
	.byte	26
	.byte	-22
	.byte	-48
	.byte	-111
	.byte	47
	.byte	-72
	.byte	85
	.byte	-71
	.byte	-38
	.byte	-123
	.byte	63
	.byte	65
	.byte	-65
	.byte	-32
	.byte	90
	.byte	88
	.byte	-128
	.byte	95
	.byte	102
	.byte	11
	.byte	-40
	.byte	-112
	.byte	53
	.byte	-43
	.byte	-64
	.byte	-89
	.byte	51
	.byte	6
	.byte	101
	.byte	105
	.byte	69
	.byte	0
	.byte	-108
	.byte	86
	.byte	109
	.byte	-104
	.byte	-101
	.byte	118
	.byte	-105
	.byte	-4
	.byte	-78
	.byte	-62
	.byte	-80
	.byte	-2
	.byte	-37
	.byte	32
	.byte	-31
	.byte	-21
	.byte	-42
	.byte	-28
	.byte	-35
	.byte	71
	.byte	74
	.byte	29
	.byte	66
	.byte	-19
	.byte	-98
	.byte	110
	.byte	73
	.byte	60
	.byte	-51
	.byte	67
	.byte	39
	.byte	-46
	.byte	7
	.byte	-44
	.byte	-34
	.byte	-57
	.byte	103
	.byte	24
	.byte	-119
	.byte	-53
	.byte	48
	.byte	31
	.byte	-115
	.byte	-58
	.byte	-113
	.byte	-86
	.byte	-56
	.byte	116
	.byte	-36
	.byte	-55
	.byte	93
	.byte	92
	.byte	49
	.byte	-92
	.byte	112
	.byte	-120
	.byte	97
	.byte	44
	.byte	-97
	.byte	13
	.byte	43
	.byte	-121
	.byte	80
	.byte	-126
	.byte	84
	.byte	100
	.byte	38
	.byte	125
	.byte	3
	.byte	64
	.byte	52
	.byte	75
	.byte	28
	.byte	115
	.byte	-47
	.byte	-60
	.byte	-3
	.byte	59
	.byte	-52
	.byte	-5
	.byte	127
	.byte	-85
	.byte	-26
	.byte	62
	.byte	91
	.byte	-91
	.byte	-83
	.byte	4
	.byte	35
	.byte	-100
	.byte	20
	.byte	81
	.byte	34
	.byte	-16
	.byte	41
	.byte	121
	.byte	113
	.byte	126
	.byte	-1
	.byte	-116
	.byte	14
	.byte	-30
	.byte	12
	.byte	-17
	.byte	-68
	.byte	114
	.byte	117
	.byte	111
	.byte	55
	.byte	-95
	.byte	-20
	.byte	-45
	.byte	-114
	.byte	98
	.byte	-117
	.byte	-122
	.byte	16
	.byte	-24
	.byte	8
	.byte	119
	.byte	17
	.byte	-66
	.byte	-110
	.byte	79
	.byte	36
	.byte	-59
	.byte	50
	.byte	54
	.byte	-99
	.byte	-49
	.byte	-13
	.byte	-90
	.byte	-69
	.byte	-84
	.byte	94
	.byte	108
	.byte	-87
	.byte	19
	.byte	87
	.byte	37
	.byte	-75
	.byte	-29
	.byte	-67
	.byte	-88
	.byte	58
	.byte	1
	.byte	5
	.byte	89
	.byte	42
	.byte	70
;;; end of F[]
	
;;; sj_enc_TB jump-return table used by sj_G
;;; 

	.type sj_enc_jtab, @object
	.p2align 1, 0
	.size sj_enc_jtab, 64
sj_enc_jtab:
	.short .Lsj_enc_R1
	.short .Lsj_enc_R2
	.short .Lsj_enc_R3
	.short .Lsj_enc_R4
	.short .Lsj_enc_R5
	.short .Lsj_enc_R6
	.short .Lsj_enc_R7
	.short .Lsj_enc_R8
	.short .Lsj_enc_R9
	.short .Lsj_enc_R10
	.short .Lsj_enc_R11
	.short .Lsj_enc_R12
	.short .Lsj_enc_R13
	.short .Lsj_enc_R14
	.short .Lsj_enc_R15
	.short .Lsj_enc_R16
	.short .Lsj_enc_R17
	.short .Lsj_enc_R18
	.short .Lsj_enc_R19
	.short .Lsj_enc_R20
	.short .Lsj_enc_R21
	.short .Lsj_enc_R22
	.short .Lsj_enc_R23
	.short .Lsj_enc_R24
	.short .Lsj_enc_R25
	.short .Lsj_enc_R26
	.short .Lsj_enc_R27
	.short .Lsj_enc_R28
	.short .Lsj_enc_R29
	.short .Lsj_enc_R30
	.short .Lsj_enc_R31
	.short .Lsj_enc_R32
	.p2align 1, 0
;;; end of sj_enc_jtab

.Lsj_data_seg_end:		
.size sj_data_seg, .Lsj_data_seg_end-sj_data_seg	; end of data segment

.Lsj_seg_end:	
.size sj_seg, .Lsj_seg_end-sj_seg			; end of skipjack stuff

;;; 
;;; EOF sjcsmac.asm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

