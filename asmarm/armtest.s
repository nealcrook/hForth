;;; piece of code to exercise all ARM op-codes in order to compare
;;; ARM's ARM assembler with my Forth ARM assembler
;;;
;;; NAC 02-Feb-1997	Example of each format, but not every opcode.
;;;			for branch, data-processing, multiply and load/store

	AREA	test,CODE,READONLY
	
	b	%F01
	bcc	%F02
	beq	%F03
03	orr	r0, r0, r0	; NOP
01	orr	r0, r0, r0	; NOP
02	orr	r0, r0, r0	; NOP
	b	%B01
	b	%B01
	
	bl	%F01
	blcc	%F02
	bleq	%F03
03	orr	r0, r0, r0	; NOP
01	orr	r0, r0, r0	; NOP
02	orr	r0, r0, r0	; NOP
	bl	%B01
	bl	%B01

	swi	01
	swi	&340123
	swieq	01

;;; DP format 1
	mov	r0, #0
	add	r3, r5, #1
	cmp	r7, #1000
	bic	r9, r8, #&ff00
;;; DP format 2
	mov	r2, r1
	add	r4, r3, r2
	cmp	r7, r9
;;; DP format 3
	mov	r2, r0, lsl #2
	add	r9, r5, r5, lsl #3
	rsb	r9, r5, r5, lsl #3
	sub	r10, r9, r8, lsr #4
	mov	r12, r4, ror r3
;;; DP shifter operands
	mov	r2, #&3c0000
	mov	r2, #&3
	mov	r2, r6
	mov	r2, r7, lsl #5
	mov	r2, r7, lsl r9
	mov	r2, r9, asr #9
	mov	r2, r12, rrx

	addeq	r3, r3, #&3c0000
	addeqs	r3, r3, #&3
	add	r4, r3, r6
	adds	r7, r3, r7, lsl #5
	add	r8, r3, r7, lsl r9
	;; reserved instruction - (nv)
	;; addnvs	r9, r3, r9, asr #9
	addnes	r9, r3, r9, asr #9
	addmi	r10, r3, r12, rrx

;;; Multiply
	mul	r4, r2, r1
	muls	r4, r2, r1
	mla	r7, r8, r9, r3
	smull	r4, r8, r2, r3
	umull	r6, r8, r0, r1
	umlal	r5, r8, r0, r1
		
;;; Addressing modes for load/store
	ldr	r7, [r5, #5]
	ldreq	r7, [r5, #-5]
	ldrne	r7, [r5, r6]
	ldrcs	r7, [r5, -r6]
	ldrhs	r7, [r5, r6, lsr #4]
	ldrcc	r7, [r5, -r6, lsr #4]
	ldrlo	r7, [r5, r6, rrx]
	ldrmi	r7, [r5, -r6, rrx]
	ldrpl	r7, [r5, #5]!
	ldrvs	r7, [r5, #-5]!
	ldrvc	r7, [r5, r6]!
	ldrhi	r7, [r5, -r6]!
	ldrls	r7, [r5, r6, lsr #4]!
	ldrge	r7, [r5, -r6, lsr #4]!
	ldrlt	r7, [r5, r6, rrx]!
	ldrgt	r7, [r5, -r6, rrx]!
	ldrle	r7, [r5], #14
	ldral	r7, [r5], #-14
	;; reserved instruction (Rm=Rn with post-indexing)
	;; ldr	r7, [r5], r5, lsr #4
	;; ldr	r7, [r5], -r5, asr #4
	;; ldr	r7, [r5], r5, rrx
	ldr	r7, [r4], r5, lsr #4
	ldr	r7, [r3], -r5, asr #4
	ldr	r7, [r2], r5, rrx
	ldr	r7, [r2], r5, lsr #4
	ldr	r7, [r2], -r5, asr #4
	ldr	r7, [r2], r5, rrx
;;; example of each instruction, with a fixed addressing mode
	ldreq	r7, [r5, r6]
	strgt	r7, [r5, r6]
	ldrltb	r7, [r5, r6]
	strneb	r7, [r5, r6]
	;; translate not allowed in pre-indexed form
	;; ldreqt	r7, [r5, r6]
	;; strgtt	r7, [r5, r6]
	;; ldrltbt	r7, [r5, r6]
	;; strnebt	r7, [r5, r6]
	ldreqt	r7, [r5], r6
	strgtt	r7, [r5], r6
	ldrltbt	r7, [r5], r6
	strnebt	r7, [r5], r6

;;; only 6 of the addressing modes are legal for these..
	ldrh	r1, [r0]
	;; undefined effect - destination same as written-back base
	;; ldrh	r1, [r1], #5
	ldrh	r7, [r1], #5
	ldrh	r1, [r2], #-9
	ldrh	r1, [r11], r3
	ldrh	r1, [r4], -r5
	ldrh	r8, [r3, #2]
	ldrh	r12, [r13, #-6]
	ldrh	r8, [r3, #2]!
	ldrh	r12, [r13, #-6]!
	ldrh	r7, [r5, r6]
	ldrh	r7, [r5, -r6]
	ldrh	r7, [r5, r6]!
	ldrh	r7, [r5, -r6]!
	ldrh	r3, [r9], #2

	ldrsh	r1, [r0]
	ldrsh	r7, [r1], #5
	ldrsh	r1, [r2], #-9
	ldrsh	r1, [r11], r3
	ldrsh	r1, [r4], -r5
	ldrsh	r8, [r3, #2]
	ldrsh	r12, [r13, #-6]
	ldrh	r8, [r3, #2]!
	ldrsh	r12, [r13, #-6]!
	ldrsh	r7, [r5, r6]
	ldrsh	r7, [r5, -r6]
	ldrsh	r7, [r5, r6]!
	ldrsh	r7, [r5, -r6]!
	ldrsh	r3, [r9], #2

	ldrsb	r1, [r0]
	ldrsb	r7, [r1], #5
	ldrsb	r1, [r2], #-9
	ldrsb	r1, [r3], r9
	ldrsb	r1, [r4], -r5
	ldrsb	r8, [r3, #2]
	ldrsb	r12, [r13, #-6]
	ldrsb	r8, [r3, #2]!
	ldrsb	r12, [r13, #-6]!
	ldrsb	r7, [r5, r6]
	ldrsb	r7, [r5, -r6]
	ldrsb	r7, [r5, r6]!
	ldrsb	r7, [r5, -r6]!
	ldrsb	r3, [r9], #2

	strh	r2, [r0]
	strh	r7, [r1], #5
	strh	r1, [r2], #-9
	strh	r1, [r7], r3
	strh	r1, [r4], -r5
	strh	r2, [r5, #0x80]
	strh	r2, [r6, #-8]
	strh	r2, [r7, #0x80]!
	strh	r2, [r8, #-8]!
	strh	r7, [r5, r6]
	strh	r7, [r5, -r6]
	strh	r7, [r5, r6]!
	strh	r7, [r5, -r6]!
	strh	r10, [r7, -r4]

	END
	






