; ----------------------------------------
; Lab 5 -  Take a Step, Enter Room, and a Little Animation
; Version  2.1
; Date: August 15 2018
; Written By : Christopher W. Fingers
; Lab Hours  : Monday/Wednesday 3:00pm - 6:45pm
; ----------------------------------------

	.INCLUDE <m328pdef.inc>

	.DEF var_A = r16
	.EQU S0 = 0b00			// state 0
	.EQU S1 = 0b01			// state 1
	.EQU S2 = 0b10			// state 2
	.EQU S3 = 0b11			// state 3
	; True and False

	.EQU true = 0xFF
	.EQU false = 0x00

	;push button ports

	.EQU dff_Q = PD2		//Q output for flip flop
	.EQU dff_clk = PD5		//clock debounce for flip fop

	.DSEG
	 room:  .BYTE   1       
	 dir:   .BYTE   1
	 next_state:  .BYTE 1	// FSM next state
	 walk:  .BYTE   1
	 row:   .BYTE 1
	 col:   .BYTE 1
	 bees:  .BYTE 1	

	.CSEG
	.ORG 0x0000

	RST_VECT:
    rjmp reset                 // jump over IVT, plus INCLUDE code

	.ORG INT0addr
	jmp INT0_ISR

	.ORG 0x0100                // bypass IVT
	.INCLUDE "spi_shield.inc"
	.INCLUDE "testbench.inc"   // DrawRoom and DrawDirection
	.INCLUDE "pseudo_instr.inc"
	.INCLUDE "maze.inc"

reset:
	ldi   r16,low(RAMEND)     // RAMEND address 0x08ff
	out   SPL,r16             // stack Pointer Low SPL at i/o address 0x3d
	ldi   r16,high(RAMEND)
	out   SPH,r16             // stack Pointer High SPH at i/o address 0x3

	call  InitShield           // initialize GPIO Ports and SPI communications
	

	; Initialize External Interrupt
	cbi EIMSK, INT0
	lds r17, EIMSK
	cbr r17, 0b00000001
	sbr r17, 0b00000010
	sts EICRA, r17
	sbi EIMSK, INT0
	// loads the 250ms delay into timer 1
	ldi r16, 0x0B
    sts TCNT1H, r16
    ldi r16, 0xDC
    sts TCNT1L, r16
    ldi r17, (1<<CS11) | (1<<CS10)
    sts TCCR1B, r17   

	clr   spiLEDS              // clear discrete LEDs
	clr   spi7SEG              // clear 7-segment display

;Initialize SRM Variables	
	clr   r16                  // initalizes dir, room, walk and next state with register r16
	sts   dir, r16
	sts   room, r16
	sts   walk, r16			   // do not walk
	sts   next_state, r16
	sts   col, r16
	sts   bees, r16
	ldi   r16, 0x14
	sts   row, r16
	ldi   r16, 0x03            // loads the hex number 3 into r16 and then stores the 3 into variable dir

	;Initiaize pins for push-button and clock debounce
	sbi DDRD, dff_clk	// clock fip flop
	cbi DDRD, dff_Q		// Q flip flop
	cbi PORTD, dff_Q	// Q flip flop
	sei					// global interrupt enabled
loop:

	call    ReadSwitches       // read switches into r7
    // dir  = switch & 0x03;
	mov     r16, switch        // move switch (r7) to temporary register r16
    cbr     r16, 0xFC          // mask-out most significant 6 bits
    sts     dir, r16           // save formatted value to SRAM variable dir.

	// state = next_state;
	lds r19, next_state

	// state s0
	State_S0:
		cpi		r19, S0
		brne	state_S1
		lds     r24, room          // calling argument room is placed in r24.
		rcall   DrawRoom		   // translate room to 7 segment bits
		mov     spi7SEG, r24       // return value, the room, is saved to 7 segment display register
    	ldi r19, S1
		sts next_state, r19
		rjmp end_state
	
	// state s1
	State_S1:
		cpi     r19, S1				// compares immediate value of s1 with reg r19
		brne    State_S2			// if value in r19 is not similar to S1 jumps to S2
		lds     r24, room           // calling argument dir is placed in r24.
		rcall   DrawRoom
		mov 	spi7SEG, r24
		lds		r24, dir
		rcall   DrawDirection      // translate direction to 7 segment bit
		or 		spi7SEG, r24
		
		lds r17, walk
		tst r17
		ldi r19, S0
		breq end_S1
		ldi r19, S2
		end_S1:

		sts next_state, r19
		rjmp end_state			//break
		
    //state S2, takes a step and updates the new room accordingly
	State_S2: 
		cpi r19, S2			//Similar to S1, compares r19 with S2 if value is not the same
		brne State_S3		// Immediately jumps to end of state
		ldi r17, false		
		sts walk, r17
		lds r24, room		// load the room, dir, row and col values to side registers
		lds r20, dir
		lds r24, row
		lds r22, col
		rcall TakeAStep		// call take a step
		sts col, r22		// save the new col/row values 
		sts row, r24
		rcall EnterRoom		// call enter room
		mov r22, r24		// mov the new r24 value to r22
		andi r22, 0xF0		// save the bee value into register r22
		swap r22
		sts bees, r22		// save the bee value from r22 to bees
		andi r24, 0x0F		// Find the row value by into r24
		sts room, r24		// save the r24 value into room
		mov spi7SEG, r24	// set 7seg
		lds r24, room
		rcall DrawRoom			
		mov spi7SEG, r24

		ldi r19, S3			// loads state 3 value into r19
		sts next_state, r19
		cpi r24, true		// if the room value is equivalent to true the state 3
		breq end_S2			// is loaded

		ldi r19, S1
		end_S2:

		sts next_state, r19
		rjmp end_state

		State_S3:
		cpi r19, S3			// compares if r19 is set to S3, if not break to end
		brne end_state
		ldi r19, S2			// loads state 2 into r19
		sts next_state, r19
		cpi r24, true		// checks to see if room is a hallway if so go to state 2
		breq end_state
		ldi r19, S0			// if room is not a hallway go to state 0
		sts next_state, r19		

		end_state:
	
	// blinks led when leaving a state
		bst r19, 1
   		bld spiLEDS, 1
    	bst r19, 0
   	    bld spiLEDS, 0

	lds r22, bees			// loads the values 
	lds r24, room
	rcall IsHallway
	rcall TestIsHallWay
	rcall  Delay
	rcall  Pulse
	
    call   WriteDisplay

	rjmp   loop

    /* Creates the delay interrupt that sets the global*/
    /* timer interrupt to clear and reset the timer*/
    
    Delay:
    push r16
    wait:
    sbis TIFR1, TOV1		//If overflow bit is not set then timer will wait
    rjmp wait				// until the timer overflow bit is set
    sbi TIFR1, TOV1    
    ldi r16, 0x0B			// loads a 250ms delay
    sts TCNT1H, r16
    ldi r16, 0XDC
    sts TCNT1L, r16
    pop r16
    ret

    /* -----PULSE SUBROUTINE-----*/
    ;  Pulse is called from the ISR
    ;  Inputs: none
    ;  A positive pulse cycle applied to
    ;  the input of the D flip-flop
    ;------------------------------

Pulse:
    cbi PORTD, dff_clk
    sbi PORTD, dff_clk
    ret  

	TakeAStep:
	bst r20, 0	//load bit 0 into the t sreg bit and check if bit is set
	brts N_E	// if set branch to a north east test
	bst r20, 1	// load bit 1 into t sreg bit and check if bit is set
	brts W		// if set branch to west direction
	inc r24		// inc r24 for moving south
	rjmp return	// jump to return 
	W:			// west direction
	dec r22		// decrement col variable
	rjmp return	// jump to return
	N_E:		// north east test
	bst r20, 1	// check bit 1 of direction
	brts N		// break to north if set
	inc r22		// inc col varible if east
	rjmp return	// jump to return
	N:			// north branch
	dec r24		// dec row variable
	return:		// return
	ret

	EnterRoom:
	push r16				// push used values
	push r2
	push ZH
	push ZL
	ldi ZL, low(theMaze<<1)	// load themaze table to z pointer
	ldi ZH, high(theMaze<<1)
	clr r2					
	ldi r16, 0x14			// load our variable to multiply
	mul r24, r16			// multiply row by 20
	adc r1, r2				// set carry to r1
	add r24, r22			// add col to new row value

	add ZL, r0				// add the values to the z pointer
	adc ZH, r1

	lpm r24, Z				// use indirect addressing for r24
	pop ZL					// pop out
	pop ZH
	pop r2
	pop r16
	ret
	

		// timer interrupt table that clears timer if tov1 is set
	INT0_ISR:
	push reg_F
	in reg_F,SREG
	push r16
	ldi r16, true
	sts walk,r16
	pop r16
	out SREG,reg_F
	pop reg_F
	reti

	;--------------------------
	;---Is this a Hallway?---
; input r24 = current room (room), r22 = bees
; output r24 = 0 if answer is false (r24 == 0), 
; otherwise theanswer is true (r24 != 0).
; The answer is true if the bear is in a hallway 
; without any bees.
; no registers are modified by this subroutine
;------------------------
IsHallway:
// return (bees == 0x00 && (room == 0x09 || room == 0x06));
tst r22						  // the room value of r22 is tested 
brne answer_is_no
cpi r22, 0x09
breq answer_is_yes
cpi r22, 0x06
breq answer_is_yes
answer_is_no:
ldi   r24, false               // room is not a hallway or contains bees
rjmp  endOfHallway
answer_is_yes:
ldi   r24, true
endOfHallway:
ret

; --------------------------
; -----Test IsHallway -----
; Called from LED display section in the main loop
; Input: r24  Outputs: spiLEDs bits 5 and 4 only
TestIsHallWay:
push r16
mov r16, spiLEDS
tst r24             // test return value from isHallway
brne inHallway
sbr r16, 0b00010000 // Bear is not in hallway, so turn on LED 4
cbr r16, 0b00100000
rjmp doneHallway
inHallway:
sbr r16, 0b00100000 //Bear is in hallway, so turn on LED 5
cbr r16, 0b00010000
doneHallway:
mov spiLEDS, r16
pop r16
ret
