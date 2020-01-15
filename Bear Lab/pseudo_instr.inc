	
     turn_left: .DB east, north, south, west

	 
	turn_right:  .DB west, south, north, east

	;--------------------------
    ;--------Turn Left --------
	; Called from WhichWay subrouting
	; input and ouput is r24
	
	TurnLeft:
	push r16				  // push the value of r16 on stack

	ldi ZH, HIGH(turn_left<<1)	// load indirect address value Z and declare pointing to table turn_left
	ldi ZL, LOW(turn_left<<1)
	
	add ZL, r24					// adds the value of r24 into Z to point at register
	lpm r24, Z					// points at value r24 is set at to table

	.

	pop r16					  // bring back original r16 value
	ret						  

	;--------------------------
    ;--------Turn Right -------
	; Called from WhichWay subrouting
	; input and ouput is r24
	
	TurnRight:
	push r16				// push the value of r16 on stack
	
	
	ldi	ZH,  high(turn_right<<1) // load indirect address value Z and declare pointing to table turn_right
	ldi ZL,  low(turn_right<<1)
	
	add ZL, r24			    // adds the value of r24 into Z to point at register

	lpm r24, Z  			// points at value r24 is set at to table

	pop  r16				// bring back original r16 value
	ret

	;--------------------------
    ;--------Turn Around-------
	; Called from WhichWay subrouting
	; input and ouput is r24

	TurnAround:
	push r16			// push the value of r16 on stack

	com r24				// compliments current values in r24
	cbr r24, 0xFC		// clears MSB and brings back 2 LSB

	pop  r16			 // bring back original r16 value
	ret
	
	;-----------------------------
	;---------Hit Wall------------
	HitWall:
	push r16

	rcall DrawDirection // calls DrawDirection subroutine
	mov r16, r24		// value saved in r24 moved to r16
	mov r24, r22		// value saved in r22 moved to r24
	rcall DrawRoom		// redraws the room to save in r24
	

	and r24, r16		// and values r24 with r16. This allows a value of 0 or 1 to be brought back
						// to compare for testing which way the bear is facing

	pop r16				
	ret


	;------------------------------
	;---------Right paw------------
	;------------------------------
	;- right paw touch the wall?
	; Called from WhichWay subroutine
	; inputs: r24(dir), r22(room)
	; output: r24
	; If the answer is no return value is set to zero
	; Else the answer is yes
	; no registers are modified
	;-----------------------------
	RightPaw:
		rcall TurnRight		// calls turn right subroutine
		rcall HitWall		// calls hit wall subroutine
		ret 
    ;-----------------------------

	;-----------------------------
	;---------Left paw------------
	;-----------------------------
	;- left paw touch the wall?
	; Called from WhichWay subroutine
	; inputs: r24(dir), r22(room)
	; output: r24
	; If the answer is no return value is set to zero
	; Else the answer is yes
	; no registers are modified
	;-----------------------------
	LeftPaw:
		rcall TurnLeft		// calls turn left subroutine
		rcall HitWall		// calls hit wall subroutine
		ret 
