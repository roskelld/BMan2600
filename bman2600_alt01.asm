    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Start unitialized segment at $80 for variables
;; Range is $80 to $FF minus a few bytes at the end if the stack is used
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u variables
    org $80
P0POSX                  byte        ; player position X
P0POSY                  byte        ; player position Y
P0SPRPTR                word        ; Pointer to P0 sprite lookup table
P0COLPTR                word        ; Pointer to P0 color lookup table
P0GRABUF		byte		; Buffer for P0 graphics for one scanline *ALTERATION
P0COLBUF		byte		; Buffer for P0 color for one scanline *ALTERATION
P0ANMSET                byte        ; P0 sprite animation frame offset
ANIMCOUNTER             byte        ; Current animation update countdown
MOVECOUNTER             byte        ; Counter to track updating movement
ANIM_FRAME              byte        ; Tracks if we're on first or second animation frame

ARENAINDEX              byte        ; Draw index of Arena
ARENA_SWITCH            byte        ; Toggle tracker
ARENACOUNTER            byte        ; Tracks the update for the arena
; ARENAPTRPF1             word
; ARENAPTRPF2             word
ARENABUFFER		ds.b	22	; Buffer for arena layout data *ALTERATION
STACKPTRBUFFER		byte		; Keep a copy of the stack pointer when we clobber it *ALTERATION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Define Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0_HGT                  = $11       ; player 0 sprite height (# rows in lookup table)
MOVE_RATE               = 160       ; Speed of player movement (255 == 100%)
ANIM_RATE               = 20        ; Speed of player movement (255 == 100%)
SPRITE_OFFSET_IDLE      = $0         ; Offset position of facing idle sprite
SPRITE_OFFSET_DOWN      = $11       ; Offset position of facing down sprite
SPRITE_OFFSET_UP        = $22       ; Offset position of facing up sprite
SPRITE_OFFSET_RIGHT     = $33       ; Offset position of facing right sprite
SPRITE_FRAME_OFFSET     = $44       ; Second frame of animation offset
UP_BOUNDS               = $98       ; Top Player Boundary
DOWN_BOUNDS             = $00       ; Bottom Player Boundary
LEFT_BOUNDS             = $0A       ; Left Player Boundary
RIGHT_BOUNDS            = $6c       ; Right Player Boundary
VERTICAL_STEP           = $1
HORIZONTAL_STEP         = $1
ARENA_HEIGHT            = 87        ; (0-87)*2=176 scanlines for arena (2LK)
ARENA_BG                = $C3

X_LANE_START            = $1E
X_LANE_WALK_UP          = $7
X_LANE_BLOCKED          = $7
X_LANE_WALK_DOWN        = $F

Y_LANE_START            = $0A
Y_LANE_UPDATE           = $10
Y_LANE_WALK_RIGHT       = $5
Y_LANE_BLOCKED          = $5
Y_LANE_WALK_LEFT        = $5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Start ROM segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000

RESET:
    CLEAN_START                 ; Macro to safely clear the memory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Init Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #%00000000
    sta ANIM_FRAME              ; Set first animation frame to 0
    lda #$0b
    sta P0POSX                  ; Set Player 0 X
    lda #$97
    sta P0POSY                  ; Set Player 0 Y

    lda #15
    sta ARENACOUNTER            ; Set the Arena counter
    lda #0
    sta ARENAINDEX              ; Offset of arena map data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Init Pointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #<IdleSprite
    sta P0SPRPTR                ; lo-byte pointer for P0 sprite lookup table
    lda #>IdleSprite
    sta P0SPRPTR+1              ; hi-byte pointer for P0 sprite lookup table

    lda #<ColorFrame0
    sta P0COLPTR                ; lo-byte pointer for P0 color lookup table
    lda #>ColorFrame0
    sta P0COLPTR+1              ; hi-byte pointer for P0 color lookup table


; *ALTERATION below
; Fill the arena buffer in RAM from ROM
	ldx #21			; 2
	ldy #10			; 2
FILL_ARENA_BUFFER:
	lda ARENA_0_PF1,y	; 4(5?)
	sta ARENABUFFER,x	; 4
	dex			; 2
	lda ARENA_0_PF2,y	; 4(5?)
	sta ARENABUFFER,x	; 4
	dex			; 2
	dey			; 2
	bpl FILL_ARENA_BUFFER	; 3/2
; *ALTERATION ends


    ; lda #<ARENA_0_PF1
    ; sta ARENAPTRPF1
    ; lda #>ARENA_0_PF1
    ; sta ARENAPTRPF1+1

    ; lda #<ARENA_0_PF2
    ; sta ARENAPTRPF2
    ; lda #>ARENA_0_PF2
    ; sta ARENAPTRPF2+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Start a new frame loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
STARTFRAME:
    VERTICAL_SYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; VBLANK - 37 SCANLINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    TIMER_SETUP 20
                                ; Cycles    Total   - Comment
ANIMUPDATE:
    lda ANIMCOUNTER             ;                   -
    clc                         ;                   -
    adc #ANIM_RATE              ;                   -
    sta ANIMCOUNTER             ;                   -
    bcc .SKIPANIM               ;                   -
    lda ANIM_FRAME              ;                   -
    EOR #SPRITE_FRAME_OFFSET    ;                   -
    sta ANIM_FRAME              ;                   -
.SKIPANIM                       ;                   -
MOVEUPDATE:                     ;                   -
    lda MOVECOUNTER             ;                   -
    clc                         ;                   -
    adc #MOVE_RATE              ;                   -
    sta MOVECOUNTER             ;                   -
    bcc .SKIPMOVE               ;                   -
    jsr ProcessJoystick         ;                   -
.SKIPMOVE                       ;                   -
    lda P0POSX                  ;                   -
    ldy #0                      ;                   - Set Y to tell SETXPOS P0 sprite 
    jsr SETXPOS                 ;                   - Set P0 horizontal position

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; END VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    TIMER_WAIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 192 SCANLINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; clean up ;; find a better place to do this
    lda #0
	sta P0GRABUF		; *ALTERATION
	sta P0COLBUF		; *ALTERATION
    sta ARENAINDEX
                                ; Cycles    Total   - Comment
    ldx #25                     ;  2                - Score scanlines
    lda #$07                    ;  2                - color
    sta COLUBK                  ;  3                - background
    sta COLUPF                  ;  3                - playfield
    lda #%00000001              ;                   - Reflect on
    sta CTRLPF                  ;                   - Set playfield reflection to true
SCORE_PANEL:
    sta WSYNC                   ;  3                - wait for scanline
    dex                         ;  2                - X--
    bne SCORE_PANEL             ;  2                - repeat until score panel is drawn
GAME_SCREEN_SETUP:
    lda #ARENA_BG               ;                   - background color  (61st scanline)
    sta COLUBK                  ;                   - set background color
    lda #$09                    ;                   - playfield color
    sta COLUPF                  ;                   - set playfield color


; *ALTERATION below
	tsx			; 2 Save a copy of the stack pointer before we clobber it
	stx STACKPTRBUFFER	; 3
	ldx #<ARENABUFFER-1	; 2 Set the stack pointer up to read out arena layout bytes
	txs			; 2

    ldx #165                    ;                   - playfield scanlines
    lda #1
    sta ARENACOUNTER            ;                   - We pull new arena data every 15 lines
    lda #%11110000              ; 2                 - Load PF0 slice (Always the same, so outside of loop)
    sta PF0                     ; 3                 - set PF0
; *ALTERATION ends

; New scanline loop *ALTERATION
	sta WSYNC		; 3
.SLINE_LOOP
; (total 0 cycles)

	lda P0GRABUF		; 3 Send Player 0 data to TIA
	sta GRP0		; 3
	lda P0COLBUF		; 3
	sta COLUP0		; 3
	dex			; 2 Decrement scanline counter and start working on data for the next scanline
; (14 cycles, total 14 cycles)

	txa			; 2 Buffer Player 0 data for next scanline
	sec			; 2 TODO Necessary?
	sbc P0POSY		; 3
	cmp #P0_HGT		; 2
	bcs .NO_P0		; 2/3
; (11 cycles, total 25 cycles)

	adc P0ANMSET		; 3 NOTE: The carry flag is always set here, so rather than spend 2 cycles clearing it, just subtract 1 from P0ANMSET when calculating it.
	tay			; 2
	lda (P0SPRPTR),y	; 5
	sta P0GRABUF		; 3
	lda (P0COLPTR),y	; 5
	sta P0COLBUF		; 3
	bcc .AFTER_P0		; 3 Replace with a JMP instruction if you're nervous about the carry flag being clear after the ADC, or page breaks (+1 byte)
;	jmp .AFTER_P0		; 3
; (24 cycles, total 49 cycles)

.NO_P0
; (total 26 cycles)
	lda #0			; 2 Store blank pattern data to the Player 0 graphics buffer
	sta P0GRABUF		; 3

	ldy #3			; 2 Waste (5 * Y) + 1 cycles for timing
.BUSYWAIT_01
	dey			; 2
	bne .BUSYWAIT_01	; 3/2

	nop			; 2 Waste 2 more cycles for timing
; (23 cycles, total 49 cycles)

.AFTER_P0
; (total 49 cycles)

; Make the arena sections 16 scanlines high. This tiny change saves 1 byte of RAM, as well as 1 cycle every loop on the test against ARENACOUNTER, plus another 5 cycles in every 15 scanlines by not needing to reset it.
	txa			; 2 Test if this scanline is a multiple of 16
	and #$0f		; 2
	bne .NO_PF		; 2/3
; (6 cycles, total 55 cycles)

	nop			; 2 Waste 2 cycles for timing, so we don't write to PF1/2 too early

	pla			; 4 Pull the arena data off the stack and send it to TIA
	sta PF2			; 3
	pla			; 4
	sta PF1			; 3
; (16 cycles, total 71 cycles)

	cpx #0			; 2
	bne .SLINE_LOOP		; 3
; (total 76 cycles)

.NO_PF
; (total 56 cycles)
	nop			; 2 Waste 15 cycles for timing
	nop			; 2
	nop			; 2
	nop			; 2
	nop			; 2
	nop			; 2
	cmp $80			; 3

	cpx #0			; 2
	bne .SLINE_LOOP		; 3
; (total 76 cycles)

	ldx STACKPTRBUFFER	; 3 Restore the stack pointer
	txs			; 2
; *ALTERATION ends

.BOTTOM                         ;                   - [BOTTOM PANEL] Start of Bottom Panel
    sta WSYNC                   ;-----3 ----78------- wait for scanline
    ldx #18                      ;     2             - Bottom panel scanlines
    lda #$07                    ;     2             - panel color
    sta COLUBK                  ;     3             - background
    sta COLUPF                  ;     3             - playfield set both the same to create border
BOTTOM_PANEL:
    sta WSYNC                   ;                   - wait for scanline
    dex                         ;                   - X--
    bne BOTTOM_PANEL            ;                   - repeat until screen is drawn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OVERSCAN - 30 SCANLINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OVERSCAN:
	TIMER_SETUP 30
    TIMER_WAIT

    jmp NEXTFRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement is single direction (up, down, left OR right) If no direction is 
;; detected then the character gets reset to idle. This works by each direction 
;; branching to the next if not true, then right (last direction) falling 
;; through to a reset branch.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessJoystick Subroutine
    ldx P0POSY
                                ; Cycles    Total   - Comment
IPT_P0_UP:
    lda #%00010000              ;      2        2   - UP
    bit SWCHA                   ;      4        6   - 
    bne IPT_P0_DN               ;    2/3      8/9   - If not by pass UP logic
    lda #%00000000              ;      2       10   - Reset sprite flip
    sta REFP0                   ;      3       13   - Set register
    lda #SPRITE_OFFSET_UP       ;                   - Up Animation Frame base address
    clc                         ;                   - Clear the carry flag
    adc ANIM_FRAME              ;                   - Add current anim frame offset
    sta P0ANMSET                ;                   - Store frame to P0 Animation

    ldy P0POSX
    lda #Y_LANE_START           ;                   - Sets A to the start of the map
.UP_NEXT_LANE                   ;                   -
    adc #Y_LANE_UPDATE          ;                   - Lane loop value
.UP_LOOP                        ;                   -
    clc                         ;                   -
    cmp P0POSX                  ;                   -
    BMI .UP_NEXT_LANE           ;                   - Y is above A Lane so check Lane above
    sbc #Y_LANE_WALK_RIGHT      ;                   - Walk up zone             
    cmp P0POSX                  ;                   - 
    BMI .UP_WALK_RIGHT          ;                   - Branch to walk up if Y is <
    sbc #Y_LANE_BLOCKED         ;                   - Subtract to Blocked walk value
    cmp P0POSX                  ;                   - 
    BMI .UP_DONT_MOVE           ;                   - Branch to blocked walk if Y is <
    sbc #Y_LANE_WALK_LEFT       ;                   - Subtract to Walk down value
    cmp P0POSX                  ;                   - 
    BEQ .UP_WALK                ;                   - Branch to walk UP if Y = 
.UP_WALK_LEFT                   ;                   - 
    dey                         ;                   - 
    sty P0POSX                  ;                   - 
    jmp .UP_RETURN              ;                   - 
.UP_WALK_RIGHT                  ;                   -
    iny                         ;                   - Move down 1
    sty P0POSX                  ;                   - 
    jmp .UP_RETURN              ;                   - 
.UP_DONT_MOVE                   ;                   - 
    jmp .UP_RETURN              ;                   - 
.UP_WALK                        ;                   - 
    inx                         ;                   - Move UP
.UP_EXIT                        ;                   - 
    cpx #UP_BOUNDS              ;                   - Test new Y position against top of level
    beq .UP_RETURN              ;                   - Skip storing update if out of bounds
    inx                         ;                   - Move Up again (Aim to move up 2x in one update)
.UP_EXIT_2
    cpx #UP_BOUNDS              ;                   - Test new Y position against top of level
    beq .UP_RETURN              ;                   - Skip storing update if out of bounds
    stx P0POSY                  ;                   - Store new X position
.UP_RETURN                      ;                   - Don't add any more direction (We only support 4 direction movement)
    rts    
IPT_P0_DN:
    lda #%00100000              ;                   - DOWN
    bit SWCHA                   ;      4        6   - 
    bne IPT_P0_LT               ;                   - If not by pass DOWN logic
    lda #SPRITE_OFFSET_DOWN     ;                   - Down Animation Frame base address
    clc                         ;                   - Clear the carry flag
    adc ANIM_FRAME              ;                   - Add current anim frame offset
    sta P0ANMSET                ;                   - Store frame to P0 Animation


    ldy P0POSX
    lda #Y_LANE_START           ;                   - Sets A to the start of the map
.DN_NEXT_LANE                   ;                   -
    adc #Y_LANE_UPDATE          ;                   - Lane loop value
.DN_LOOP                        ;                   -
    clc                         ;                   -
    cmp P0POSX                  ;                   -
    BMI .DN_NEXT_LANE           ;                   - Y is above A Lane so check Lane above
    sbc #Y_LANE_WALK_RIGHT      ;                   - Walk up zone             
    cmp P0POSX                  ;                   - 
    BMI .DN_WALK_RIGHT          ;                   - Branch to walk up if Y is <
    sbc #Y_LANE_BLOCKED         ;                   - Subtract to Blocked walk value
    cmp P0POSX                  ;                   - 
    BMI .DN_DONT_MOVE           ;                   - Branch to blocked walk if Y is <
    sbc #Y_LANE_WALK_LEFT       ;                   - Subtract to Walk down value
    cmp P0POSX                  ;                   - 
    BEQ .DN_WALK                ;                   - Branch to walk DN if Y = 
.DN_WALK_LEFT                   ;                   - 
    dey                         ;                   - 
    sty P0POSX                  ;                   - 
    jmp .DN_RETURN              ;                   - 
.DN_WALK_RIGHT                  ;                   -
    iny                         ;                   - Move down 1
    sty P0POSX                  ;                   - 
    jmp .DN_RETURN              ;                   - 
.DN_DONT_MOVE                   ;                   - 
    jmp .DN_RETURN              ;                   - 
.DN_WALK                        ;                   - 
    dex                         ;                   - Move DN
    jmp .DN_EXIT                ;                   - 
.DN_EXIT                        ;                   - 
    cpx #DOWN_BOUNDS            ;                   - Test new X position against DN of level
    beq .DN_RETURN              ;                   - Skip storing update if out of bounds
    dex
.DN_EXIT_2
    cpx #DOWN_BOUNDS            ;                   - Test new X position against DN of level
    beq .DN_RETURN              ;                   - Skip storing update if out of bounds    
    stx P0POSY                  ;                   - Store new X position
.DN_RETURN                      ;                   - Don't add any more direction (We only support 4 direction movement)
    rts    
IPT_P0_LT:
    ldx P0POSX                  ;                   - Load Position X to X Register
    lda #%01000000              ;                   - LEFT
    bit SWCHA                   ;      4        6   - 
    bne IPT_P0_RT               ;                   - If not by pass LEFT logic
    lda #%00001000              ;                   - Flip sprite left
    sta REFP0                   ;                   - Set register
    lda #SPRITE_OFFSET_RIGHT    ;                   - Right Animation Frame base address
    clc                         ;                   - Clear the carry flag
    adc ANIM_FRAME              ;                   - Add current anim frame offset
    sta P0ANMSET                ;                   - Store frame to P0 Animation

    ;; Lane Check Loop
    ldy P0POSY
    lda #0                      ;                   - Clears A so it can add lane loop value
.LEFT_NEXT_LANE                 ;                   -
    adc #X_LANE_START           ;                   - Lane loop value
.LEFT_LOOP                      ;                   -
    clc                         ;                   -
    cmp P0POSY                  ;                   -
    BMI .LEFT_NEXT_LANE         ;                   - Y is above A Lane so check Lane above
    sbc #X_LANE_WALK_UP         ;                   - Walk up zone             
    cmp P0POSY                  ;                   - 
    BMI .LEFT_WALK_UP           ;                   - Branch to walk up if Y is <
    sbc #X_LANE_BLOCKED         ;                   - Subtract to Blocked walk value
    cmp P0POSY                  ;                   - 
    BMI .LEFT_DONT_MOVE         ;                   - Branch to blocked walk if Y is <
    sbc #X_LANE_WALK_DOWN       ;                   - Subtract to Walk down value
    cmp P0POSY                  ;                   - 
    BEQ .LEFT_WALK              ;                   - Branch to walk left if Y = 
.LEFT_WALK_DOWN                 ;                   -
    dey                         ;                   - Move down 1
    sty P0POSY                  ;                   - 
    jmp .LEFT_RETURN            ;                   - 
.LEFT_WALK_UP                   ;                   - 
    iny                         ;                   - 
    sty P0POSY                  ;                   - 
    jmp .LEFT_RETURN            ;                   - 
.LEFT_DONT_MOVE                 ;                   - 
    jmp .LEFT_RETURN            ;                   - 
.LEFT_WALK                      ;                   - 
    dex                         ;                   - Move Left
    jmp .LEFT_EXIT              ;                   - 
.LEFT_EXIT                      ;                   - 
    cpx #LEFT_BOUNDS            ;                   - Test new X position against left of level
    beq .LEFT_RETURN            ;                   - Skip storing update if out of bounds
    stx P0POSX                  ;                   - Store new X position
.LEFT_RETURN                    ;                   - Don't add any more direction (We only support 4 direction movement)
    rts                         ;                   -
IPT_P0_RT:
    lda #%10000000              ;                   - RIGHT
    bit SWCHA                   ;      4        6   - 
    bne INP_P0_RST              ;                   - If not by pass RIGHT logic
    lda #%00000000              ;                   - Reset sprite flip
    sta REFP0                   ;                   - Set register
    lda #SPRITE_OFFSET_RIGHT    ;                   - Right Animation Frame base address
    clc                         ;                   - Clear the carry flag
    adc ANIM_FRAME              ;                   - Add current anim frame offset
    sta P0ANMSET                ;                   - Store frame to P0 Animation

    ;; Lane Check Loop
    ldy P0POSY
    lda #0                      ;                   - Clears A so it can add lane loop value
.RIGHT_NEXT_LANE                ;                   -
    adc #X_LANE_START           ;                   - Lane loop value
.RIGHT_LOOP                     ;                   -
    clc                         ;                   -
    cmp P0POSY                  ;                   -
    BMI .RIGHT_NEXT_LANE        ;                   - Y is above A Lane so check Lane above
    sbc #X_LANE_WALK_UP         ;                   - Walk up zone             
    cmp P0POSY                  ;                   - 
    BMI .RIGHT_WALK_UP          ;                   - Branch to walk up if Y is <
    sbc #X_LANE_BLOCKED         ;                   - Subtract to Blocked walk value
    cmp P0POSY                  ;                   - 
    BMI .RIGHT_DONT_MOVE        ;                   - Branch to blocked walk if Y is <
    sbc #X_LANE_WALK_DOWN       ;                   - Subtract to Walk down value
    cmp P0POSY                  ;                   - 
    BEQ .RIGHT_WALK             ;                   - Branch to walk RIGHT if Y = 
.RIGHT_WALK_DOWN                ;                   -
    dey                         ;                   - Move down 1
    sty P0POSY                  ;                   - 
    jmp .RIGHT_RETURN           ;                   - 
.RIGHT_WALK_UP                  ;                   - 
    iny                         ;                   - 
    sty P0POSY                  ;                   - 
    jmp .RIGHT_RETURN           ;                   - 
.RIGHT_DONT_MOVE                ;                   - 
    jmp .RIGHT_RETURN           ;                   - 
.RIGHT_WALK                     ;                   - 
    inx                         ;                   - Move RIGHT
    jmp .RIGHT_EXIT             ;                   - 
.RIGHT_EXIT                     ;                   - 
    cpx #RIGHT_BOUNDS           ;                   - Test new X position against RIGHT of level
    beq .RIGHT_RETURN           ;                   - Skip storing update if out of bounds
    stx P0POSX                  ;                   - Store new X position
.RIGHT_RETURN                   ;                   - Don't add any more direction (We only support 4 direction movement)
    rts                         ;                   -
INP_P0_RST:                     ;                   - Reset animation cause there was no input
    lda #SPRITE_OFFSET_IDLE     ;                   - Idle Animation Frame base address
    clc                         ;                   - Clear the carry flag
    adc ANIM_FRAME              ;                   - Add current anim frame offset
    sta P0ANMSET                ;                   - Store frame to P0 Animation
IPT_P0_ND Subroutine            ;                   - End animation
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NEXTFRAME:
    jmp STARTFRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CKCOLP0PF subroutine
    lda #%10000000              ; CXP0FB bit 7 detects P0 and PF collision
    bit CXP0FB                  ; Check CXP0FB with above pattern
    bne .COLP0PF                ; if Collision P0 PF happened
    jmp ENDCKCOL                ; else, skip
.COLP0PF
    rts                         ; Go back to caller and they remove direction

ENDCKCOL:                       ; Fallback
    sta CXCLR                   ; Reset Collision
    jmp STARTFRAME              ; Didn't collide go to next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is target X-coord position
;; Y is object (0: P0, 1: P1, 2: MISSILE0, 3: MISSILE1, 4: BALL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SETXPOS Subroutine
    lda P0POSX                  ; load register A with P0 position X
    sta WSYNC                   ; Start fresh scanline
    sta HMCLR                   ; clear old horizontal position values
    sec                         ; set carry flag before subtraction
.DIVIDE_LOOP:   
    sbc #15                     ; A -= 15
    bcs .DIVIDE_LOOP            ; Loop while carry flag is still set

    eor #7                      ; adjust remainder in A between -8 and 7
    asl                         ; shift left by 4 as HMP0 only uses 4 bits
    asl
    asl
    asl
    sta HMP0,Y                  ; set the fine position
    sta RESP0,Y                 ; reset the 15-step rough position
    sta WSYNC                   ;
    sta HMOVE                   ; Apply fine position
    rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sprites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IdleSprite:
        .byte #%00000000;$0E
        .byte #%00101000;$58
        .byte #%00101000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%10000010;$42
        .byte #%10111010;$02
        .byte #%11111110;$9A
        .byte #%01111100;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01000100;$0E
        .byte #%01000100;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%00001100;$58
        .byte #%00001100;$58
DownSprite:
        .byte #%00000000;$0E
        .byte #%00100000;$58
        .byte #%00101000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%00000010;$42
        .byte #%10111010;$02
        .byte #%11111110;$9A
        .byte #%01111100;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01001100;$0E
        .byte #%01001100;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%00001100;$58
        .byte #%00001100;$58
UpSprite:
        .byte #%00000000;$0E
        .byte #%00100000;$58
        .byte #%00101000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%00000010;$42
        .byte #%10111010;$02
        .byte #%11111110;$9A
        .byte #%01111100;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01111100;$0E
        .byte #%01111100;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%01100000;$58
        .byte #%01100000;$58
RightSprite:
        .byte #%00000000;$0E
        .byte #%01001100;$58
        .byte #%01101000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%01000100;$42
        .byte #%01111000;$02
        .byte #%01111000;$9A
        .byte #%00111000;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01100000;$0E
        .byte #%01100000;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%00110000;$58
        .byte #%00110000;$58
IdleSprite1:
        .byte #%00000000;$0E
        .byte #%00101000;$58
        .byte #%00101000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%01000100;$42
        .byte #%10111010;$02
        .byte #%11111110;$9A
        .byte #%01111100;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01000100;$0E
        .byte #%01000100;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%00001100;$58
        .byte #%00001100;$58
DownSprite1:
        .byte #%00000000;$0E
        .byte #%00001000;$58
        .byte #%00101000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%10000000;$42
        .byte #%10111010;$02
        .byte #%11111110;$9A
        .byte #%01111100;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01100100;$0E
        .byte #%01100100;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%01100000;$58
        .byte #%01100000;$58
UpSprite1:
        .byte #%00000000;$0E
        .byte #%00001000;$58
        .byte #%00101000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%10000000;$42
        .byte #%10111010;$02
        .byte #%11111110;$9A
        .byte #%01111100;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01111100;$0E
        .byte #%01111100;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%00001100;$58
        .byte #%00001100;$58
RightSprite1:
        .byte #%00000000;$0E
        .byte #%00011000;$58
        .byte #%00110000;$58
        .byte #%00111000;$0E
        .byte #%00111000;$9A
        .byte #%00110000;$42
        .byte #%00111000;$02
        .byte #%00111000;$9A
        .byte #%00111000;$9A
        .byte #%00111000;$0E
        .byte #%01111100;$58
        .byte #%01110000;$0E
        .byte #%01110000;$0E
        .byte #%01111100;$58
        .byte #%00111000;$0E
        .byte #%01100000;$58
        .byte #%01100000;$58  
;---End Graphics Data---

;---Color Data from PlayerPal 2600---
ColorFrame0
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;
ColorFrame1
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;
ColorFrame2
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;
ColorFrame3
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;
ColorFrame4
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;
ColorFrame5
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;
ColorFrame6
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;
ColorFrame7
        .byte #$0E;
        .byte #$58;
        .byte #$58;
        .byte #$0E;
        .byte #$9A;
        .byte #$42;
        .byte #$02;
        .byte #$9A;
        .byte #$9A;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$0E;
        .byte #$58;
        .byte #$0E;
        .byte #$58;
        .byte #$58;     
;---End Color Data---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arena
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ARENA_0_PF0:
    .byte %11110000
    .byte %00010000
    .byte %01010000
    .byte %00010000
    .byte %01010000
    .byte %00010000
    .byte %01010000
    .byte %00010000
    .byte %01010000
    .byte %00010000
    .byte %01010000
    .byte %00010000
    .byte %11110000

ARENA_0_PF1:
    .byte %11100000
    .byte %11100110
    .byte %11100000
    .byte %11100110
    .byte %11100000
    .byte %11100110
    .byte %11100000
    .byte %11100110
    .byte %11100000
    .byte %11100110
    .byte %11100000

ARENA_0_PF2:
    .byte %00000000
    .byte %01100110
    .byte %00000000
    .byte %01100110
    .byte %00000000
    .byte %01100110
    .byte %00000000
    .byte %01100110
    .byte %00000000
    .byte %01100110
    .byte %00000000


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC       ; Move to position $FFFC
    .word RESET     ; Write 2 bytes with reset address
    .word RESET     ; Write 2 bytes with interruption vector
