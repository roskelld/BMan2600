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
P0GRABUF		        byte		; Buffer for P0 graphics for one scanline
P0COLBUF		        byte		; Buffer for P0 color for one scanline
P0ANMSET                byte        ; P0 sprite animation frame offset
P0DRAW                  byte        ; P0 Draw Height
ANIMCOUNTER             byte        ; Current animation update countdown
MOVECOUNTER             byte        ; Counter to track updating movement
ANIM_FRAME              byte        ; Tracks if we're on first or second animation frame

ARENAINDEX              byte        ; Draw index of Arena
ARENA_SWITCH            byte        ; Toggle tracker
ARENACOUNTER            byte        ; Tracks the update for the arena

ARENABUFFER		        ds.b	22	; Buffer for arena layout data
STACKPTRBUFFER		    byte		; Keep a copy of the stack pointer when we clobber it

BOMB_SPRITE_PTR         word        ;
TEMP                    ds 1        ; Scratch Variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Define Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PLAYER_HEIGHT                  = $11       ; player 0 sprite height (# rows in lookup table)
MOVE_RATE               = 160       ; Speed of player movement (255 == 100%)
ANIM_RATE               = 20        ; Speed of player movement (255 == 100%)
SPRITE_OFFSET_IDLE      = 0         ; Offset position of facing idle sprite
SPRITE_OFFSET_DOWN      = $11       ; Offset position of facing down sprite
SPRITE_OFFSET_UP        = $22       ; Offset position of facing up sprite
SPRITE_OFFSET_RIGHT     = $33       ; Offset position of facing right sprite
SPRITE_FRAME_OFFSET     = $44       ; Second frame of animation offset
UP_BOUNDS               = $A8       ; Top Player Boundary
DOWN_BOUNDS             = $10       ; Bottom Player Boundary
LEFT_BOUNDS             = $0A       ; Left Player Boundary
RIGHT_BOUNDS            = $6c       ; Right Player Boundary
VERTICAL_STEP           = $1
HORIZONTAL_STEP         = $1
ARENA_HEIGHT            = $A5        ; (0-83)*2=166 scanlines for arena (2LK)
ARENA_BG                = $C3

X_LANE_START            = $10
X_LANE_UPDATE           = $1E
X_LANE_WALK_UP          = $7
X_LANE_BLOCKED          = $7
X_LANE_WALK_DOWN        = $F

Y_LANE_START            = $0A
Y_LANE_UPDATE           = $10
Y_LANE_WALK_RIGHT       = $5
Y_LANE_BLOCKED          = $5
Y_LANE_WALK_LEFT        = $5

BOMB_HGT                = $9
TEST_BOMB_X             = $0A       ; 
TEST_BOMB_Y             = $7A       ; Cell 38
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
    lda #$A5
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

    lda #<Bomb0
    sta BOMB_SPRITE_PTR
    lda #>Bomb0
    sta BOMB_SPRITE_PTR+1

                                ; Cycles    Total   - Comment
; Fill the arena buffer in RAM from ROM
	ldx #21			            ;      2            - Memory offset value for ARENABUFFER
	ldy #10			            ;      2            - Index of PF graphic data (start from end)
FILL_ARENA_BUFFER:
	lda ARENA_0_PF1,y			;      4            - Load PF1 Arena Data with Y offset
	sta ARENABUFFER,x	        ;      4            - Store in RAM with X index offset
	dex			                ;      2            - Shift X index for next update
	lda ARENA_0_PF2,y	        ;      4            - Load PF1 Arena Data with Y offset
	sta ARENABUFFER,x	        ;      4            - Store in RAM with X index offset
	dex			                ;      2            - Shift X index for next update
	dey			                ;      2            - Shift Y index for next update
	bpl FILL_ARENA_BUFFER	    ;    2/3            - Loop if still plus

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
    lda #0
    sta P0GRABUF		        ;
	sta P0COLBUF		        ;
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
ARENA_SETUP:
    lda #ARENA_BG               ;                   - background color  (61st scanline)
    sta COLUBK                  ;                   - set background color
    lda #$09                    ;                   - playfield color
    sta COLUPF                  ;                   - set playfield color

    tsx                         ;     2             - Save SP (Stack Pointer) before clobbering it 
    stx STACKPTRBUFFER          ;     3             - Store old SP (Stack pointer) to RAM
    ldx #<ARENABUFFER-1         ;     2             - Load Arena Graphic slice from RAM
    txs                         ;     2             - Store Arena Graphic on the SP (Stack Pointer)

    ldx #177                    ;                   - playfield scanlines
    lda #%11110000              ; 2                 - Load PF0 slice (Always the same, so outside of loop)
    sta PF0                     ; 3                 - set PF0
    lda #%11111111              ; 2                 - Load PF0 slice (Always the same, so outside of loop)
    sta PF2                     ;
    sta PF1                     ;
;---------------------------------------------------- START OF GAME PLAY ZONE 
                                ; Cycles    Total   - Comment
    sta WSYNC                   ;     3             - Start a fresh line
.SLINE_LOOP                     ; (14 Cycles)       - Gameplay Zone Scanline Loop
    lda P0GRABUF                ;     3             - Load P0 graphic buffer
    sta GRP0                    ;     3             - Send P0 data to TIA
	lda P0COLBUF		        ;     3             - Load P0 color buffer
	sta COLUP0		            ;     3             - Send P0 data to TIA
    dex                         ;     2             - Decrement scanline counter

                                ; (11/12 Cycles)    - Check if need to Draw P0
    txa                         ;     2             - Buffer P0 data for next scanline
    sec                         ;     2             - 
    sbc P0POSY                  ;     3             - Player Y from scanline height
    cmp #PLAYER_HEIGHT          ;     2             - Player Height
    bcs .NO_P0                  ;   2/3             - Branch if not needing to draw P0

    adc P0ANMSET                ;     3             - 
    tay                         ;     2             -
    lda (P0SPRPTR),y            ;     5             - 
    sta P0GRABUF                ;
    lda (P0COLPTR),y            ;
    sta P0COLBUF                ;
    bcc .AFTER_P0               ;

.NO_P0
    lda #0                      ;
    sta P0GRABUF                ;
    ldy #3                      ;

.BUSYWAIT_01
    dey                         ;
    bne .BUSYWAIT_01            ;
    nop                         ;
.AFTER_P0
    txa                         ;
    and #$0f                    ;
    bne .NO_PF                  ;

    nop                         ;
    pla                         ;
    sta PF2                     ;
    pla                         ;
    sta PF1                     ;

    cpx #0                      ;
    bne .SLINE_LOOP             ;

.NO_PF
    nop                         ;
    nop                         ;
    nop                         ;
    nop                         ;
    nop                         ;
    nop                         ;
    cmp $80                     ;
    cpx #0                      ;
    bne .SLINE_LOOP             ;

    ldx STACKPTRBUFFER          ;
    txs                         ;

;---------------------------------------------------- END OF GAME PLAY ZONE



.BOTTOM                         ;                   - [BOTTOM PANEL] Start of Bottom Panel
    sta WSYNC                   ;-----3 ----78------- wait for scanline
;    ldx #0                      ;     2             - Bottom panel scanlines (6 for P0 color 19 for not)
    lda #$07                    ;     2             - panel color
    sta COLUBK                  ;     3             - background
    sta COLUPF                  ;     3             - playfield set both the same to create border
;BOTTOM_PANEL:
;    sta WSYNC                   ;                   - wait for scanline
;    dex                         ;                   - X--
;    bne BOTTOM_PANEL            ;                   - repeat until screen is drawn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OVERSCAN - 30 SCANLINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OVERSCAN:
	TIMER_SETUP 30
    TIMER_WAIT

    jmp NEXTFRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is target X-coord position
;; Y is object (0: P0, 1: P1, 2: MISSILE0, 3: MISSILE1, 4: BALL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SETXPOS Subroutine
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

    ; Prepare P0 Y for 2LK
    ; ldx #1                      ; preload X for setting VDELPx
    ; lda P0POSY                  ; get the P0 Y position
    ; ;clc                         ;
    ; ;adc #1                      ; add 1 to compensate for priming of GRP0 
    ; ; Removed the divide, not sure how to incorporate it into the code
    ; ;lsr                         ; divide by 2 for the 2LK position
    ; sta TEMP                    ; save for position calculations

    ; ; P0 Sprite Height in Arena
    ; ; P0DRAW = ARENA_HEIGHT + PLAYER_HEIGHT - P0POSY + 1
    ; lda #(ARENA_HEIGHT + PLAYER_HEIGHT)
    ; sec 
    ; sbc P0POSY
    ; sta P0DRAW

    ; ; P0 Sprite Pointer
    ; lda #<(#IdleSprite + PLAYER_HEIGHT - 1) 
    ; sec
    ; sbc TEMP
    ; sta P0SPRPTR
    ; lda #>(#IdleSprite + PLAYER_HEIGHT - 1)
    ; sbc #0
    ; sta P0SPRPTR+1

    rts 
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
    lda #X_LANE_START            ;                   - Sets a to bottom bounds
.LEFT_NEXT_LANE                 ;                   -
    adc #X_LANE_UPDATE           ;                   - Lane loop value
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
    lda #X_LANE_START            ;                   - Sets a to bottom bounds
.RIGHT_NEXT_LANE                ;                   -
    adc #X_LANE_UPDATE           ;                   - Lane loop value
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
PLAYER_HEIGHT = * - IdleSprite
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

Bomb0
        .byte #%00000000;$0E
        .byte #%00011000;$0E
        .byte #%00111100;$0E
        .byte #%00111100;$0E
        .byte #%00111100;$0E
        .byte #%00111100;$0E
        .byte #%00011000;$0E
        .byte #%00001010;$1C
        .byte #%00000100;$1C

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