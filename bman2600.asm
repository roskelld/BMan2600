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
P0ANMSET                byte        ; P0 sprite animation frame offset
ANIMCOUNTER             byte        ; Current animation update countdown
MOVECOUNTER             byte        ; Counter to track updating movement
ANIM_FRAME              byte        ; Tracks if we're on first or second animation frame

ARENAINDEX              byte        ; Draw index of Arena
ARENA_SWITCH            byte        ; Toggle tracker
ARENACOUNTER            byte        ; Tracks the update for the arena
; ARENAPTRPF1             word
; ARENAPTRPF2             word
BOMB_SPRITE_PTR         word 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Define Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;P0_HGT                  = $11       ; player 0 sprite height (# rows in lookup table)
MOVE_RATE               = 160       ; Speed of player movement (255 == 100%)
ANIM_RATE               = 20        ; Speed of player movement (255 == 100%)
SPRITE_OFFSET_IDLE      = 0         ; Offset position of facing idle sprite
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

    lda #<Bomb0
    sta BOMB_SPRITE_PTR
    lda #>Bomb0
    sta BOMB_SPRITE_PTR+1

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
    ldx #165                    ;                   - playfield scanlines
    lda #1
    sta ARENACOUNTER            ;                   - We pull new arena data every 15 lines
    lda #%11110000              ; 2                 - Load PF0 slice (Always the same, so outside of loop)
    sta PF0                     ; 3                 - set PF0
;---------------------------------------------------- START OF GAME PLAY ZONE    
                                ; Cycles    Total   - Comment
.SLINE_LOOP                     ;                   - Gameplay Zone Scanline Loop
    sta WSYNC                   ;     3      0/78   - Start new Scanline
.ARENA_COUNT_DOWN               ; (7/8 Cycles)      - Every 15 scanlines update PF1 and PF2
    dec ARENACOUNTER            ;     5      5      - Count down arena draw counter 
    bne .INSIDE_P0              ;     2/3    7/9    - Jump to sprite check if no arena update needed
.RST_ARENA_COUNTER              ; (5 Cycles)        - [UPDATE ARENA] Reset update counter
    lda #15                     ;     2      9      - Reset Arena map update counter
    sta ARENACOUNTER            ;     3     12      - Store the arena draw counter
.DRAW_ARENA                     ; (22 Cycles)       - 
    ldy ARENAINDEX              ;     3     15      - Get the map data offset
    lda ARENA_0_PF1,y           ;     4     19      - Load PF1
    sta PF1                     ;     3     22      - set PF1 slice
    lda ARENA_0_PF2,y           ;     4     26      - Load PF2
    sta PF2                     ;     3     29      - Set PF2 slice
    inc ARENAINDEX              ;     5     34      - Move to next line of arena map data
     
.INSIDE_P0:                     ; (13/14 Cycles)    - P0 Position draw check 
    txa                         ;     2     36      - Transfer X to A
    sec                         ;     2     38      - Set carry before subtraction
    sbc P0POSY                  ;     3     41      - Subtract sprite Y coord
    cmp #P0_HGT                 ;     2     43      - Current scanline inside p0 sprite bounds?
    bcc .DRAWSPRP0              ;     2/3   45      - Draw P0 sprite routine
    lda #0                      ;     2     47      - else, index to 0
.DRAWSPRP0:                     ; (23 Cycles)       - P0 Draw sprite slice
    clc                         ;     2     49      -  
    adc P0ANMSET                ;     3     52      - Add animation frame offset ($0/$44)
    tay                         ;     2     54      - load Y so we can work with pointer
    lda (P0SPRPTR),Y            ;     5     59      - 
    sta GRP0                    ;     3     62      - set graphics for player0
    lda (P0COLPTR),Y            ;     5     67      -
    sta COLUP0                  ;     3     70      - set color of player 0


; .INSIDE_BOMB:
;     txa
;     sec 
;     sbc #TEST_BOMB_Y
;     cmp #BOMB_HGT
;     bcc .DRAWBOMB
;     lda #0
; .DRAWBOMB
;     clc
;     tay 
;     lda (BOMB_SPRITE_PTR),Y
;     sta GRP1 
;     lda #$0
;     sta COLUP1

.DECREMENT_SCANLINE             ; (5 Cycles)        - Decrement Scanline loop 
    dex                         ;     2     72      - Reduce scanline counter (x)
    bne .SLINE_LOOP             ;     3     75      - repeat until screen is drawn
;---------------------------------------------------- END OF GAME PLAY ZONE



.BOTTOM                         ;                   - [BOTTOM PANEL] Start of Bottom Panel
    sta WSYNC                   ;-----3 ----78------- wait for scanline
    ldx #6                      ;     2             - Bottom panel scanlines (6 for P0 color 19 for not)
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
P0_HGT = * - IdleSprite
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