                                                                                            !zone LICENCE
;
;                                                 PROJECT 64
;                                       A Game by Daniel Arnold (C)2019
;
;
; --------------------------                      MIT Licence                     --------------------------
;
; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
; documentation files (the "Software"), to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all copies or substantial portions
; of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
;----------------------------------------------------------------------------------------------------------
                                                                                            !zone BOOTSTRAP
  * = $0801                                 ; BASIC start address (#2049
  !byte $0d,$08,$dc,$07,$9e,$20,$32         ; BASIC loader to start at $c000...
  !byte $31,$37,$36,$00,$00,$00,$00         ; puts BASIC line 2012 SYS 49152
  * = $0880                                 ; start address for 6502 code
        
;----------------------------------------------------------------------------------------------------------
                                                                                            !zone MEMORYMAP
  
  address_music = $1000                            ; loading address for sid tune
  sid_init  = $1000                                ; init routine for music
  sid_play  = $1003                                ; play music routine
  mapaddr   = $A000                                ; map address
  
;----------------------------------------------------------------------------------------------------------
                                                                                            !zone CONSTANTS
        ; Game constants
        
        FRAMETRIGGER = 50
        SCREENCOLS = 40
        SCREENLINES = 24
        VIEWPORTWIDTH = 26
        VIEWPORTHIGHT = SCREENLINES
        MAPHEIGHT = 13
        MENUHEIGHT = 6 + 7
        
        ; Memory location constants
        
        ZP_INTCLOCK     = $02              ; Intrupt clock 
        ZP_PTRH         = $fc              ; Addressing pointer high byte zero page address
        ZP_PTRL         = $fb              ; Addressing pointer low byte zero page address
        
        SCREENRAM       = $0400            ; Screen memory address
        COLORRAM        = $D800            ; Color memory address
        KERNAL_GETIN    = $FFE4            ; GETIN Kernal routine
        KERNAL_CHROUT   = $FFD2            ; CHROUT Kernal routine
        KERNAL_SETNAME  = $FFBD            ; SETNAME Kernal routine
        KERNEL_SETLFS   = $FFBA            ; SETLFS Kernal routine
        KERNAL_LOAD     = $FFD5            ; LOAD Kernal routine
        KVAR_TEXTCOL    = $0286            ; Text color
        SYS_INTVECTOR   = $0314            ; Iterrupt vector
        VIC_BACKCOL     = $d021            ; Background color
        VIC_BORDCOL     = $d020            ; Border color
        VIC_CHRSET      = $d018            ; Char set
        VIC_INTSTATREGA = $dc0d            ; Interrupt status register A
        VIC_INTSTATREGB = $dd0d            ; Interrupt status register B
        VIC_INTCTLREG   = $d01a            ; Interrupt control register
        VIC_CURRASTER   = $d012            ; Current raster line
        VIC_SCRCTLREG   = $d011            ; Screen control register
        
        ; Assembler defines
        
        DEBUG_NOMUSIC  
;---------------------------------------------------------------------------------------------------------  

                                                                                            !zone INIT 
 initialize
   
         lda $01                  ; Switch out Basic
         and #$fe                 ;
         sta $01                  ;
         
         lda  #FRAMETRIGGER       ; number of frames until counter is reset
         sta  ZP_INTCLOCK         ; inttrupt generated clock
         
         lda  #$00                ; Initalise ----------------------------------------
                                  ;                                                  |  
         sta  bdrcol              ; dynamic border colour updated every FRAME        |
         sta  doneload            ; done loading flag                                |
         sta  ZP_PTRL             ; Zero Page Pointer Low                            |
         sta  ZP_PTRH             ; Zero Page Pointer High                           |
         sta  VIC_BACKCOL         ; initalise background and text color the same     |                  ;
         sta  KVAR_TEXTCOL        ; for a 'quick blank'                              |
         sta  VIC_BORDCOL         ; set border colour                                |
         lda  #$93                ; clear screen                                     |
         jsr  KERNAL_CHROUT       ; -------------------------------------------------|
                                  ; Set up raster interrupt                          |    
                                  ; -------------------------------------------------|
         sei                      ; Set Interrupts                                   |       
         ldy  #$7f                ; $7f = %01111111                                  |
         sty  VIC_INTSTATREGA     ; Turn off CIAs Timer interrupts                   |
         sty  VIC_INTSTATREGB     ; Turn off CIAs Timer interrupts                   |
         lda  VIC_INTSTATREGA     ; cancel all CIA-IRQs in queue/unprocessed         |
         lda  VIC_INTSTATREGB     ; cancel all CIA-IRQs in queue/unprocessed         |
         lda  #$01                ; Set Interrupt Request Mask...                    |
         sta  VIC_INTCTLREG       ; ...we want IRQ by Rasterbeam                     |
         lda  #<irq               ; point IRQ Vector to our custom irq routine       |
         ldx  #>irq               ;                                                  |
         sta  SYS_INTVECTOR       ; store in interrupt vector                        |
         stx  SYS_INTVECTOR +1    ;                                                  |
         lda  #$10                ; trigger first interrupt at row zero              |
         sta  VIC_CURRASTER       ;                                                  |
         lda  VIC_SCRCTLREG       ; Bit#0 of $d011 is basically...                   |
         and  #$7f                ; ...the 9th Bit for $d012                         |
         sta  VIC_SCRCTLREG       ; we need to make sure it is set to zero           |
         cli                      ; clear interrupt disable flag                     |
         ;----------------------------------------------------------------------------
          
         
         lda  #fnameend-fname    ; Load Screen 
         sta  txtlength
         ldx  #<fname
         ldy  #>fname
         jsr  loadcode
         
         lda  #cnameend-cname    ; Load Color Table
         sta  txtlength
         ldx  #<cname
         ldy  #>cname
         jsr  loadcode
         
         lda  #mnameend-mname    ; Load Map
         sta  txtlength
         ldx  #<mname
         ldy  #>mname
         jsr  loadcode
         
 !ifndef DEBUG_NOMUSIC { 
         lda  #sidend-sid         ; Load SID Tune
         sta  txtlength
         ldx  #<sid
         ldy  #>sid
         jsr  loadcode
         lda  #2                  ; Init SID tune 
         jsr  sid_init            ;
         lda  #$01                ; Flag Music ready
         sta  musicready          ;
}
       
         jsr clearmap
         jsr clrmenu
         lda #0
         sta VIC_BORDCOL
;---------------------------------------------------------------------------------------------------------
         
                                                                                            !zone GAMELOOP
start
          jsr menu                        ; Display menu
          
main_loop                                 ; ---------------------------------------------------------------
          jsr clrvpcr                     ;                                                        |      |
          lda count                       ;                                      Full Speed Loop   |      |
          cmp oldcount                    ;                                                        |      |
          beq main_loop                   ; Wait untll the counter changes                  <-------      |
          sta oldcount                    ; update oldcount with new count                                |
                                          ;                                      Count Speed Loop         |
          jmp main_loop                   ;                                                 <--------------              
                                          ;----------------------------------------------------------------
          
          jmp start                       ; Start game loop
restart   jmp initialize                  ; Reload game
          jmp restart                     ;
;----------------------------------------------------------------------------------------------------------
;                                   Raster Interrupt         
;----------------------------------------------------------------------------------------------------------
                                                                                            !zone INT_RASTER


irq
          dec  $d019               ; acknowledge IRQ / clear register for next interrupt
          pha                      ; Push the registers on the stack
          txa                      ;
          pha                      ;
          tya                      ;
          pha                      ;
          lda  ZP_PTRH                ; Push the Pointer on the stack
          pha                      ;
          lda  ZP_PTRL                ;
          pha                      ;
          
          lda doneload
          beq .bskip
          inc VIC_BORDCOL
.bskip    
          dec  ZP_INTCLOCK            ; count down int clock
          bne  .skip               ; skip ahead until zero
          lda  #FRAMETRIGGER       ; then reset int clock
          sta  ZP_INTCLOCK            ; to FRAMETRIGGER default
          inc  txtcol
          
.skip       
          
          lda  musicready          ; if music ready is not ready
          cmp  #1                  ; then skip calling the play subroutine
          bne  .skipplay           ;
          jsr  sid_play            ;
          
.skipplay
          
          pla                      ; Pull the pointer off the stack
          sta  ZP_PTRL             ;
          pla                      ;
          sta  ZP_PTRH             ;
          pla                      ; Pull the regesters off the staack
          tay                      ;
          pla                      ;
          tax                      ;
          pla                      ;
          jmp  $ea31               ; return to kernel interrupt routine
          
;-----------------------------------------------------------------------------------------------------------
;                                   Subroutines
;-----------------------------------------------------------------------------------------------------------
                                                                                            !zone SUB_LOADCODE

loadcode
          inc  doneload
          lda  txtlength
          jsr  KERNAL_SETNAME      ; call SETNAM
          lda  #$01
          ldx  $BA                 ; end used device number
          bne  @skip
          ldx  #$08                ; default to device 8
@skip                              ; not $01 means: load to address stored in file
          jsr  KERNEL_SETLFS       ; call SETLFS
          lda  #$00                ; $00 means: load to memory (not verify)
          jsr  KERNAL_LOAD         ; call LOAD
          bcs  .error              ; if carry set, a load error has happened
          lda  #0
          sta  doneload
          rts                      ; End SUB
.error
          sei
          ldy  #$7f                ; $7f = %01111111
          sty  $dc0d               ; Turn off CIAs Timer interrupts
          sty  $dd0d               ; Turn off CIAs Timer interrupts
          lda  $dc0d               ; cancel all CIA-IRQs in queue/unprocessed
          lda  $dd0d               ; cancel all CIA-IRQs in queue/unprocessed
          
          lda  #$93                ; clear screen character
          jsr  KERNAL_CHROUT
          lda  #2
          sta  KVAR_TEXTCOL
          lda #12
          sta cursorx;
          
          lda  #fileerrortextend - fileerrortext
          sta  txtlength  
          jsr  pad
          lda  #<fileerrortext         
          sta  ZP_PTRL
          lda  #>fileerrortext
          sta  ZP_PTRH
          jsr  textout            ; display error message
          
          
          lda  #02                ;
          sta  VIC_BORDCOL            ;
          
          jmp *                   ; loop forever
;----------------------------------------------------------------------------------------------------------
                                                                                            !zone SUB_PAD
pad
          lda  txtlength
          clc
          ror  a
          sta  padding
          lda  # SCREENCOLS / 2
          sec
          sbc  padding
          bcc  .skip
          sta  cursory
.skip
          rts
          
;----------------------------------------------------------------------------------------------------------          
                                                                                            !zone SUB_TEXTOUT
  
textout                            ; 
          lda  #$13
          jsr  KERNAL_CHROUT
          ldx cursorx
.xloop    beq xskip
          lda  #$11
          jsr  KERNAL_CHROUT
          dex
          jmp .xloop
xskip     
          ldx cursory
.yloop    beq yskip
          lda  #$1D
          jsr  KERNAL_CHROUT
          dex
          jmp .yloop
 yskip          


          ldy #0
.loop
          lda  (ZP_PTRL),y            ; read chrs from pointer
          jsr  KERNAL_CHROUT       ; output using rom
          iny                      ;
          cpy  txtlength           ; has index (y) reached the length of text
          bne  .loop               ; if not loop
          rts                      ; End Sub
;---------------------------------------------------------------------------------------------------------          
                                                                                            !zone SUB_CLEARMAP
  
clearmap
         lda #2
         sta cursorx;
         lda #28
         sta cursory;
.loop
         lda  #clrmapend - clrmap
         sta  txtlength
         lda  #<clrmap
         sta  ZP_PTRL
         lda  #>clrmap
         sta  ZP_PTRH
         jsr  textout 
         inc  cursorx
         lda  #MAPHEIGHT
         cmp  cursorx
         bpl  .loop
         rts 
         
;--------------------------------------------------------------------------------------------------------
                                                                                            !zone SUB_CLRMENU
  
clrmenu
         lda #2
         sta  KVAR_TEXTCOL
         lda #7
         sta cursorx;
         lda #5
         sta cursory;
.loop
         lda  #menuclrend - menuclr
         sta  txtlength
         lda  #<menuclr
         sta  ZP_PTRL
         lda  #>menuclr
         sta  ZP_PTRH
         jsr  textout 
         inc  cursorx
         lda  #MENUHEIGHT
         cmp  cursorx
         bpl  .loop
         rts 
         
;--------------------------------------------------------------------------------------------------------
                                                                                            !zone SUB_CLEARVP                 
clrvp

          lda  #<SCREENRAM
          sta ZP_PTRL
          lda  #>SCREENRAM
          sta ZP_PTRH
    
          ldx  #0
@doloop    
          cpx #25
          beq @skip
          ldy  #0                    
          lda  #$66
@loop
          sta  (ZP_PTRL),y
          iny                   
          cpy  #27           
          bne  @loop             
          inx
          clc
          lda ZP_PTRL
          adc #40
          sta ZP_PTRL
          bcc @doloop
          inc ZP_PTRH
          jmp @doloop
          
@skip
         
          
clrvpcr

          lda  #<COLORRAM
          sta  ZP_PTRL
          lda  #>COLORRAM
          sta  ZP_PTRH
    
          ldx  #0
doloop    
          cpx #25
          beq .skip
          ldy  #0                    
          lda  txtcol
@loop
          sta  (ZP_PTRL),y
          INY                   
          CPY  #27           
          BNE  @loop             
          inx
          clc
          lda ZP_PTRL
          adc #40
          sta ZP_PTRL
          bcc doloop
          inc ZP_PTRH
          jmp doloop
          
.skip
          rts
          
          jmp *
  
;----------------------------------------------------------------------------------------------------------
                                                                                            !zone SUB_MENU 
  
menu  
         ldx  txtcol
         stx  KVAR_TEXTCOL
          
         lda #10
         sta cursorx;
         lda #7
         sta cursory;
         
         lda  #startend - starttext
         sta  txtlength
         lda  #<starttext
         sta  ZP_PTRL
         lda  #>starttext
         sta  ZP_PTRH
         jsr  textout 
          
         lda  #02
         sta  bdrcol
         jsr  KERNAL_GETIN
         beq  menu
         
         jsr clrvp
         
         lda  #0
         sta  oldcount
         rts
         
         jmp *
;----------------------------------------------------------------------------------------------------------
                                                                                            !zone DATA
      cursory      !byte 0
      cursorx      !byte 0
      txtcol       !byte 0
      musicready   !byte 0
      txtlength    !byte 0
      oldcount     !byte 0
      bdrcol       !byte 0
      padding      !byte 0
      doneload     !byte 0

      fileerrortext   !text "FILE ERROR"
      fileerrortextend

      menuclr     !text   "                 "
      menuclrend

      starttext   !text "PRESS ANY KEY"
      startend     

      clrmap      !text "           "
      clrmapend

      cname       !text "LOADC"
      cnameend
      mname       !text "M1"
      mnameend
      fname       !text "LOADS"
      fnameend

!ifndef DEBUG_NOMUSIC {
      sid         !text "SID"
      sidend
}

      count       !byte 0, 0, 0