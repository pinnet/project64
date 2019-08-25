  lda  #$1C                ; Change to RED text
          sta  error
          lda  #errorlast - errortext; Output error message
          sta  txtlength
          lda  #<errortext
          sta  PTRL
          lda  #>errortext
          sta  PTRH
          jsr  consoleout
          lda  #02
          sta  bdrcol
          jmp *
          
errorlast
copytext    !text ">LOADER V 1.0 (C)DANNYARNOLD.COM 2019"
copylast

clearview
          lda  #<SCREENRAM
          sta  PTRL
          lda  #>SCREENRAM
          sta  PTRH
         
          rts
          
consoleout                         ; Output cursor down until last line
          lda  #$13
          jsr  KERNAL_CHROUT
          ldy  #0
@start
          lda  #$11
          jsr  KERNAL_CHROUT
          iny
          cpy  #SCREENLINES
          bne  @start
padcenter
          lda  txtlength
          clc
          ror  a
          sta  pads
          lda  # SCREENCOLS / 2
          sec
          sbc  pads
          bcc  @cont
          sta  pads
          tay
          lda  #$1D
@lp
          jsr  KERNAL_CHROUT
          dey
          bne  @lp
@cont

textout
          ldy  #$00                ;
@loop
          lda  (PTRL),y            ; read chrs from pointer
          jsr  KERNAL_CHROUT       ; output using rom
          INY                      ;
          CPY  txtlength           ; has index (y) reached the length of text
          BNE  @loop               ; if not loop
          rts
          
          JMP*
 
 lda  #$9B
          sta  copytext
          lda  #>copytext
          sta  PTRH
          lda  #<copytext
          sta  PTRL
          lda  #copylast-copytext      ;length of string
          sta  txtlength
          JSR  consoleout          
          
countanim
          ldy  #5
          ldx  #0
@loop
          lda  count,x
          pha
          and  #$0f
          jsr  plotbcd
          pla
          lsr  a
          lsr  a
          lsr  a
          lsr  a
          jsr  plotbcd
          inx
          cpx  #3
          bne  @loop
          rts
plotbcd
          clc
          adc  #48
          sta  CLKPOS,y
          dey
          rts
          
deccount  sed                      ; DECIMAL MODE
          clc                      ; add one to count and carry
          lda  count               ; thanks to Robin @bedfordlvlexp
          adc  #1                  ; https://www.youtube.com/channel/UC3gRBswFkuteshdwMZAQafQ
          sta  count               ; for this one
          lda  count + 1;
          adc  #0                  ;
          sta  count + 1;
          lda  count + 2;
          adc  #0                  ;
          sta  count + 2;
          cld                      ; END DECIMAL MODE
          rts

statusout
          lda  #$13                ; Cursor Home
          jsr  KERNAL_CHROUT
          ldy  #0
@start
          lda  #$11                ; Cursor Down
          jsr  KERNAL_CHROUT
          iny
          cpy  #STATUSLINE
          bne  @start
          lda  #$1D                ; Cursor Right
          jsr  KERNAL_CHROUT
          jmp  textout


 