;    MovRec macro
;
;               
;
;     !movrec rows,offsetx,offsety,filter 
;
;     useage 
;            ZP$FC < destination
;            ZP$FD > destination
;            ZP$FE < source
;            ZP$FF > source
;            LDX offsetx
;            LDY offsety
;
;PUSH ZP$FC-$FF
;PUSH AXY
;
; !for 
;  LDA  ZP$FC 
;  jsr filter 
;  STA  ZP$FC
;
;
;
;POP AXY
;POP ZP$FC-$FF

  