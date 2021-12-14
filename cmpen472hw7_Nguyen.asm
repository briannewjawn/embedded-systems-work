***********************************************************************
*
* Title:          SCI Serial Port and 7-segment Display at PORTB
*
* Objective:      CMPEN 472 Homework 7, in-class-room demonstration
*                 program
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:          Oct. 4 2021
*
* Programmer:     Brian Nguyen
*
* Company:        The Pennsylvania State University
*                 Department of Computer Science and Engineering
*
* Program:        Simple SCI Serial Port I/O and Demonstration
*                 Typewriter program and 7-Segment display, at PORTB
*                
*
* Algorithm:      calculator
*
* Register use:  A: Serial port data
*                 X,Y: Delay loop counters
*
* Memory use:     RAM Locations from $3000 for data,
*                 RAM Locations from $3100 for program
*
* Output:        
*                 PORTB bit 7 to bit 4, 7-segment MSB
*                 PORTB bit 3 to bit 0, 7-segment LSB
*
* Observation:    This is a typewriter program that displays ASCII
*                 data on PORTB - 7-segment displays.
*
***********************************************************************
* Parameter Declearation Section
*
* Export Symbols
            XDEF        pstart       ; export 'pstart' symbol
            ABSENTRY    pstart       ; for assembly entry point
 
* Symbols and Macros
PORTB       EQU         $0001        ; i/o port B addresses
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register


CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address

Buff        DS.B        13
                                     ;   for Data for CMPEN 472 class
CCount      DS.B        1       ; X register count number for time delay

                                     ;   inner loop for msec
NUM1   DS.B        3           ;for printing out decimal numbers
NUM2   DS.B        3
ANS    DS.B        3
reverse DS.B     6
msg1        DC.B        'S:       Show the contents of memory location', $00
msg2        DC.B        'W:      Write the data byte to memory location', $00                          
        ; Y register count number for time delay
                                     ;   outer loop for sec
                                     




; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of
; the program - before the last "END" line.
                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start

*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*

            ORG        $3100        ; Program start address, in RAM
pstart      LDS        #$3100       ; initialize the stack pointer

            LDAA       #%11111111   ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA       DDRB         ; as output

            LDAA       #%00000000
            STAA       PORTB        ; clear all bits of PORTB

            ldaa       #$0C         ; Enable SCI port Tx and Rx units
            staa       SCICR2       ; disable SCI interrupts

            ldd        #$0001       ; Set SCI Baud Register = $0001 => 2M baud at 24MHz (for simulation)
;            ldd        #$0002       ; Set SCI Baud Register = $0002 => 1M baud at 24MHz
;            ldd        #$000D       ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd        #$009C       ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std        SCIBDH       ; SCI port baud rate change








           
           
           
buffinitial  
             ldx   #msg13       ;load msg10 into register x      
            jsr   printmsg     ; jump to print msg subroutine
            clr reverse
             clr NUM1
             clr NUM2
             clr ANS              
             LDX #Buff                  ;load buff into x register
             CLR CCount                 ;clear ccount
             ;CLR writethis
             ;CLR storehere
             CLR $3000                  ;clear memory space 3000
             CLR $3001                  ;clear memory space 3001
             CLR $3002                  ;clear memory space 3002
             CLR $3003                  ;clear memory space 3003
             CLR $3004                  ;clear memory space 3004
             CLR $3005                  ;clear memory space 3005
             CLR $3006                  ;clear memory space 3006
             CLR $3007                  ;clear memory space 3007
             CLR $3008                  ;clear memory space 3008
             CLR $3009                  ;clear memory space 3009
             CLR $300A                  ;clear memory space 300A
             clr $3017
             clr $3018
             clr $3019
             clr $301A
             clr $301B
             clr $301C
             
       
looop      


           
            jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   looop
 
                                     ;  otherwise - what is typed on key boar
            STAA  1,X+               ;increase x counter store in a
            INC   CCount             ;increase ccount
            jsr   putchar            ;jump to putchar subroutine
           


            cmpa #$0D           ;check for enter key
            beq enterkeypressed     ;if enter key is pressed jump to buff initialization            
            ldab   CCount       ; load count into accumulator B
            cmpb   #9           ; compare count to 5
            BGE    errorthrow  ; if count is greater or equal to 5 jump to buff initialize


           

           bra looop     ;if nothing is satisifed jump back to buff initialized


enterkeypressed
            psha                    ;push a
            pshb                    ;push b
           LDAB $3000               ;load memory 3000
           jsr  convb               ;perform conversion
           STAB $3000
           LDAB $3001
           jsr  convb
           STAB $3001
           LDAB $3002
           jsr  convb
           STAB $3002
           
           
           LDAB $3004
           jsr  convb
           STAB $3004
           LDAB $3005
           jsr  convb
           STAB $3005
           LDAB $3006
           jsr  convb
           STAB $3006
                                           
                                   
            LDAA  $3003             ;load memory space 3000
            cmpa  #$2B              ; is it +?
            Lbeq   adds             ;jump to adds subroutine        
            cmpa  #$2D              ; is it -?
            Lbeq   subs             ;jump to subtract subroutine  
            cmpa  #$2A              ;is it *?
            Lbeq  muls              ;jump to multiply sub routine
            cmpa  #$2F              ;is it /?
            Lbeq  divis             ; jump to division
           pula                     ;pula
           pshb                     ;pulb
           
           jsr buffinitial              ;if nothing is entered when enter key is go back to main loop


           
           
;subroutine section below          
errorthrow  
            pshb                    ;error throw loop
            ldab   CCount       ;load count into register B
            cmpb   #9           ;compare if b is equal to 5
            BGE    errorthrowmsg ;if greater than 5 or same jump to errorwthrow msg
            pulb
            rts                  ;return back to previous loop
             


errorthrowmsg                  ;error throw msg

            ldx   #msg10       ;load msg10 into register x      
            jsr   printmsg     ; jump to print msg subroutine
            jsr   nextline     ;jump to nextline subroutine
            jsr   buffinitial  ;jump to buff initialization
           

errorthrow2
            ldx   #msg11       ;load msg11 into register x      
            jsr   printmsg     ; jump to print msg subroutine
            jsr   nextline     ;jump to nextline subroutine
            jsr   buffinitial  ;jump to buff initialization
           
errorthrow3
            ldx   #msg12       ;load msg11 into register x      
            jsr   printmsg     ; jump to print msg subroutine
            jsr   nextline     ;jump to nextline subroutine
            jsr   buffinitial  ;jump to buff initialization
           
sucessmsg
            ldx   #msg12       ;load msg12 into register x      
            jsr   printmsg     ; jump to print msg subroutine
            jsr   nextline     ;jump to nextline subroutine
            jsr   buffinitial  ;jump to buff initialization
           
                       


nextline    ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
           
            rts

           
           
;***********add subrountine***************************
adds


           jsr storenum


           LDD $300E                     ;load num1 location
           ADDD $3011                    ; add to num2 location
           STD ANS                       ;store in ans
           jsr printans                  ;jump to print ans subroutine
             
           
;****************************subtract subroutine*********************************************            
             
subs
                                     
           jsr storenum

           LDD $300E                    ;load num1 location
           SUBD $3011                   ;subtract from num2 location
           cmpa #128                    ;see if number is bigger than 1000 0000
           LBHS negativenum             ; if it is bigger jump to negative num sum roubtine
           STD ANS                      ;otherwise  store ans 
           jsr printans                 ;jump to print ans subroutine
;*************************multiply roubtien******************************************

muls
           jsr storenum
           

           LDD $300E                         ;load num1
           LDY $3011                         ;load num2
           EMUL                              ;multiply
           cpy #00                           ;if there is an overflow (y has something in its register)
           LBHI errorthrow3                  ;throw overflow error
           STD ANS                           ;other wise go to ans
       
           jsr printans                      ;go to print ans
;*****************************divide subroutine****************************************

divis
           jsr storenum

           LDD $300E           ;load num1
           LDX $3011           ;load num2
           IDIV                ;divide
           STX ANS             ;store ans
           jsr printans        ;jump to print ans subroutine
;*************************print ans subroutine*******************************************
printans                            
           LDY #reverse       ;load reverse 
           LDD ANS              ;load anser into D
xloop    
                                ;convert into deciaml number loop
           ldx  #$A             ;load 10 into x
           IDIV                 ;divide D/X
           addb #$30            ;do asicc bias
     
           STAB 1,Y+            ;store into reverse
           subb #$30            ;subtrct asci bias
           TFR X,D              ;transfer x to d register
           cpx  #$00            ;is x empty?
           BNE  xloop           ;if not loop
printloop                       
           LDAA 1,Y-            ;read memory location reverse
           jsr putchar          ;put mem
           cpy #$3016           ;is the memory location 3016(beginning)>?
           bne printloop        ;if not keep printing
           jsr nextline         ;jump next line
           
           jsr buffinitial
;*****************************negative number****************************************
negativenum                     ;neggative numebr subroutine
            nega                 ;negate A
            suba #1              ;subtract 1 cus nega adds 1
            negb                 ;negate b
            STD ANS               ;store D into ans
            LDAA #$2D             ;load negative sign
            jsr putchar           ;put char
            jsr printans           ;jump to print ans route

            rts

         
;*************************************************************************
typewriter
            jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            lbeq   typewriter
                                       ;  otherwise - what is typed on key board
            jsr   putchar            ; is displayed on the terminal window - echo print

            staa  PORTB              ; show the character on PORTB

            cmpa  #CR
            lbne  typewriter              ; if Enter/Return key is pressed, move the
            ldaa  #LF                ; cursor to next line
            jsr   putchar
            bra   typewriter

;*******************storenum*******************************************
           


storenum  
           clra
           LDAB $3000         
           LDY #$A
           EMUL
           ADDB $3001
           LDY #$A
           EMUL
           cmpb #$FA
           jsr  twofiftysix
           
           ADDB $3002
           
          STD NUM1

           clra
           LDAB $3004
           LDY #$A
           EMUL
           ADDB $3005
           LDY #$A
           EMUL
           cmpb #$FA
           jsr  twofiftysixb
           ADDB $3006
           STD NUM2
           rts
 ;*************************************************************************
 
twofiftysix 
           psha
           LDAA $3002
           tfr A,X
           cpx #$06
           pula
           LBHS twofiftysix2
           rts
           
twofiftysix2
             ABX
             TFR X,D
             STD NUM1
             psha
             LDAA #$00
             STAA $3002
             pula
             rts

;*************************************************************************

twofiftysixb 
           psha
           LDAA $3006
           tfr A,X
           cpx #$06
           pula
           LBHS twofiftysix2b
           rts
           
twofiftysix2b
             ABX
             TFR X,D
             STD NUM2
             psha
             LDAA #$00
             STAA $3006
             pula
             rts
                       
;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;*
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx
               pula
               rts
;***********end of printmsg********************


;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL                      ; send a character
               rts
;***************end of putchar*****************




             
;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar****************


;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip
msg3        DC.B        'QUIT:   Quit the main program, run Type writer program.', $00
msg10        DC.B        'invalid input', $00
msg11       DC.B    'input not allowed' , $00
msg12       DC.B    'overflow error' , $00
msg13       DC.B    'Ecalc>',$00

;************************valid number?********************************************
convb
         
         
          cmpb #$30                            ;is num les than 0?
          lBLO  errorthrow2                     ;jump to error if so
          cmpb  #$39                           ;is num more than 9?
          BHI  morethan9b                      ;jump to more than 9 sub
          subb #$30                             ; else add 30
          rts

morethan9b
         cmpb #$46                             ;is num more than F?
         lBHI  errorthrow2                      ;if so throw error
         cmpb #$41                             ;is number less than A>
         lBLO  errorthrow2                      ;throw error also
         subb  #$37                            ;else subtract 37
         rts
 




               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled