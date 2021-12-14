;*******************************************************
;* CMPEN 472, HW8 Real Time Interrupt, MC9S12C128 Program
;* CodeWarrior Simulator/Debug edition, not for CSM-12C128 board
;* Oct.  13, 2016 brian nguyen
;* Oct.  14, 2020 brian nguyen
;* Oct.  23, 2021 brian nguyen
;* 
;* 1 second LED1 blink, timer using Real Time Interrupt.
;* This program is a 1 second timer using 
;* a Real Time Interrupt service subroutine (RTIISR).  This program
;* displays the time on the 7 Segment Disply in Visualization Tool 
;* every 1 second.  That is, this program 
;* displays '1 0 1 0 1 0 . . . ' on the 7 segment displys. 
;* The 7 segment displys are connected to port B of
;* MC9S12C32 chip in CodeWarrior Debugger/Simulator.
;* Also on the Terminal component of the simulator,  
;* user may enter any key, it will be displayed on the screen - effectively
;* it is a typewriter.
;*
;* Please note the new feature of this program:
;* RTI vector, initialization of CRGFLG, CRGINT, RTICTL, registers for the
;* Real Time Interrupt.
;* We assumed 24MHz bus clock and 4MHz external resonator clock frequency.  
;* This program any user input (a typewriter). 
;* 
;*******************************************************
;*******************************************************
;***********************************************************************
;*
;* Title:          SCI Serial Port and 8-segment Display at PORTB   and timer
;*
;* Objective:      CMPEN 472 Homework 8, in-class-room demonstration
;*                 program
;*
;* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
;*
;* Date:          Oct. 28 2021
;*
;* Programmer:     Brian Nguyen
;*
;* Company:        The Pennsylvania State University
;*                 Department of Computer Science and Engineering
;*
;* Program:        Simple SCI Serial Port I/O and Demonstration
;*                 Typewriter program and 7-Segment display, at PORTB
;*                
;*
;* Algorithm:      calculator and timer
;*
;* Register use:  A: Serial port data
;;*                 X,Y: Delay loop counters
;*
;* Memory use:     RAM Locations from $3000 for data,
;*                 RAM Locations from $3100 for program
;*
;* Output:        
;*                 PORTB bit 7 to bit 4, 7-segment MSB
;*                 PORTB bit 3 to bit 0, 7-segment LSB
;*
;* Observation:    This is a typewriter program that displays ASCII
;*                 data on PORTB - 7-segment displays.
;*
;***********************************************************************



; export symbols - program starting point
            XDEF        Entry        ; export 'Entry' symbol
            ABSENTRY    Entry        ; for assembly entry point

; include derivative specific macros
PORTB       EQU         $0001
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section
            ORG    $3000             ; RAMStart defined as $3000
                                     ; in MC9S12C128 chip

Buff        DS.B        13
                                     ;   for Data for CMPEN 472 class
CCount      DS.B        1       ; X register count number for time delay

NUM1   DS.B        3           ;for printing out decimal numbers
NUM2   DS.B        3
ANS    DS.B        3
reverse DS.B     6
msg1        DC.B        's: Set time', $00
msg2        DC.B        'q: Stop clock and calculator, start Typewrite program', $00                          
     


ctr2p5m     DS.W   1                 ; interrupt counter for 2.5 mSec. of time
times       DS.B   1




;*******************************************************
; interrupt vector section
            ORG    $FFF0             ; RTI interrupt vector setup for the simulator
;            ORG    $3FF0             ; RTI interrupt vector setup for the CSM-12C128 board
            DC.W   rtiisr

;*******************************************************
; code section

            ORG    $3100
Entry
            LDS    #Entry         ; initialize the stack pointer

            LDAA   #%11111111   ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA   DDRB         ; as output
            STAA   PORTB        ; set all bits of PORTB, initialize

            ldaa   #$0C         ; Enable SCI port Tx and Rx units
            staa   SCICR2       ; disable SCI interrupts

            ldd    #$0001       ; Set SCI Baud Register = $0001 => 2M baud at 24MHz (for simulation)
;            ldd    #$0002       ; Set SCI Baud Register = $0002 => 1M baud at 24MHz
;            ldd    #$000D       ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd    #$009C       ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std    SCIBDH       ; SCI port baud rate change

            ldx    #msg1          ; print the first message, 'Hello'
            jsr    printmsg
            jsr    nextline

            ldx    #msg2          ; print the second message
            jsr    printmsg
            jsr    nextline
            
            bset   RTICTL,%00011001 ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                    ;      4MHz quartz oscillator clock
            bset   CRGINT,%10000000 ; enable RTI interrupt
            bset   CRGFLG,%10000000 ; clear RTI IF (Interrupt Flag)


            ldx    #0
            stx    ctr2p5m          ; initialize interrupt counter with 0.
            cli                     ; enable interrupt, global


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
            ldab times
            cmpb #59
            lbhi resettimes
          
            jsr    LEDtoggle        ; if 0.5 second is up, toggle the LED 

            jsr    getchar          ; type writer - check the key board
            tsta                    ;  if nothing typed, keep checking
            beq    looop
            
             STAA  1,X+               ;increase x counter store in a
            INC   CCount             ;increase ccount
            jsr   putchar            ;jump to putchar subroutine
            
                                    ;  otherwise - what is typed on key board
            cmpa #$0D           ;check for enter key
            beq enterkeypressed     ;if enter key is pressed jump to buff initialization            
            ldab   CCount       ; load count into accumulator B
            cmpb   #9           ; compare count to 5
            lBGE    errorthrow  ; if count is greater or equal to 5 jump to buff initialize
            
    
            
            
            bra    looop


;subroutine section below

;**************************enterkeypressed******************************
enterkeypressed
            psha                    ;push a
            pshb                    ;push b
            LDAB $3000               ;load memory 3000
            cmpb #$73                ;is it s?
            lbeq skeypress           ;jump to skey press
            LDAB $3000               ;load memory 3000
            cmpb #$71                ;is it q?
            lbeq qkeypress           ;jump to q key
           
           
           
           LDAB $3000               ;load memory 3000
           cmpb #$39                             
           lBHI  errorthrow2        ;throw error if number bigger than 9              
           jsr  convb               ;perform conversion
           STAB $3000
           LDAB $3001
                      cmpb #$39                             
           lBHI  errorthrow2 
           jsr  convb
           STAB $3001
           LDAB $3002
                      cmpb #$39                             
           lBHI  errorthrow2 
           jsr  convb
           STAB $3002
           
           
           LDAB $3004
                      cmpb #$39                             
           lBHI  errorthrow2 
           jsr  convb
           STAB $3004
           LDAB $3005
                      cmpb #$39                             
           lBHI  errorthrow2 
           jsr  convb
           STAB $3005
           LDAB $3006
                      cmpb #$39                             
           lBHI  errorthrow2 
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






;**************************skeypress******************************

skeypress   
            LDAB $3001                 ;load 3001
            cmpb #$20                  ;is there a space?
            lbne errorthrow2           ;if not throw error
            LDAB $3003                 ; is there an enter?
            cmpb #$0D
            lbeq  skeypress1           ;jump to first digit
            LDAB $3004                 ;is there an enter?
            cmpb #$0D                  
            lbeq  skeypress2           ;jump to 2 digits
            jsr errorthrow2


skeypress1
           LDAB $3002
           jsr convbtime                ;convert to hex
           jsr HEX2BCD                  ;convert to bcd
           STAB times                   ;store in times
           STAB PORTB                   ;show on lcd
           jsr buffinitial
           
           
skeypress2
           clra
           LDAB  $3002                 
           jsr   convbtime             ;convert to hex
           STAB  $3002                 
           LDAB  $3003
           jsr   convbtime
           STAB  $3003
           
           LDAB  $3002
           LDY   #$A
           EMUL
           ADDB  $3003
           cmpb #59                     ;if biger than 59 error
           lbhi errorthrow2             
           jsr HEX2BCD                  ;convert to bcd
           STAB PORTB                   ;store in port b
           jsr BCD2HEX                  ;bcd to hex
           STAB times                   ;store in times
        
           jsr buffinitial
                      

;**************************errorthrow******************************

convbtime
         
         
          cmpb #$30                            ;is num les than 0?
          lBLO  errorthrow2                     ;jump to error if so
          cmpb  #$39                           ;is num more than 9?
          lBHI  errorthrow2                     ;jump to more than 9 sub
          subb #$30                             ; else add 30
          rts


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
 



;***********add subrountine***************************
adds


           jsr storenum1
           jsr storenum2

           LDD $300E                     ;load num1 location
           ADDD $3011                    ; add to num2 location
           STD ANS                       ;store in ans
           jsr printans                  ;jump to print ans subroutine
             
           
;****************************subtract subroutine*********************************************            
             
subs
                                     
           jsr storenum1
           jsr storenum2
           
           LDD $300E                    ;load num1 location
           SUBD $3011                   ;subtract from num2 location
           cmpa #128                    ;see if number is bigger than 1000 0000
           LBHS negativenum             ; if it is bigger jump to negative num sum roubtine
           STD ANS                      ;otherwise  store ans 
           jsr printans                 ;jump to print ans subroutine
;*************************multiply roubtien******************************************

muls
           jsr storenum1
           jsr storenum2

           LDD $300E                         ;load num1
           LDY $3011                         ;load num2
           EMUL                              ;multiply
           cpy #00                           ;if there is an overflow (y has something in its register)
           LBHI errorthrow3                  ;throw overflow error
           STD ANS                           ;other wise go to ans
       
           jsr printans                      ;go to print ans
;*****************************divide subroutine****************************************

divis
           jsr storenum1
           jsr storenum2
            
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
qkeypress   LDAA #%00010000
            TFR A,CCR
            

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
           


storenum1                            ;store num into memory
           clra
           LDAB $3000              ;convert it to hex
           LDY #$A
           EMUL
           ADDB $3001
           LDY #$A
           EMUL
           cmpb #$FA
           lbhs  twofiftysix        ;special case for 256-259
           
           ADDB $3002
           
          STD NUM1                 ;store in num1
          rts
          
storenum2
           clra
           LDAB $3004
           LDY #$A
           EMUL
           ADDB $3005
           LDY #$A
           EMUL
           cmpb #$FA
           lbhs twofiftysixb         ;special case for 256
           ADDB $3006
           STD NUM2                  ;store in num2
           rts
 ;*************************************************************************
 
twofiftysix                         ;#256-259 error correction  for num1
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

twofiftysixb                           ;#256-259 error correction  for num2
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

 

;************************************************************************
errorthrow  
            pshb                    ;error throw loop
            ldab   CCount       ;load count into register B
            cmpb   #9           ;compare if b is equal to 5
            BGE    errorthrowmsg ;if greater than 5 or same jump to errorwthrow msg
            pulb
            rts                  ;return back to previous loop
             


errorthrowmsg                  ;error throw msg

            ;ldx   #msg10       ;load msg10 into register x      
            ;jsr   printmsg     ; jump to print msg subroutine
            ;jsr   nextline     ;jump to nextline subroutine
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

;***********RTI interrupt service routine***************
rtiisr      bset   CRGFLG,%10000000 ; clear RTI Interrupt Flag - for the next one
            ldx    ctr2p5m          ; every time the RTI occur, increase
            inx                     ;    the 16bit interrupt count
            stx    ctr2p5m
rtidone     RTI
;***********end of RTI interrupt service routine********

;***************LEDtoggle**********************
;* Program: toggle LED if 0.5 second is up
;* Input:   ctr2p5m variable
;* Output:  ctr2p5m variable and LED1
;* Registers modified: CCR
;* Algorithm:
;    Check for 0.5 second passed
;      if not 0.5 second yet, just pass
;      if 0.5 second has reached, then toggle LED and reset ctr2p5m
;**********************************************
LEDtoggle   psha
            pshx
            pshy


            ldx    ctr2p5m          ; check for 1 sec
;            cpx    #200             ; 2.5msec * 400 = 1 sec
            cpx    #200             ; 2.5msec * 400 = 1 sec
            
 
            
            blo    doneLED          ; NOT yet

            ldx    #0               ; 1 sec is up,
            stx    ctr2p5m          ;     clear counter to restart
        
            
            LDAB   times  ;PORTB
            jsr  HEX2BCD
            STAB   PORTB

           
            INC    times


doneLED     puly
            pulx
            pula
            rts
;***************end of LEDtoggle***************

resettimes
          ldab #$00
          stab times
          jsr looop

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
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                bsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
                pula
                rts
;***********end of printmsg********************
HEX2BCD                          ;turn hex number to bcd
        TFR    B,A               ;transfer b to a
UP      CMPB   #10               ; see if its 10
        BLO    DONE2             ;if lower go to done2
        SUBB   #10               ;subtract 10 from b
        ADDA   #6                ;add 6 to a
        BRA    UP                ;repeat
DONE2
        TFR   A,B                ;transfer a to b
        RTS

BCD2HEX                          ;turn bcd 2 hex
        TFR    B,A               ;transfer b to a 
DOWN      CMPB   #16             ;see if number iss 16
        BLO    DONE3             ;if lower then done
        SUBB   #16               ;subtract 16 from b
        SUBA   #6                ; add 6 to a
        BRA    DOWN              ;repeat
DONE3
        TFR   A,B                ;transfer a to b
        RTS



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
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
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

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    psha
            ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            pula
            rts
;****************end of nextline***************
msg10        DC.B        'invalid input', $00
msg11       DC.B    'input not allowed' , $00
msg12       DC.B    'overflow error' , $00
msg13       DC.B    'Tcalc>',$00
            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled

