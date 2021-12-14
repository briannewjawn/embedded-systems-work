/*
 * File:   Project3.c
 * Author: brian
 *
 * Created on November 14, 2020, 5:16 PM
 */

#define FCY 3685000UL
#include <p33EP64MC502.h>
#include <libpic30.h>
#include <stdio.h>
#include <xc.h>
#include <string.h>
#include "EE200_LCD.h"

#pragma config ICS = PGD2 // Communicate on PGED2 (pin 14) and PGEC2 (pin 15)
#pragma config JTAGEN = OFF

/* declare functions */
void initialize_IO_ports (void);
void initialize_INT(void);
void __attribute__((__interrupt__,auto_psv)) _INT1Interrupt (void);

int counter = 0; //set counter  as global variable  for reset purposes
char clock[16]; //set clock character

typedef enum {
    S0, S1, S2, S3 //define states
} STATES;
STATES state = S0; //default state

void initialize_IO_ports (void) {
   
    ANSELA= 0;           //set a bit to digital
    ANSELB = 0;         //set b b it to  digital
   
    /* Set I/O pin direction */
    TRISAbits.TRISA0 = 1;  //set  L to  input            
    TRISAbits.TRISA1 = 1;  //set U  to input
    TRISAbits.TRISA2 = 1; //set S to input
    TRISBbits.TRISB4 = 1; //set R to input
    TRISBbits.TRISB0 = 0; //set DD to output
    TRISBbits.TRISB1 = 0; //set AD to output
    TRISBbits.TRISB2 = 0; //SET SD to output
    TRISBbits.TRISB3 = 0; //set clock to output

  }
void initialize_INT(void){
    RPINR0bits.INT1R = 0x24; //SET int1r to pin 11 10 0010
    INTCON2bits.GIE = 1; // set global interrupt
    INTCON2bits.INT1EP = 1; //imnterrupt on negative edge
    IFS1bits.INT1IF = 0; // clear interrupt flag
    IEC1bits.INT1IE =1; //enable interrupt
           

}
void __attribute__((__interrupt__,auto_psv)) _INT1Interrupt (void){ //interrupt command
    LATBbits.LATB0 = 1; // flash  DD on
    LATBbits.LATB1 = 1; // FLASH AD on
    LATBbits.LATB2 = 1; // flash SD on
    state = S0; //SET STATE 0
    Init_LCD_Module(); //Initialize lCD MODULE
    Position_LCD_Cursor(0x00); // put cursor  at  position 0x00
    Write_LCD_String("STATE = 0"); // SET STATE to 0
    counter =0; // SET COUNTER TO  0
    Position_LCD_Cursor(0x40);  //PUT CURSOR AT position 0x40
    Write_LCD_String(clock);  // WRITE TO CLOCK       
            
    IFS1bits.INT1IF =0; //CLEAR  INTERRUPT
}



int main(void) { // main loop
    //STATES state = S0; // set initial state to S0

    int input, L, U, S;
    
   
    char S0_write[] = "STATES = S0"; //assign string to write for S0
    char S1_write[] = "STATES = S1"; //assign string to write for S1
    char S2_write[] = "STATES = S2"; //assign string to write for S2
    char S3_write[] = "STATES = S3"; //assign string to write for S3
  
    sprintf(clock, "Counter = %3d", counter);  //sprint f to counter
    Position_LCD_Cursor(0x40); // move cursor to 0x40
    Write_LCD_String(clock); //write clock 

    initialize_IO_ports(); //intialize io ports
    Init_LCD_Module(); //intialize lcd module
    initialize_INT(); //initialize int 
    
        while (1) {  // while loop
        LATBbits.LATB3 = 1;    
        L = (PORTAbits.RA0 ^1) << 2; // multiple by 2^2
        U = (PORTAbits.RA1 ^1) << 1; //multiple by 2^1
        S = PORTAbits.RA2 ^1; //multiple by s^0
       
        input = L +  U + S;   //set input to  add up to  7

        switch (state) {
            case S0:
                switch (input) {
                    case 0: state = S0;break;
                    case 1: state = S0;break;
                    case 3: state = S0;break;
                    case 2: state = S1;break; // U is pressed next state  is s1
                    case 4: state = S0;break;
                    case 5: state = S0;break;
                    case 6: state = S0;break;
                    case 7: state = S0;break;
                    
                }; break;
            case S1:
                switch (input) {
                    case 0: state = S1;break;
                    case 1: state = S1;break;
                    case 3: state = S1;break;
                    case 2: state = S2;break; // U is pressed next state  is s2
                    case 4: state = S0;break; // L is pressed next state  is s0 for case 4-7
                    case 5: state = S0;break;
                    case 6: state = S0;break;
                    case 7: state = S0;break;
                };  break;
            case S2:
                switch (input) {
                    case 0: state = S2;break;
                    case 2: state = S2;break;
                    case 3: state = S2;break; 
                    case 1: state = S3;break; // S is pressed next state  is s3
                    case 4: state = S0;break; // L is pressed next state  is s0 for case 4-7
                    case 5: state = S0;break;
                    case 6: state = S0;break;
                    case 7: state = S0;break;
                };  break;
            case S3:
                switch (input) {
                    case 0: state = S3;break;
                    case 2: state = S3;break;
                    case 3: state = S3;break;
                    case 1: state = S2;break; // S is pressed next state  is s2
                    case 4: state = S0;break; // L is pressed next state  is s0 for case 4-7
                    case 5: state = S0;break;
                    case 6: state = S0;break;
                    case 7: state = S0;break;
                };  break;
        }
        Position_LCD_Cursor(0x00); //position cursor to 0x00
        switch (state) {// set output
            case S0:
                LATBbits.LATB0 = 0; // DD led off
                LATBbits.LATB1 = 0; // AD led off
                LATBbits.LATB2 = 0; // SD led off
                Write_LCD_String(S0_write); //write state  0 string to lcd
                
         
            break;
            case S1:
                LATBbits.LATB0 = 1; // DD led on  
                LATBbits.LATB1 = 0; // AD led off
                LATBbits.LATB2 = 0; // AD led off
                Write_LCD_String(S1_write); //write state  1 string to lcd

            break;
            case S2:
                LATBbits.LATB0 = 1; // DD led on
                LATBbits.LATB1 = 1; // AD led on
                LATBbits.LATB2 = 0; // SD  led off
                Write_LCD_String(S2_write); //write state  2 string to lcd

            break;
            case S3:
                LATBbits.LATB0 = 1; // DD led on
                LATBbits.LATB1 = 1; // AD led on
                LATBbits.LATB2 = 1; // SD  led on
                Write_LCD_String(S3_write);  //write state  3 string to lcd
            break;    
            
        }
        counter ++; //counter + 1
        sprintf(clock, "Counter = %3d", counter);
        Position_LCD_Cursor(0x40); // position the counter
        Write_LCD_String(clock); // write clock 
        __delay_ms(1500); // delay 1.5 sec
        LATBbits.LATB3 = 0; // turn off Clock
        __delay_ms(1500); // delay 1.5 sec
        
        
        ClrWdt();
    }
    return 0;
}