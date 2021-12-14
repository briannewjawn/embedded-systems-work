/*
 * File:   EE200_LCD.c
 * Author: brian
 *
 * Created on November 14, 2020, 5:18 PM
 */

#define FCY 3685000UL
#include <p33EP64MC502.h>
#include <libpic30.h>
#include <string.h>

#define pgm_delay_ms 2
#define INSTRUCTION 0
#define DATA 1

void initialize_LCD_DIO_ports (void);
void Toggle_Enable_line (void);
void Write_LCD_Nibble(int data, int cmd);
void Write_LCD_Byte(int data, int cmd);


void initialize_LCD_DIO_ports (void) {
    ANSELB = 0; //set all Port B  bits to  digital
    TRISBbits.TRISB8 = 0; //set  tris b bit 8-12 to output
    TRISBbits.TRISB9 = 0;
    TRISBbits.TRISB10 = 0;
    TRISBbits.TRISB11 = 0;
    TRISBbits.TRISB12 = 0;
    TRISBbits.TRISB13 = 0;
    
    PORTBbits.RB8 = 0; //set  port bit 8-12  low
    PORTBbits.RB9 = 0;
    PORTBbits.RB10 = 0;
    PORTBbits.RB11 = 0;
    PORTBbits.RB12 = 0;
    
}

void Toggle_Enable_line (void) {
    __delay_ms(pgm_delay_ms);   // delay
    PORTBbits.RB13 = 1;         // set E high
    __delay_ms(pgm_delay_ms);   // delay
    PORTBbits.RB13 = 0;         // set E low
    __delay_ms(pgm_delay_ms);   // delay
}

void Write_LCD_Nibble(int data, int cmd) {
 int x;
 x =  PORTB & 0xF0FF; //clock
 PORTB = x +(data <<8);

 PORTBbits.RB12 = cmd;
 Toggle_Enable_line (); //initialize  enable line
 PORTBbits.RB14 = 0; //port 14 low
 PORTBbits.RB15 = 0; // port 15 low
 
 
}

void Write_LCD_Byte(int data, int cmd) {
    Write_LCD_Nibble((data & 0x00F0) >> 4, cmd); // write upper nibble
    Write_LCD_Nibble( data & 0x000F, cmd);       // write lower nibble
}

void Init_LCD_Module(void) {
    initialize_LCD_DIO_ports();
    Write_LCD_Nibble(0b0011, INSTRUCTION);  // Initialize the LCD Module
    Write_LCD_Nibble(0b0011, INSTRUCTION);
    Write_LCD_Nibble(0b0011, INSTRUCTION);
    Write_LCD_Nibble(0b0010, INSTRUCTION);  // invoke 4-bit mode
    Write_LCD_Byte(0b00101000, INSTRUCTION);// 4-bit mode, two-line,5X7 dot
    Write_LCD_Byte(0b00000001, INSTRUCTION);// clear display, cursor at 0x00
    Write_LCD_Byte(0b00001100, INSTRUCTION);// display on,cursor blink/underline
}

void Position_LCD_Cursor(int cell_num){
    Write_LCD_Byte(0x80 + cell_num, INSTRUCTION); //cursor
}

void Write_LCD_String(char char_Array[16]){
    int idx;
    for (idx = 0; idx<strlen(char_Array); idx++) //write to LCD
        Write_LCD_Byte(char_Array[idx], DATA);
        
}