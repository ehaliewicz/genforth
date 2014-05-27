#include "genesis.h"
#include "string.h"
#include "vdp.h"
#include "io.h"


extern char buffer[WIDTH_TILES];
extern u32 *bufftop, *curkey;

static int printCol, printRow;



void initIO() {

  VDP_init();
  JOY_init();
  printCol = 1;
  printRow = 0;
  
}

void clearScreen() {
  for(int i = 0; i < HEIGHT_TILES; i++) {
    VDP_clearTextLine(i);
  }
}

void scrollUp() {

}

void scrollDown() {

}

void incPrintRow() {
  printRow++;
  printCol = 1;
  if(printRow >= HEIGHT_TILES) {
    clearScreen();
    printRow = 0;
    printCol = 1;
  }
  
}

void incPrintCol() {
  printCol++;
  if(printCol >= WIDTH_TILES+1) {
    
    incPrintRow();
    
  }
}

static int cursorCnt = 0;
int printCursor() {
  int retval = 0;
  
  cursorCnt++;
  
  if(cursorCnt >= 20 && cursorCnt <= 40) {
    retval = 1;
  }

  if(cursorCnt > 40) {
    cursorCnt = 0;
  }
  return retval;
}

char charUpcase(char c) {
  if((c >= 97) & (c <= 122)) {
    return c-32;
  } else {
    return c;
  }
}

char charDowncase(char c) {
  if((c >= 41) & (c <= 90)) {
    return c+32;
  } else {
    return c;
  }
}

void get_line_of_input() {
  
  incPrintRow();

  // reset buffer
  for(int i = 0; i < WIDTH_TILES-1; i++) {
    buffer[i] = ' ';
  }

  /* buffer[0] = ':'; */
  /* buffer[1] = ' '; */
  /* buffer[2] = 'S'; */
  /* buffer[3] = 'Q'; */
  /* buffer[4] = ' '; */
  /* buffer[5] = 'D'; */
  /* buffer[6] = 'U'; */
  /* buffer[7] = 'P'; */
  /* buffer[7] = ' '; */
  /* buffer[7] = '2'; */
  /* buffer[7] = ' '; */
  /* buffer[7] = '*'; */
  /* buffer[7] = ' '; */
  /* buffer[7] = ';'; */
  
  
  
  /* bufftop = (u32*)(buffer+WIDTH_TILES); */
  /* curkey  = (u32*)(buffer); */
  /* return; */
  
  char tmpBuffer[WIDTH_TILES+1];
  for(int i = 0; i < WIDTH_TILES; i++) {
    tmpBuffer[i] = ' ';
  }
  tmpBuffer[0] = 'F';
  tmpBuffer[1] = 'O';
  tmpBuffer[2] = 'R';
  tmpBuffer[3] = 'T';
  tmpBuffer[4] = 'H';
  tmpBuffer[5] = '>';
  tmpBuffer[6] = ' ';

  tmpBuffer[WIDTH_TILES] = '\0';
  
  int minPtr = 7;
  int ptr = minPtr;

  void incPtr(){
    ptr++;
    if (ptr >= WIDTH_TILES) {
      ptr = minPtr;
    }
  }

  void decPtr() {
    ptr--;
    if(ptr < minPtr) {
      ptr = WIDTH_TILES-1;
    }
  }
  
  void shiftBack(int start) {
    for(int i = start; i < WIDTH_TILES-1; i++) {
      tmpBuffer[i] = tmpBuffer[i+1];
    }
  } 
  
  void shiftForward(int start) {
    for(int i = WIDTH_TILES-2; i >= start; i--) {
      tmpBuffer[i+1] = tmpBuffer[i];
    }
  }


  u16 lastState = JOY_readJoypad(JOY_1);
  //int read = 0;
  while(1) {
    VDP_waitVSync();
    u16 state = JOY_readJoypad(JOY_1);
    
    u16 diff = (state ^ lastState) & state;
    /* if (read >= 1 && read < 10) { */
    /*   read++; */
    /*   continue\; */
    /* } else { */
    /*   read = 0; */
    /* } */

    if(diff & BUTTON_LEFT) {
      
      //read = 1;
      decPtr();
      cursorCnt = 20;

    } else if (diff & BUTTON_RIGHT) {
      //read = 1;
      incPtr();
      cursorCnt = 20;
    }

    else if (diff & BUTTON_UP) {
      //read = 1;
      tmpBuffer[ptr]++;
      cursorCnt = 20;    
    }

    else if (diff & BUTTON_DOWN) {
      //read = 1;
      tmpBuffer[ptr]--;
      cursorCnt = 20;       
    }
    
    if (diff & BUTTON_START) {
      break;
    }
    
    if(diff & BUTTON_A) {
      //read = 1;
      tmpBuffer[ptr] = 'A'; 
      cursorCnt = 20;        
    }
    
    if(diff & BUTTON_B) {
      //read = 1;
      shiftBack(ptr-1);
      decPtr();
    }

    if(diff & BUTTON_C) {
      //read = 1;
      shiftForward(ptr);
      tmpBuffer[ptr] = ' ';
      incPtr();
    }
    
    VDP_clearTextLine(printRow+1);
    VDP_drawText(&tmpBuffer[0], 1, printRow);
    
    if(printCursor()) {
      VDP_drawText("-", ptr+1, printRow+1);
    }
    
    lastState = state;
  }
  
  VDP_clearTextLine(printRow+1);
  incPrintRow();

  // skip the null at the end
  for(int i = minPtr; i < WIDTH_TILES; i++) {
    buffer[i-minPtr+1] = charUpcase(tmpBuffer[i]);
  }
  
  
  bufftop = (u32*)(buffer+WIDTH_TILES-1);
  curkey  = (u32*)(buffer);
  
}




void printChar(char c) {

  if (c == '\n') {
    incPrintRow();
  } else {
    char str[2];
    str[0] = c;
    str[1] = '\0';
    
    VDP_drawText(str, printCol, printRow);
    
    incPrintCol();
  }
}

void printStrn(char* str, u8 strLen) {
  incPrintRow();
  for(int i = 0; i < strLen; i++) {
    printChar(str[i]);
  }
}

void printStrNewline(char* str, u8 strLen) {
  // if string will wrap-around, print newline first
  if(printCol+strLen >= WIDTH_TILES+1) {
    incPrintRow();
  }
  printStrn(str, strLen);
}
