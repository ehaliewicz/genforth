#include "genesis.h"
#include "string.h"
#include "vdp.h"
#include "io.h"
#include "KDebug.h"


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
  for(int i = 0; i < HEIGHT_INPUT; i++) {
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
  if(printRow >= HEIGHT_INPUT) {
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


extern u32 **loc_latest;
static u32 *cur_entry;
static int prefix_cnt, failed_match, clear_cnt;


void resetPrefixSearch() {
  cur_entry = *loc_latest;
  failed_match = 0;
  prefix_cnt = 0;
  clear_cnt = 0;
}



// returns a pointer to the next entry that matches the given prefix
u32* prefixMatch(char* word, u32 wordLen) {
  u8 nameLen;
  char* entryWord;

  
 check_match:
  nameLen = *(((u8*)cur_entry)+4);
  
  if(nameLen < wordLen) {
    // continue onto next entry
    goto get_next;
  } else {
    entryWord = ((u8*)cur_entry)+5;
    // try to match characters
    for(int i = 0; i < wordLen; i++) {
      if(entryWord[i] != word[i]) {
        goto get_next;
      }
    }
    u32* res_entry = cur_entry;
    cur_entry = *((u32**)cur_entry);
    return res_entry;
    
  }
  
 get_next:
  // dereference link
  cur_entry = *((u32**)cur_entry);
  if(cur_entry) {
    goto check_match; 
  } else {
    goto no_match;
  }
  
 no_match:
  failed_match = 1;
  return NULL;

}

/* int oskX, oskY; */
/* void drawOsk() { */
/*   u8 ascii_cnt = 33; */
/*   char str[2] = {' ', '\0'}; */
/*   for(int y = HEIGHT_INPUT+1; y < HEIGHT_TILES; y+=2) { */
/*     for(int x = 0; x < 40; x+=2) { */
/*       str[0] = ascii_cnt++; */
/*       VDP_drawText(str,x,y); */
/*       if(ascii_cnt >= 128) { return; } */
/*     } */
/*   } */
  
/* } */




static int defMode;

static int initialized=0;
static int used = 0;

void get_line_of_input() {
  
  if(initialized == 1) {
    printStrn("ok.", 3);
  } else {
    initialized = 1;
  }

  defMode = 0;
  incPrintRow();

  // reset buffer
  for(int i = 0; i < WIDTH_TILES-1; i++) {
    buffer[i] = ' ';
  }
  if(used == 0) {

  buffer[0] = '3';
  buffer[1] = '3';
  buffer[2] = ' ';
  buffer[3] = ' ';
  buffer[4] = ' ';
  buffer[5] = ' ';
  buffer[6] = ' ';
  buffer[7] = ' ';
  
  
  
  bufftop = (u32*)(buffer+WIDTH_TILES);
  curkey  = (u32*)(buffer);
  used = 1;
  return;
  }

  char tmpBuffer[WIDTH_TILES+1];
  for(int i = 0; i < WIDTH_TILES; i++) {
    tmpBuffer[i] = ' ';
  }

  tmpBuffer[WIDTH_TILES] = '\0';
  
  int minPtr = 0;
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
  
  while(1) {
    VDP_waitVSync();
    u16 state = JOY_readJoypad(JOY_1);
    
    u16 diff = (state ^ lastState) & state;

    if(diff & BUTTON_LEFT) {
      
      //read = 1;
      decPtr();
      cursorCnt = 20;
      resetPrefixSearch();
      
    } else if (diff & BUTTON_RIGHT) {
      //read = 1;
      incPtr();
      cursorCnt = 20;
      resetPrefixSearch();
    }

    else if (diff & BUTTON_UP) {
      //read = 1;
      tmpBuffer[ptr]++;
      cursorCnt = 20;    
      resetPrefixSearch();
    }

    else if (diff & BUTTON_DOWN) {
      //read = 1;
      tmpBuffer[ptr]--;
      cursorCnt = 20;
      resetPrefixSearch();
    }
    
    if (diff & BUTTON_MODE) {
      switch (defMode) {
      case 0:
        shiftForward(ptr);
        tmpBuffer[ptr++] = ':';
        defMode++;
        break;
      case 1:
        tmpBuffer[ptr++] = ';';
        defMode++;
        break;
      }
    }
    
    if (diff & BUTTON_START) {
      resetPrefixSearch();
      break;
    }
    
    if(diff & BUTTON_A) {
      //read = 1;
      tmpBuffer[ptr] = 'A'; 
      cursorCnt = 20;     
      resetPrefixSearch();
    }
    
    if(diff & BUTTON_B) {
      //read = 1;
      shiftBack(ptr-1);

      char delChar = tmpBuffer[ptr-1];
      if(delChar == ':') {
        defMode = 0;
      } else if (delChar == ';') {
        defMode = 1;
      }
      decPtr();
      resetPrefixSearch();
    }

    if(diff & BUTTON_C) {
      //read = 1;
      shiftForward(ptr);
      tmpBuffer[ptr] = ' ';
      incPtr();
      resetPrefixSearch();
    }

    if(diff & BUTTON_X) {  

      int cnt = 0;
      
      if(tmpBuffer[ptr] == ' ') {
        continue;
      }
      
    
      
      while(1) {
        if(tmpBuffer[ptr+cnt] == ' ') {
          break;
        }
        cnt++;
      }
      if(prefix_cnt == 0) {
        prefix_cnt = cnt;
      }
      u32* res = prefixMatch(tmpBuffer+ptr, prefix_cnt);
      if(res) {
        u8 entryWordLen = *(((u8*)res)+4);
        char* entryWord = ((u8*)res)+5;
        if(entryWordLen > clear_cnt) {
          clear_cnt = entryWordLen;
        }
        for(int i = 0; i < clear_cnt; i++) {
          tmpBuffer[ptr+i] = ' ';
        }
        for(int i = 0; i < entryWordLen; i++) {
          //shiftForward(ptr+i+1);
          tmpBuffer[ptr+i] = entryWord[i];
        }
      }
    }

    if(state & BUTTON_Y) {
      //drawOsk();
      continue;
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
