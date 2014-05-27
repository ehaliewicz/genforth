#include "types.h"
#include "genesis.h"
#include "vdp.h"
#include "sys.h"
#include "string.h"


u32 *stackPointer, *rStackPointer;
u32 tosDump, torsDump;


void printPtr(u32* ptr, int y, int isPtr) {
  char str[11];
  str[10] = '\0';
  int lastLen;
  
  intToHex((u32)ptr, str, 8);
  
  // top of parameter stack isn't a ptr,
  // as it's buffered in a register
  if(isPtr) {
    VDP_drawText(str, 0, y);
  }
  lastLen = strlen(str);
  strclr(str);
    
  VDP_drawText(":", lastLen, y);
  lastLen += 1;
    
  u32 hex = *ptr;
  intToHex(hex, str, 8);
  VDP_drawText("0x",lastLen+1, y);
  VDP_drawText(str, lastLen+3, y);
  lastLen += (strlen(str)+3);
    
    
  strclr(str);
 
    
  uintToStr(hex, str, 10);
  VDP_drawText(", ", lastLen, y);
  VDP_drawText(str, lastLen+2, y);
  lastLen = lastLen+2+strlen(str);
    
    
  VDP_drawText(", \"", lastLen, y);
    
  char chars[2];
  chars[1] = '\0';
  chars[0] = ((hex & 0xFF000000) >> 24);
  VDP_drawText(chars, lastLen+3, y);
  chars[0] = ((hex & 0x00FF0000) >> 16);
  VDP_drawText(chars, lastLen+4, y);
  chars[0] = ((hex & 0x0000FF00) >>  8);
  VDP_drawText(chars, lastLen+5, y);
  chars[0] = (hex & 0x000000FF);
  VDP_drawText(chars, lastLen+6, y);
    
  VDP_drawText("\"", lastLen+7, y);
    
}

void printStack(u32* sp, u32 tos, int y, char* stackName, int topIsPtr) {
  
  
 
  
  VDP_drawText("Top of ", 0, y-1);
  VDP_drawText(stackName, 7, y-1);
  printPtr(&tos, y, topIsPtr);
  y++;
  for(int i = 0; i < 9; i++) {
    printPtr(sp, y, 1);
    sp++;
    y++;
  }
  
}



void stackError(char* errorMsg) {
  VDP_init();
  VDP_drawText(errorMsg, 9, 0);
  
  int y = 3;
  printStack(stackPointer, tosDump, y, "param stack", 0);
  printStack(rStackPointer, torsDump, y+14, "return stack", 1);
  
  
  while(1) { }  
}


void stackOverflowError()
{
  stackError("STACK OVERFLOW!");
}


void stackUnderflowError() {
  stackError("STACK UNDERFLOW!"); 
}


void interpParseError() 
{
  VDP_init();
  VDP_drawText("INTERPRETER PARSE ERROR!", 7, 0);
  
  while(1) { }
  
}
