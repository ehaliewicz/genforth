#ifndef IO_H
#define IO_H

#include "genesis.h"
#include "string.h"
#include "vdp.h"

#define WIDTH_TILES 38
#define HEIGHT_TILES 28
#define HEIGHT_INPUT_NO_OSK 28
#define HEIGHT_INPUT_OSK 17

/// first ascii character we give a shit about
#define CHAR_START 32

void initIO();
void clearScreen();
void incPrintRow();
void incPrintCol();
void get_line_of_input();
void printChar(char c);
void printStrn(char* str, u8 strLen);
void printStrNewline(char* str, u8 strLen);

#endif
