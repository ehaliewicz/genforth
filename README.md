megaforth
---------
A 68000 Forth designed for the Sega Megadrive.


Currently, it requires the SGDK and only builds on Windows.



**Controls (6-button isn't required but helps):**
* A - Set character at cursor to A 
* B - Delete character in front of cursor 
* C - Insert space at cursor 
* X - show possible words based on prefix starting where the cursor is pointing (shows one suggestion per button press) 
* Y - enable on-screen keyboard
* Z - hold to allow left-right movement in keyboard mode. 
* Mode - Starts or completes word definitions by inserting ";" or ":" based on state. This is done on a line-by-line basis, it doesn't keep track of the state of the interpreter. Don't use with multi-line definitions. 
* Start - send line to interpreter


**Building (requires GNU make):**

make -f SGDK/makefile.gen
