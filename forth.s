        | pushes a value onto the return stack
        | which is buffered to have the top value in d0
        | this macro also does bounds checking
        | the stack grows down, so we use pre-decrement addressing
        .macro PUSH reg
        cmpa.l #ps_overflow, %a6
        blt stackOverflow
        move.l %d0, -(%a6)
        move.l \reg, %d0
        .endm

        | same as above, but in the reverse order
        | moves the top value of the stack in d0
        | to the given register
        | and pops the a6 register up by 4 bytes, placing what it previously pointed to in d0
        .macro POP reg
        cmpa.l #ps_end, %a6
        bgt stackUnderflow
        move.l %d0, \reg
        move.l (%a6)+, %d0
        .endm

        | convenience macros for pushing and popping onto the native (return) stack
        .macro RPOP reg
        move.l (%a7)+, \reg
        .endm

        .macro RPUSH reg
        move.l \reg, -(%a7)
        .endm


    
        .text
        .globl main, get_line_of_input, buffer, bufftop, curkey
        .extern stackOverflowError, stackPointer, rStackPointer, tosDump, torsDump, printChar, initIO, printNewline

        | start of ROM
main:
        | well, we don't really need ROM yet so jump to RAM ;)
        jmp ram_start
        


        

        .data
ram_start:
        | initialize I/O stuff (in io.c)
        jsr initIO

        | init TOS (top of stack)
        moveq.l #0, %d0 
        
        | set up parameter stack
        move.l (ps_end), %a6

        
        | init 'latest' var (stores pointer to last defined word in dictionary)
        PUSH #LAST_WORD
        jsr latest
        jsr store

        | init 'here' var (stores pointer to next word after dictionary, used for compiling new code or dictionary headers)
        PUSH #herePtr
        jsr here
        jsr store
        | read a word from input ( -- strAddr strLen )
        jsr word

        jsr number                      | ( strAddr strLen -- parsedNumber #unparsedChars) 
        jsr dot                         | ( value --  ) takes the top value on the stack and prints it according to the 'BASE' variable  
        jsr dot
back:           
        jmp back


        | %d0 is top of stack
        | %a6 is rest of param stack
        | %a7 is return stack


        | DEBUG     EQU 1       
        .equ F_IMMED,  0x80
        .equ F_HIDDEN, 0x20
        .equ F_LENMASK, 0x1F
        .equ PARAM_STACK_SIZE, 128

        .set LAST_WORD, 0
        .set LAST_WORD_2, 0
        .set HERE_PTR, 0

        

        .macro DEFWORD name, nameLen, label, flags
        .set LAST_WORD_2, .
        dc.l LAST_WORD
        .set LAST_WORD, LAST_WORD_2
        .byte \nameLen+\flags
        .ascii "\name"
        .align 2
\label:
        .endm


        .macro DEFVAR name, nameLen, label, flags, val
loc_\label:
        dc.l \val
        DEFWORD \name, \nameLen, \label, \flags
        PUSH #loc_\label
        rts
        .endm

        .macro DEFCONST name, nameLen, label, flags, val
        DEFWORD \name, \nameLen, \label, \flags
        PUSH \val
        rts
        .endm
        
        .macro SAVE_REGS
        movem.l %d0/%a6, -(%a7) 
        .endm

        .macro RESTORE_REGS
        movem.l (%a7)+, %d0/%a6
        .endm
        
stackOverflow:
        
        move.l  %d0,  (tosDump)         | tos in d0
        move.l (%a7), (torsDump)        | tors in d1
        move.l  %a6,  (stackPointer)    
        move.l  %a7,  (rStackPointer)
        
        jmp stackOverflowError

stackUnderflow:
        
        move.l  %d0,  (tosDump)         | tos in d0
        move.l (%a7), (torsDump)        | tors in d1
        move.l  %a6,  (stackPointer)    
        move.l  %a7,  (rStackPointer)
        jmp stackUnderflowError
        
        
        DEFWORD "drop",4,drop,0
        | ( a -- )
        addq.l #4, %a6
        rts

        DEFWORD "dup",3,dup,0
        | ( a -- a a )
        move.l %d0, -(%a6)
        rts

        DEFWORD "over",4,over,0
        | ( a b -- a b a )
        move.l (%a6), %d1
        move.l %d0, -(%a6)
        move.l %d1, %d0
        rts

        DEFWORD "swap",4,swap,0
        | ( a b -- b a )
        move.l %d0, %d1
        move.l (%a6), %d0
        move.l %d1, (%a6)
        rts

        
        DEFWORD "rot",3,rot,0  
        | ( a b c -- b c a )
        | swap +4 and +0
        move.l (%a6)+, %d1
        move.l (%a6)+, %d2
        | now push them back in the right order
        PUSH %d1
        PUSH %d0
        move.l %d2, %d0
        rts
     	  
        DEFWORD "-rot",4,nrot,0
        | ( a b c -- c a b )
        move.l (%a6)+, %d1
        move.l (%a6)+, %d2
        move.l %d0, -(%a6)
        move.l %d2, -(%a6)
        move.l %d1, %d0
        rts
        
        DEFWORD "2dup",4,tdup,0
        | ( a b -- a b a b )
        move.l (%a6), %d1
        move.l %d0, -(%a6)
        move.l %d1, -(%a6)
        rts
    
        DEFWORD "2drop",5,tdrop,0
        | ( a b c d -- a b )
        addq.l #4, %a6
        POP %d0
        rts
        
        DEFWORD "2swap",5,tswap,0
        | ( 1 2 3 4 -- 3 4 1 2 )
        move.l %d0, %d3
        move.l (%a6)+, %d2
        move.l (%a6)+, %d1
        move.l (%a6)+, %d0
        movem.l %d1/%d2/%d3, -(%a6)
        rts
        
        DEFWORD "?dup",4,qdup,0 
        | ( 0 -- )
        | or 
        | ( nonzero -- nonzero nonzero )
        tst %d0
        bne qdEnd
        move.l %d0, -(%a6)
qdEnd:
        rts
        
        DEFWORD ">r",2,rto,0
        | p:( val -- ) r:(  --  val)
        move.l (%a7), %a0
        | pop top of stack into rstack slot
        pop (%a7)
        jmp (%a0)
        
        DEFWORD "r>",2,rfrom,0
        | p:(  -- val ) r:( val -- )
        move.l %d0, -(%a6)
        move.l (%a7)+, %a0
        move.l (%a7)+, %d0
        jmp (%a0)
        
        
        DEFWORD "1+",2,inc,0
        | ( val -- val+1 )
        addq.l #1, %d0
        rts
        
        DEFWORD "1-",2,dec,0
        | ( val -- val-1 )
        subq.l #1, %d0
        rts
        
        DEFWORD "4+",2,finc,0
        | ( val -- val+4 )      
        addq.l #4, %d0
        rts
        
        DEFWORD "4-",2,fdec,0
        | ( val -- val-4 )      
        subq.l #4, %d0
        rts
        
        DEFWORD "+",1,add,0
        | ( x y -- x+y )              
        add.l (%a6)+, %d0
        rts
        
        DEFWORD "-",1,sub,0
        | ( x y -- x-y )                
        sub.l (%a6)+, %d0
        neg.l %d0
        rts

        | *, /, /mod, and mod
        | only really handle bytes i think....
        | should add full word multiplication and division
        
        DEFWORD "*",1,mul,0
        | ( x y -- x-y )
        muls (%a6)+, %d0
        rts
        
        DEFWORD "/mod",4,divmod,0
        | ( x y -- x%y x/y )
        divs (%a6),%d0
        move.l %d0, %d1
        rol.l #8, %d1
        andi.l #0x0000FFFF, %d1
        move.w %d1, (%a6)
        rts
        
        DEFWORD "/",1,div,0
        | ( x y -- x/y )
        divs (%a6)+,%d0
        andi.l #0x0000FFFF, %d0
        rts
    
        DEFWORD "mod",3,mod,0
        | ( x y -- x%y )
        divs (%a6)+,%d0
        rol.l #8, %d0
        andi.l #0x0000FFFF, %d0
        rts
    
push_true:
        | ( -- 1 )
        PUSH #1
        rts
push_false:
        | ( -- 0 )
        PUSH #0
        rts    
        
        DEFWORD "cmp",3,comp,0
        POP %d1
        POP %d2
        cmp.l %d1,%d2
        beq push_true
        bra push_false
        
        
        DEFWORD "=",1,eq,0
        | ( x y == 1 ) when x == y
        | ( x y == 0 ) when x != y
        
        POP %d1
        POP %d2
        cmp.l %d1,%d2
        beq push_true
        bra push_false
        
        
        DEFWORD "!=",2,neq,0
        POP %d1
        POP %d2
        cmp.l %d1,%d2
        beq push_false
        bra push_true
        
        DEFWORD "0=",2,zeq,0
        POP %d1

        | status flags are set on the moved data, not the address
        | so we can pop a value off the stack,
        | and the flags are set for that value already
        | tst.l %d1
        beq push_true
        bra push_false

        DEFWORD "0<",2,zlt,0
        POP %d1
        blt push_true
        bra push_false

        DEFWORD "0<=",3,zle,0
        POP %d1
        ble push_true
        bra push_false
        
        DEFWORD "0>",2,zgt,0
        POP %d1
        bgt push_true
        bra push_false
        
        DEFWORD "0>=",2,zge,0
        POP %d1
        bge push_true
        bra push_false
        
        
        DEFWORD "0!=",3,zneq,0
        POP %d1
        bne push_true
        bra push_false
        
        DEFWORD "<",1,lt,0
        POP %d2
        POP %d1
        cmp.l %d1, %d2
        blt push_true
        bra push_false
            
        DEFWORD ">",1,gt,0
        POP %d2
        POP %d1
        cmp %d1, %d2
        bgt push_true
        bra push_false
        
        DEFWORD "<=",2,le,0
        POP %d2
        POP %d1
        cmp %d1, %d2
        ble push_true
        bra push_false
    
        DEFWORD ">=",2,ge,0
        POP %d2
        POP %d1
        cmp %d1, %d2
        bge push_true
        bra push_false
    
        DEFWORD "bye",3,bye,0
        PUSH #'B'
        jsr emit
        PUSH #'Y'
        jsr emit
        PUSH #'E'
        jsr emit    
bye_loop:        
        bra bye_loop
        
        
        DEFWORD "!",1,store,0
        | ( val addr -- )
        | stores the given value at the given address
        | works with 32-bit values
        move.l %d0, %a0
        move.l (%a6)+, (%a0)
        move.l (%a6)+, %d0
        rts

        DEFWORD "!+",2,incMem,0
        | ( addr -- )
        | increments the value at the given address
        move.l %d0, %a0
        addq.l #1, (%a0)
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "!-",2,decMem,0
        | ( addr -- )
        | decrements the value at the address
        move.l %d0, %a0
        subq.l #1, (%a0)
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "@",1,read,0
        | ( addr -- val )
        | reads a 32bit value from the given address
        move.l %d0, %a0
        move.l (%a0), %d0
        rts
        
        DEFWORD "@+",2,readInc,0
        | ( addr -- val addr+4 )
        | reads 32-bit values from successive addresses
        move.l %d0, %a0
        move.l (%a0), %d1
        move.l %d1, -(%a6)
        addq.l #4, %d0
        rts
        
        DEFWORD "@-",2,readDec,0
        | ( addr -- val addr-4 )
        | reads 32-bit values from decreasing addresses
        move.l %d0, %a0
        move.l (%a0), %d1
        move.l %d1, -(%a6)
        subq.l #4, %d0
        rts
        
        DEFWORD "w!",2,wstore,0
        | ( val addr -- )       
        | same as ! but only writes word values
        move.l %d0, %a0
        move.l (%a6)+, %d1
        move.w %d1, (%a0)
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "w@",2,wread,0
        move.l %d0, %a0
        move.w (%a0), %d0
        andi.w #0xFFFF, %d0
        rts
        
        DEFWORD "c!",2,cstore,0
        move.l %d0, %a0
        move.l (%a6)+, %d1
        move.b %d1, (%a0)    | store just a byte
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "c@",2,cread,0
        move.l %d0, %a0
        move.b (%a0), %d0
        andi.l #0x000000FF, %d0   | just keep the byte read
        rts
        
        DEFWORD "c@c!",4,ccopy,0
        | ( srcAddr destAddr )
        move.l %d0, %a1       | get dest ptr
        move.l (%a6), %a0     | get src ptr
        move.b (%a0), (%a1)   | copy byte
        addq.l #1, %d0        | increment dest ptr
        addq.l #1, (%a6)      | increment src ptr
        rts
        
        DEFWORD "cmove",5,cmove,0
                                | ( srcAddr destAddr numBytes )
                                | move longwords all at once ;)
        pop %d1                 | numBytes in %d1
        pop %a1                 | dest in %a1
        pop %a0                 | src in %a0
        tst %d1
        ble cMoveEnd
                                | get number of longwords to copy, along with remainder
        lsr.l #2, %d1           | divide by 4 (faster and correct because %d1 can't be negative)
        move.l %d1, %d2         | dividend in %d2
        andi.l #0x0000FFFF, %d2
        rol.l #8, %d1  
        andi.l #0x0000FFFF,%d1       | remainder in %d1
        tst %d1
        beq cMoveLongLoop
cMoveByte:
        move.b (%a0)+, (%a1)+
        dblt %d1, cMoveByte     | count down the remainder, max 3 bytes copied individually  
cMoveLong:                      | copy four words at a time ;)
        tst %d2
        beq cMoveEnd
cMoveLongLoop:
        move.l (%a0)+, (%a1)+
        dblt %d2, cMoveLongLoop
cMoveEnd:
        rts        
    
   
        DEFWORD ".",1,dot,0     | prints the top element of the stack in decimal
        moveq #0, %d1           | %d1 is counter  ( rest integer )
        POP %d2                 | ( rest of stack )
dotLoop:
        move.b %d2, %d3
        andi.l #0xFFFF, %d3      | get word?
        divs #10, %d3
        lsr.l #8, %d3
        lsr.l #8, %d3
        add.l #0x30, %d3
        PUSH %d3                | save char on stack ( rest of stack firstChar )
        addq.l #1, %d1
        divu #10, %d2
        andi.l #0xFFFF, %d2 
        bgt dotLoop
        subq.l #1, %d1
dotPrintLoop:
                                | ( rest of stack chars )
                                | we need to save %d1
        PUSH %d1
        bsr swap
        bsr emit
        POP %d1
        dbeq %d1, dotPrintLoop
        PUSH #0x20
        bra emit
    
        DEFWORD "depth",5,depth,0
        | ( -- depth )
        move.l %a6, %d1
        subi.l #ps_end, %d1
        neg.l %d1
        divs #4, %d1
        andi.l #0x0000FFFF, %d1
        PUSH %d1
        rts
        
        DEFWORD "goto",4,goto,0
        move.l %d0, %a0
        move.l (%a6)+, %d0
        addq #4, %a7
        jmp (%a0)
        

        

        DEFVAR "state",5,state,0,0
        DEFVAR "here",4,here,0,0
        DEFVAR "latest",6,latest,0,0
        DEFVAR "base",4,base,0,0xA
        
        | 1 == case-sensitive, 0 == case-insensitive
        DEFVAR "case",4,case,0
        DEFCONST "s0",2,szero, 0, #ps_end
        
        | hundredths of versions ;)
        DEFCONST "version", 7, version, 0, #001
        DEFCONST "r0", 2, rzero, 0, #0x00FFFE00 
        DEFCONST "f_immed", 7, fimmed, 0, #F_IMMED
        DEFCONST "f_hidden", 8, fhidden, 0, #F_HIDDEN
        DEFCONST "f_lenmask", 9, flenmask, 0, #F_LENMASK
    
    
        DEFWORD "and",3,and,0
        and.l (%a6)+, %d0
        rts
        
        DEFWORD "or",2,or,0
        or.l (%a6)+, %d0
        rts
        
        DEFWORD "xor",3,xor,0
        move.l (%a6)+, %d1
        eor.l %d1, %d0
        rts

        DEFWORD "negate", 6, negate,0
        neg.l %d0
        rts
        
        DEFWORD "not", 3, not, 0
        not.l %d0
        rts

        DEFWORD "exit", 4, exit, 0
        addq #4, %sp
        rts

        DEFWORD "lit", 3, lit, 0
        move.l (%a7), %a0
        push (%a0)
        addq.l #4, (%a7)
        rts

        | won't work inlined!
        DEFWORD "rsp@",4,rspread,0
        |  r ( something return-addr )
        PUSH %sp
        addq #4, %d0
        rts

        | won't work inlined!
        DEFWORD "rsp!",4,rspstore,0
        move.l (%a7), %a0
        POP %a7
        jmp (%a0)

        | won't work inlined!
        DEFWORD "rdrop",5,rdrop,0
        move.l (%a7)+, (%a7)
        rts


        DEFWORD "dsp@",4,dspfetch,0
        move.l %a6, %d1
        PUSH %d1
        rts

        DEFWORD "dsp!",4,dspstore,0
        move.l %d0, %a6
        move.l (%a6)+, %d0
        rts

        DEFWORD "key",3,key,0
        bra _key
afterCallKey:   
        rts

_key:
        | is buffer empty?
        move.l curkey, %d1
        cmp.l bufftop, %d1

                                | if curkey == end of buffer, get new line of input
        bge get_new_input
                                | otherwise grab the next character from the buffer

        | move.l %d0
        | PUSH (%a0)+
        move.l %d0, -(%a6)
        move.l (curkey), %a0
        move.b (%a0), %d0
        and.l #0x000000FF, %d0
                
        addq.l #1, (curkey)
        bra afterCallKey
        
        
get_new_input:
        movem.l %d0/%a6, -(%a7) 
        jsr get_line_of_input
        movem.l (%a7)+, %d0/%a6
        
        | go back to the top and try to grab character
        bra _key


        
        DEFWORD "ccall1",6,ccall1,0
        | ( p1 addr -- )
        POP %a0 | get address
        POP %d1 | get param
        SAVE_REGS
        RPUSH %d1
        jsr (%a0)
        addq.l #4, %a7
        RESTORE_REGS
        rts

        DEFWORD "ccall2",6,ccall2,0
        | ( p1 p2 addr -- )
        POP %a0
        POP %d2
        POP %d1
        SAVE_REGS
        RPUSH %d2
        RPUSH %d1
        jsr (%a0)
        addq.l #8, %a7
        RESTORE_REGS
        rts
        
        DEFWORD "ccall3",6,ccall3,0
        | ( p1 p2 p3 addr -- )
        POP %a0
        POP %d3
        POP %d2
        POP %d1
        SAVE_REGS
        RPUSH %d3
        RPUSH %d2
        RPUSH %d1
        jsr (%a0)
        add.l #12, %a7
        RESTORE_REGS
        rts
        
        DEFWORD "emit",4,emit,0
        | ( key )
        | ccall1 ( param addr -- )
        PUSH #printChar
        jsr ccall1
        
        rts

        DEFWORD "word",4,word,0
        moveq.l #0, %d1                 | length in d1
        bra get_first_char
after_get_char:
        | POP %d2                       
        move.l #wordBuffer, %d0         | get rid of space char
        PUSH %d1
        rts
        
get_first_char:
                                        | get key on top of stack
        bsr _key
        cmpi.b #' ', %d0
        beq get_first_char

        move.l #wordBuffer, %a0
        moveq.l #0, %d1
store_chars:    
        move.b %d0, (%a0)+         | pop char into word buffer
        move.l (%a6)+, %d0
        addq.b #1, %d1
                
        RPUSH %a0
        RPUSH %d1
        jsr key                        | get next char
        RPOP %d1
        RPOP %a0
        cmpi.b #' ', %d0
                                        | if not space keep grabbing chars
        bne store_chars
                                        | if found space return address of character and number of characters
        bra after_get_char
        



        
        
        DEFWORD "tell",4,tell,0
        | just print a whole string
        | ( strAddr strLen -- )
        push #printStrn
        jsr ccall2
        rts

        DEFWORD "cr",2,cr,0
        PUSH #'\n'
        jsr emit
        rts
        
        DEFWORD "number",6,number,0
        | ( strAddr strLen -- )
        | parses a string into a string
        | returns 0 if error detected
       
        bra _number
after_get_number:       
        PUSH %d3 | parsed number
        PUSH %d1 | number of unparsed characters
        rts

_number:
        | move.b (base), %d4      | get base
        jsr base
        jsr read
        POP %d4                 | place base in d4
        POP %d1                 | place length in d1
        POP %a0                 | place addr in a0

        moveq.l #0, %d3         | cmover register for result
        moveq.l #0, %d5         | cmover register for temporary characters
        
        | check if first char is '-'
        move.b (%a0)+, %d5      | first char in d5     
        PUSH #0                 | push 0 on stack
        cmp.b #'-', %d5
        bne number_conv_char
        move.l #-1, %d0         | push something !=0 to indicate negative number
        subq #1, %d1            | if zero, the only character is '-'
        bne read_digit_loop
        moveq.l #0, %d3         | set number to 0
        POP %d5                 | pop -1 off stack
        move.l #1, %d1          | and set unparsed characters to 1
        bra after_get_number      

read_digit_loop:
        muls.w %d4, %d3         | multiply by base
        move.b (%a0)+, %d5      | read character into d5
        
number_conv_char:      
        sub.b #'0', %d5         | subtract '0' char to get number
        blt number_negate       | less than 0?, jump to negate
        cmp #10, %d5            | <= 9
        blt number_cmp_base
        sub.b #17, %d5          | < 'A' (16 is 'A'-'0')
        blt number_negate
        add.b #10, %d5          
        
number_cmp_base:
        cmp %d4, %d5            | >= base?
        bge number_negate       


        add.l %d5, %d3
        subq.l #1, %d1
        bne read_digit_loop
        
number_negate:
        POP %d6
        tst %d6
        beq number_end
        neg.l %d3
        
number_end:     
        bra after_get_number
        

        DEFWORD "find",4,find,0
        

        
        
ps_overflow:
        .space 10
        | param stack grows down, with the pointer pointing at the top value
        .space 4*PARAM_STACK_SIZE, 'A'

ps_end: 
ps_end_var:
        dc.l ps_end

        .align 2        
        | word buffer
wordBuffer:     
        .space 38, ' '
        

        | ptr in key buffer
curkey:
        dc.l buffer
        | ptr to end of input in key buffer
bufftop:
        dc.l buffer
        .align 2
        | input buffer
buffer:
        .space 38, ' ' | 38 character buffer (screen width w/ 1 char margin on each side)
        
herePtr:
        
