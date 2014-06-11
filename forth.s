        
        | subroutine threaded forth for the megadrive

        | a6 points to the second to top value on the parameter stack and grows downward
        | d0 contains the top value on the parameter stack
        | a7 points to the top value on the return stack (native stack) and grows downward

             
        | DEBUG     EQU 1       
        .equ F_IMMED,  0x80
        .equ F_HIDDEN, 0x20  
        .equ F_LENMASK, 0x1F 
        .equ F_HIDDENLENMASK,  0x3F
        .equ PARAM_STACK_SIZE, 32      
        .equ SHADOW_STACK_OFFSET, PARAM_STACK_SIZE
        .set LAST_WORD, 0
        .set LAST_WORD_2, 0
        .set USER_VAR_OFFSET, 0
        .set USER_VAR_COUNT, 0
        .set RSTACK_END, 0x00FFFE00 

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

        .macro HALT
halt_loop:
        bra halt_loop
        .endm
                

        .macro DEFWORD name, nameLen, label, flags, doc
        .set DOC_START, .
        .ascii "\doc"
        .set DOC_LEN, .-DOC_START
        .balign 2
        dc.w DOC_LEN
        dc.l DOC_START
        dc.l LAST_WORD
        .set LAST_WORD, .-4
        .byte \nameLen+\flags
        .ascii "\name"
        .balign 2
\label:
        .endm

        .macro DEFSYSVAR name, nameLen, label, flags, val, doc
loc_\label:
        dc.l \val
        DEFWORD \name, \nameLen, \label, \flags, "\doc"
        PUSH #loc_\label
        rts
       .endm
        

        .macro DEFUSERVAR name, nameLen, label, flags, val, doc
        
        DEFWORD \name, \nameLen, \label, \flags, "\doc"
        add.l %a5, %a4                  | offset by user task pointer
        PUSH %a4                        | push result onto stack
        rts
        
        | increment offset pointer
        .set USER_VAR_OFFSET, (USER_VAR_OFFSET+4)
        .set USER_VAR_COUNT, USER_VAR_COUNT+1
        .endm


        
        .macro DEFCONST name, nameLen, label, flags, val, doc
        DEFWORD \name, \nameLen, \label, \flags, "\doc"
        PUSH \val
        rts
        .endm
        
        .macro SAVE_REGS
        movem.l %d0/%a5/%a6/%a7, -(%a7) 
        .endm

        .macro RESTORE_REGS
        movem.l (%a7)+, %d0/%a5/%a6/%a7
        .endm

   



        
        .text
        .globl main, get_line_of_input, buffer, bufftop, curkey, loc_latest
        .extern stackOverflowError, stackPointer, rStackPointer, tosDump, torsDump, printChar, initIO, printNewline, printStrn, printStrNewline

        | ENTRY POINT, start of ROM
main:
        | well, we don't really need ROM so jump to RAM
        jmp ram_entry_point



        | start of ram
        .data

        | place all system vars at the beginning
        DEFSYSVAR "CP",2,cp,0,0,"Location of the current dictionary pointer."
        DEFSYSVAR "LATEST",6,latest,0,0,"Location of the last compiled word."
        DEFSYSVAR "UOFF",4,user_var_offset,0,0,"Offset to user data."
        DEFSYSVAR "UCNT",4,user_var_count,0,0,"# of user variables defined."
        
        DEFSYSVAR "STATE",5,state,0,0,"State of the interpreter (1==compile,0==interpreter)."
        DEFSYSVAR "BASE",4,base,0,0xA,"Number base for parser/output words."   
        | 1 == case-sensitive, 0 == case-insensitive
        | DEFSYSVAR "CASE",4,case,0,"Case sensitive? (1 == yes,0==no)"
        
        DEFCONST "S0",2,szero, 0, #ps_end, "Location of the top of the stack (grows downward)."
        DEFCONST "SS0",3,sszero, 0, #shadow_stack, "Location of the top of the shadow stack (grows downward)."
        
        | hundredths of versions
        DEFCONST "VERSION", 7, version, 0, #03,"Current version."
        DEFCONST "R0", 2, rzero, 0, #RSTACK_END,"Top of return stack. Uses the native stack so grows downward."
        DEFCONST "F_IMMED", 7, fimmed, 0, #F_IMMED,"Flag for marking words as immediate."
        DEFCONST "F_HIDDEN", 8, fhidden, 0, #F_HIDDEN,"Flag for marking words as hidden."
        DEFCONST "F_LENMASK", 9, flenmask, 0, #F_LENMASK,"Masks off the length of a word from the flags+length field."

        
welcome_message:
        .ascii "Mega Forth"
version_message:        
        .ascii "Version: "
        .balign 2
ram_entry_point:
        | initialize I/O stuff (in io.c)
        jsr initIO
        
        
        | set up parameter stack
        move.l (ps_end), %a6

        
        bsr init_latest
        bsr init_cp
        bsr init_uvo
        bsr init_up

        moveq.l #0, %d0 


        
        PUSH #welcome_message
        PUSH #10
        bsr telln
        bsr cr
        PUSH #version_message
        PUSH #9
        bsr telln
        bsr version
        | divu #10, %d0
        | andi.l #0x0000FFFF, %d0
        | bsr dot
        PUSH #'.'
        bsr emit
        bsr version
        divu #10, %d0
        andi.l #0xFFFF0000, %d0
        asr.l #8, %d0
        asr.l #8, %d0
        bsr dot
        bra quit
        
        .balign 4
        .space 16
ps_overflow:

        
        | param stack grows down, with the pointer pointing at the top value
        .space 4*PARAM_STACK_SIZE, 'A'
ps_end: 
ps_end_var:
        dc.l ps_end

        .space 4*PARAM_STACK_SIZE, 0
shadow_stack:
        
        
        .balign 2        
        | word buffer
curWordSlot:
        dc.l wordBuffer
wordBuffer:     
        .space 256, ' '
wordBufTop:
        

        | ptr in key buffer
curkey:
        dc.l buffer
        | ptr to end of input in key buffer
bufftop:

        dc.l buffer
        .balign 2
        | input buffer
buffer:
        .space 38, ' ' | 38 character buffer (screen width w/ 1 char margin on each side)
        

        






        
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
        
        
        DEFWORD "DROP",4,drop,0, "( a -- )"
        move.l (%a6)+, %d0
        rts

        DEFWORD "DUP",3,dup,0,"( a -- a a )"
        move.l %d0, -(%a6)
        rts

        DEFWORD "OVER",4,over,0,"( a b -- a b a )"
        | ( a b -- a b a )
        move.l (%a6), %d1
        move.l %d0, -(%a6)
        move.l %d1, %d0
        rts

        DEFWORD "SWAP",4,swap,0,"( a b -- b a )"
        | ( a b -- b a )
        move.l %d0, %d1
        move.l (%a6), %d0
        move.l %d1, (%a6)
        rts

        
        DEFWORD "ROT",3,rot,0, "( a b c -- b c a )"  
        | ( a b c -- b c a )
        | swap +4 and +0
        move.l (%a6)+, %d1
        move.l (%a6)+, %d2
        | now push them back in the right order
        PUSH %d1
        PUSH %d0
        move.l %d2, %d0
        rts
     	  
        DEFWORD "-ROT",4,nrot,0, "( a b c -- c a b )"
        | ( a b c -- c a b )
        move.l (%a6)+, %d1
        move.l (%a6)+, %d2
        move.l %d0, -(%a6)
        move.l %d2, -(%a6)
        move.l %d1, %d0
        rts
        
        DEFWORD "2DUP",4,tdup,0, "( a b -- a b a b )"
        | ( a b -- a b a b )
        move.l (%a6), %d1
        move.l %d0, -(%a6)
        move.l %d1, -(%a6)
        rts
    
        DEFWORD "2DROP",5,tdrop,0, "( a b c d -- a b )"
        | ( a b c d -- a b )
        addq.l #4, %a6
        POP %d0
        rts
        
        DEFWORD "2SWAP",5,tswap,0, "( a b c d -- c d a b )"
        | ( 1 2 3 4 -- 3 4 1 2 )
        move.l %d0, %d3
        move.l (%a6)+, %d2
        move.l (%a6)+, %d1
        move.l (%a6)+, %d0
        movem.l %d1/%d2/%d3, -(%a6)
        rts
        
        DEFWORD "?DUP",4,qdup,0, "( a == 0 -- a a ), ( a != 0 -- )"
        | ( 0 -- )
        | or 
        | ( nonzero -- nonzero nonzero )
        tst.l %d0
        bne qdEnd
        move.l %d0, -(%a6)
qdEnd:
        rts
        
        DEFWORD ">R",2,rto,0, "( a -- ) r: ( -- a )"
        | p:( val -- ) r:(  --  val)
        move.l (%a7), %a0
        | pop top of stack into rstack slot
        pop (%a7)
        jmp (%a0)
        
        DEFWORD "R>",2,rfrom,0, "( -- a ) r: ( a -- )"
        | p:(  -- val ) r:( val -- )
        move.l %d0, -(%a6)
        move.l (%a7)+, %a0
        move.l (%a7)+, %d0
        jmp (%a0)
        
        
        DEFWORD "1+",2,inc,0, "( a -- a+1 )"
        | ( val -- val+1 )
        addq.l #1, %d0
        rts
        
        DEFWORD "1-",2,dec,0, "( a -- a-1 )"
        | ( val -- val-1 )
        subq.l #1, %d0
        rts
        
        DEFWORD "4+",2,finc,0, "( a -- a+4 )"
        | ( val -- val+4 )      
        addq.l #4, %d0
        rts
        
        DEFWORD "4-",2,fdec,0, "( a -- a-4 )"
        | ( val -- val-4 )      
        subq.l #4, %d0
        rts
        
        DEFWORD "+",1,add,0, "( a b -- a+b )"
        | ( x y -- x+y )              
        add.l (%a6)+, %d0
        rts
        
        DEFWORD "-",1,sub,0, "( a b -- a-b )"
        | ( x y -- x-y )                
        sub.l (%a6)+, %d0
        neg.l %d0
        rts

        | *, /, /mod, and mod
        | only really handle bytes i think....
        | should add full word multiplication and division
        
        DEFWORD "*",1,mul,0, "( a b -- a*b )"
        | ( x y -- x*y )
        move.l (%a6)+, %d1
        muls.w %d1, %d0
        
        rts
        
        DEFWORD "/MOD",4,divmod,0, "( a b -- a%b a/b )"
        | ( x y -- x%y x/y )
        divs (%a6),%d0
        move.l %d0, %d1
        rol.l #8, %d1
        andi.l #0x0000FFFF, %d1
        move.w %d1, (%a6)
        rts
        
        DEFWORD "/",1,div,0, "( a b -- a/b )"
        | ( x y -- x/y )
        divs (%a6)+,%d0
        andi.l #0x0000FFFF, %d0
        rts
    
        DEFWORD "MOD",3,mod,0, "( a b -- a%b )"
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
        
        
        
        DEFWORD "=",1,eq,0, "( a b -- a==b?1:0 )"
        | ( x y == 1 ) when x == y
        | ( x y == 0 ) when x != y
        
        POP %d1
        POP %d2
        cmp.l %d1,%d2
        beq push_true
        bra push_false
        
        
        DEFWORD "!=",2,neq,0, "( a b -- a!=b?1:0 )"
        POP %d1
        POP %d2
        cmp.l %d1,%d2
        beq push_false
        bra push_true
        
        DEFWORD "0=",2,zeq,0, "( a -- a==0?1:0 )"
        POP %d1

        | status flags are set on the moved data, not the address
        | so we can pop a value off the stack,
        | and the flags are set for that value alfetchy
        | tst.l %d1
        beq push_true
        bra push_false

        DEFWORD "0<",2,zlt,0, "( a -- a>0?1:0 )"
        POP %d1
        blt push_true
        bra push_false

        DEFWORD "0<=",3,zle,0, "( a -- a>=0?1:0 )"
        POP %d1
        ble push_true
        bra push_false
        
        DEFWORD "0>",2,zgt,0, "( a -- a<0?1:0 )"
        POP %d1
        bgt push_true
        bra push_false
        
        DEFWORD "0>=",2,zge,0, "( a -- a<=0?1:0 )"
        POP %d1
        bge push_true
        bra push_false
        
        
        DEFWORD "0!=",3,zneq,0, "( a -- a!=0?1:0 )"
        POP %d1
        bne push_true
        bra push_false
        
        DEFWORD "<",1,lt,0, "( a b -- a<b?1:0 )"
        POP %d2
        POP %d1
        cmp.l %d1, %d2
        blt push_true
        bra push_false
            
        DEFWORD ">",1,gt,0, "( a b -- a>b?1:0 )"
        POP %d2
        POP %d1
        cmp %d1, %d2
        bgt push_true
        bra push_false
        
        DEFWORD "<=",2,le,0, "( a b -- a<=b?1:0 )"
        POP %d2
        POP %d1
        cmp %d1, %d2
        ble push_true
        bra push_false
    
        DEFWORD ">=",2,ge,0, "( a b -- a>=b?1:0 )"
        POP %d2
        POP %d1
        cmp %d1, %d2
        bge push_true
        bra push_false
    
        DEFWORD "BYE",3,bye,0, "( -- ) Exits interpreter."
        PUSH #'E'
        PUSH #'Y'
        PUSH #'B'
        bsr emit
        bsr emit
        bsr emit
bye_loop:        
        bra bye_loop

        DEFWORD "RESTART",7,restart,0, "( -- ) Restarts system."
        PUSH #SYS_reset
        bra ccall1
        
        
        DEFWORD "!",1,store,0, "( a b -- ) Stores a at the address given by b."
        move.l %d0, %a0
        move.l (%a6)+, (%a0)
        move.l (%a6)+, %d0
        rts

        DEFWORD "!+",2,incMem,0, "( b -- ) Increments the longword value at b."
        POP %a0
        addq.l #1, (%a0)
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "!-",2,decMem,0, "( b -- ) Decrements the longword value at b."
        move.l %d0, %a0
        subq.l #1, (%a0)
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "@",1,fetch,0, "( a -- b ) Fetches the longword value b at a."
        move.l %d0, %a0
        move.l (%a0), %d0
        rts
        
        DEFWORD "@+",2,fetchInc,0, "( a -- b a+4 ) Fetches longword value b from a, returning the incremented address."
        move.l %d0, %a0
        move.l (%a0), %d1
        move.l %d1, -(%a6)
        addq.l #4, %d0
        rts
        
        DEFWORD "@-",2,fetchDec,0, "( a -- b a-4 ) Fetches longword value b from a, returning the decremented address."
        move.l %d0, %a0
        move.l (%a0), %d1
        move.l %d1, -(%a6)
        subq.l #4, %d0
        rts
        
        DEFWORD "W!",2,wstore,0, "( a b -- ) Stores first lower word of a at address b."
        move.l %d0, %a0
        move.l (%a6)+, %d1
        move.w %d1, (%a0)
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "W@",2,wfetch,0, "( a -- b ) Fetches word b at a."
        move.l %d0, %a0
        move.w (%a0), %d0
        andi.w #0xFFFF, %d0
        rts
        
        DEFWORD "C!",2,cstore,0, "( a b -- ) Stores low byte of a at address b."
        move.l %d0, %a0
        move.l (%a6)+, %d1
        move.b %d1, (%a0)    | store just a byte
        move.l (%a6)+, %d0
        rts
        
        DEFWORD "C@",2,cfetch,0, "( a -- b ) Fetches byte b from address a."
        move.l %d0, %a0
        move.b (%a0), %d0
        andi.l #0x000000FF, %d0   | just keep the byte fetch
        rts
        
        DEFWORD "C@C!",4,ccopy,0, "( src dest -- src dest ) Copies a byte from src to dest." 
        move.l %d0, %a1       | get dest ptr
        move.l (%a6), %a0     | get src ptr
        move.b (%a0), (%a1)   | copy byte
        addq.l #1, %d0        | increment dest ptr
        addq.l #1, (%a6)      | increment src ptr
        rts
        
        DEFWORD "CMOVE",5,cmove,0, "( src dest num -- ) Copies num bytes from src to dest."
                                | unrolled loop
                                | move longwords all at once 
        pop %d1                 | numBytes in %d1
        pop %a1                 | dest in %a1
        pop %a0                 | src in %a0
        tst.l %d1
        ble cMoveEnd
                                | get number of longwords to copy, along with remainder
        lsr.l #2, %d1           | divide by 4 (faster and correct because %d1 can't be negative)
        move.l %d1, %d2         | dividend in %d2
        andi.l #0x0000FFFF, %d2
        rol.l #8, %d1  
        andi.l #0x0000FFFF,%d1       | remainder in %d1
        tst.l %d1
        beq cMoveLongLoop
cMoveByte:
        move.b (%a0)+, (%a1)+
        dblt %d1, cMoveByte     | count down the remainder, max 3 bytes copied individually  
cMoveLong:                      | copy four words at a time 
        tst.l %d2
        beq cMoveEnd
cMoveLongLoop:
        move.l (%a0)+, (%a1)+
        dblt %d2, cMoveLongLoop
cMoveEnd:
        rts        
    
   
        DEFWORD ".",1,dot,0, "( a -- ) Prints the top object on the stack. (decimal only)"
        moveq #0, %d1
        | %d1 is counter  ( rest integer )
        POP %d2
        | ( rest of stack )
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
        rts
        
        DEFWORD "DEPTH",5,depth,0, "( -- depth ) Returns current depth of stack."
        | ( -- depth )
        move.l %a6, %d1
        move.l #ps_end, %d2
        sub.l %d1, %d2
        lsl #2, %d2
        PUSH %d2
        rts

        DEFWORD "RDEPTH",6,rdepth,0, "( -- depth ) Returns current depth of return stack"
        move.l %a7, %d1
        move.l RSTACK_END, %d2
        sub.l %d1, %d2
        lsl #2, %d2
        PUSH %d2
        rts
        

        DEFWORD "GOTO",4,goto,0, "( a -- ) Transfer control to address a."
        move.l %d0, %a0
        move.l (%a6)+, %d0
        jmp (%a0)
        
    
        DEFWORD "AND",3,and,0, "( a b -- a & b)"
        and.l (%a6)+, %d0
        rts
        
        DEFWORD "OR",2,or,0, "( a b -- a | b)"
        or.l (%a6)+, %d0
        rts
        
        DEFWORD "XOR",3,xor,0, "( a b -- a xor b)"
        move.l (%a6)+, %d1
        eor.l %d1, %d0
        rts

        DEFWORD "NEGATE", 6, negate,0, "( a -- -a )"
        negx.l %d0
        rts
        
        DEFWORD "NOT", 3, not, 0, "( a -- ~a )"
        not.l %d0
        rts

        | DEFWORD "LIT", 3, lit, 0
        | move.l (%a7), %a0
        | push (%a0)
        | addq.l #4, (%a7)
        | rts

        | won't work inlined!
        DEFWORD "RSP@",4,rspfetch,0, "( -- a ) Fetches return stack pointer a."
        |  r ( something return-addr )
        PUSH %sp
        addq #4, %d0
        rts

        | won't work inlined!
        DEFWORD "RSP!",4,rspstore,0, "( a -- ) Sets return stack pointer to a."
        move.l (%a7), %a0
        POP %a7
        jmp (%a0)

        | won't work inlined!
        DEFWORD "RDROP",5,rdrop,0, "r: ( a -- ) Drops the top object off the return stack."
        move.l (%a7)+, (%a7)
        rts


        DEFWORD "DSP@",4,dspfetch,0, "( -- a ) Fetches stack pointer a."
        move.l %a6, %d1
        PUSH %d1
        rts

        DEFWORD "DSP!",4,dspstore,0, "( a -- ) Sets stack pointer to a."
        move.l %d0, %a6
        move.l (%a6)+, %d0
        rts     

        DEFWORD "KEY",3,key,0, "( -- key ) Reads a character from input."

_key:
        | is buffer empty?
        move.l curkey, %a1
        move.l bufftop, %a2
        cmp.l %a2, %a1

        
                                | if curkey == end of buffer, get new line of input
        bge get_new_input
                                | otherwise grab the next character from the buffer

        move.b (%a1), %d1
        andi.l #0x000000FF, %d1
        PUSH %d1
        
        addq.l #1, (curkey)
        rts


        
get_new_input:
        PUSH #get_line_of_input
        bsr ccall0
              
        | go back to the top and try to grab character
        bra _key


        DEFWORD "CCALL0",6,ccall0,0, "( a -- ) Calls a c-function at addr a."
        POP %a0 | address
        SAVE_REGS
        jsr (%a0)
        RESTORE_REGS
        rts
        
        
        DEFWORD "CCALL1",6,ccall1,0, "( a b -- ) Calls a c-function at addr b with operand a."
        | ( p1 addr -- )
        POP %a0 | get address
        POP %d1 | get param
        SAVE_REGS
        RPUSH %d1
        jsr (%a0)
        addq.l #4, %a7
        RESTORE_REGS
        rts

        DEFWORD "CCALL2",6,ccall2,0, "( a b c -- ) Calls a c-function at addr c with operands a and b."
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
        
        DEFWORD "CCALL3",6,ccall3,0, "( a b c d ) Calls a c-function at addr d with operands a, b, and c."
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
        
        DEFWORD "EMIT",4,emit,0, "( a -- ) Prints a as a character."
        PUSH #printChar
        bra ccall1

        DEFWORD "CR",2,cr,0, "( -- ) Prints a newline."
        PUSH #'\n'
        bra emit
        
        
        DEFWORD "WORD",4,word,0, "( -- wordAddr wordLen ) Reads a word from input."
        bra get_first_char
after_get_word:
        | start address in a0
        | length in d1
        | move.l #wordBuffer, %d0         | get rid of space char       
        move.l %a0, (curWordSlot)
        move.l %a2, %d0
        PUSH %d1
        rts

        
get_first_char:
                                        | get key on top of stack
        bsr key
        cmpi.b #' ', %d0
        bne start_store_chars
        POP %d2
        bra get_first_char

start_store_chars:      
        | #curWordSlot
        | #wordBuffer
        move.l (curWordSlot), %a0
        move.l %a0, %a2                 | start of word
        move.l #wordBufTop, %a1
        cmp.l %a0, %a1
        beq flush_word_buffer
        moveq.l #0, %d1
store_chars:

        POP %d2
        move.b %d2, (%a0)+      
        
        addq.b #1, %d1
                
        RPUSH %a0
        RPUSH %a2
        RPUSH %d1
        bsr key                        | get next char
        RPOP %d1
        RPOP %a2
        RPOP %a0
        cmpi.b #' ', %d0
                                        | if not space keep grabbing chars
        bne store_chars
                                        | if found space return address of character and number of characters
        bra after_get_word

flush_word_buffer:
        move.l #wordBuffer, (curWordSlot)
        bra start_store_chars
        

        DEFWORD "TELL",4,tell,0, "( a b -- ) Prints string at a with length b."
        | just print a whole string
        | ( strAddr strLen -- )
        push #printStrn
        bra ccall2

        DEFWORD "TELLN",5,telln,0, "( a b -- ) Prints string at a with length b. Starts with a freshline is string is too long."
        push #printStrNewline
        bra ccall2
        
        DEFWORD "NUMBER",6,number,0, "( strAddr strLen -- number #unparsedChars ) Parses a string, returning a number and the number of unparsed characters (only nonzero in case of error)."
        bra _number
after_get_number:       
        PUSH %d3 | parsed number
        PUSH %d1 | number of unparsed characters
        rts

_number:
        | move.b (base), %d4      | get base
        bsr base
        bsr fetch
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
        bne fetch_digit_loop
        moveq.l #0, %d3         | set number to 0
        POP %d5                 | pop -1 off stack
        move.l #1, %d1          | and set unparsed characters to 1
        bra after_get_number      

fetch_digit_loop:
        muls.w %d4, %d3         | multiply by base
        move.b (%a0)+, %d5      | fetch character into d5
        
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
        bne fetch_digit_loop
        
number_negate:
        POP %d6
        tst.l %d6
        beq number_end
        neg.l %d3
        
number_end:     
        bra after_get_number

        DEFWORD "IS-IMMEDIATE",12,qimmediate,0, "( wordAddr -- isImmediate ) Returns 1 if given word is an immediate word, 0 otherwise."
        | ( dictEntryAddr -- t/f )
        addq #4, %d0
        move.l %d0, %a0 
        move.b (%a0), %d0
        andi.l #F_IMMED, %d0
        rts

        DEFWORD "MARK",4,mark,0, "( -- ) Places a mark on the shadow stack."
        move.l #1, (SHADOW_STACK_OFFSET, %a6)             | mark this location in the shadow stack
        rts

        DEFWORD "UNMARK",6,unmark,0, "( -- ) Removes a mark from the shadow stack."
        move.l #0, (SHADOW_STACK_OFFSET, %a6)
        rts
        

        DEFWORD "MARKED?",7,markedp,0,"( -- ) Is the current location on the shadow stack marked?"
        move.l (SHADOW_STACK_OFFSET, %a6), %d1            | move shadow stack value to stack (shadow stack should only contain 0s and 1s
        move.l %d0, -(%a6)
        move.l %d1, %d0
        rts
        

        DEFWORD "CLEAR-TO-MARK",13,clearToMark,0, "( -- ) Clear stack until we reach a marked slot."
        move.l %a6, %a0
        add.l #SHADOW_STACK_OFFSET, %a0
ctmLoop:        
        move.l (%a0)+, %d1
        bne ctmDone
        POP %d2
        bra ctmLoop
ctmDone:        
        rts

        
        DEFWORD "COUNT-TO-MARK",13,countToMark,0, "( -- #objects ) Count number of items on stack above the last mark."
        move.l #0, %d2
        move.l %a6, %a0
        add.l #SHADOW_STACK_OFFSET, %a0
cntLoop:
        move.l (%a0)+, %d1
        bne cntDone
        addq.l #1, %d2
        bra cntLoop
cntDone:
        PUSH %d2
        rts
        
        
        DEFWORD "FIND",4,find,0, "( strAddr strLen -- wordAddr ) Finds address of word corresponding to string. Returns 0 if no word exists."
        POP %d1                 | length of string in d1
        POP %a0                 | address of string in a0
        bra _find_start
after_find:     
        PUSH %a1                | dictionary address will be in a1
        rts
        
_find_start:
        moveq.l #0, %d2
        move.l (loc_latest), %a1      | address of latest header in a1      
_find:
        | compare lengths of strings
        move.b 4(%a1), %d2                   | get length in d2

        andi.b #F_HIDDENLENMASK, %d2       | get length and hidden flag

        | if hidden flag is set, this compare will always fail
        cmp.b %d2, %d1                    | compare lengths
        bne.b get_next_entry


        | compare strings in detail
        move.l %a1, %a2                   | dict string address in a2
        addq.l #5, %a2
        move.l %a0, %a3                   | make copy of address 
        | move.l %d1, %d2                 | make copy of length
        subq #1, %d2

| loop_back:      
|        bra loop_back
tst_loop:
        cmp.b (%a2)+, (%a3)+
        bne get_next_entry     
        dbra %d2, tst_loop              | if dbra is zero 
        
entry_found:    
        bra after_find
        
get_next_entry:
        tst.l (%a1)
        beq entry_not_found
        move.l (%a1), %a1
        bra _find
        
entry_not_found:
        move.l #0, %a1           | return null pointer               
        bra after_find
        

        DEFWORD "DOC",3,doc,0, "( -- ) Retrieves documentation for the next word in the input stream."
        bsr word
        bsr find
        POP %a0
        cmp #0, %a0      | null pointer?
        beq endDoc
        
        sub.l #4, %a0
        move.w (%a0), %a1 | address in a1
        
        beq endDoc
        subq.l #2, %a0

        move.l (%a0), %d1 | address in d1
        beq endDoc
        
        PUSH %a1
        PUSH %d1
        bsr tell
        bra cr
docUnavailableMsg:
        .ascii "Documentation unavailable."
        .balign 2
endDoc:
        PUSH #docUnavailableMsg
        PUSH #26
        bsr telln
        bra cr

        DEFWORD ">CFA", 4, tcfa, 0, "( addr -- cfaAddr ) Gets the code field address (start of 68k code) for a given word address."
        | header format
        | prev_link (4 bytes)
        | namelen (1 byte)
        | string .. (namelen bytes)
        | padding (0 or more bytes)
        moveq.l #0, %d1
        POP %a0                 | pointer to prev link in a0
        addq.l #4, %a0          | pointer to namelen
        move.b (%a0), %d1       |
        addq #1, %a0
        andi.b #F_LENMASK, %d1  | name len in d1
        add.l %a0, %d1          | pointer to next slot after name
        btst #0, %d1            | if least significant bit is 0, we're good
        beq tcfa_return_addr
        addq.l #1, %d1
tcfa_return_addr:       
        PUSH %d1
        rts



        DEFWORD "EXECUTE",7,execute,0, "( cfa -- ) Jumps to given cfa."
        POP %a0
        jsr (%a0)
        rts


        DEFWORD "WORDS",5,words,0, "( -- ) Prints a list of all defined words."
        bra words_start
after_words:    
        rts

words_start:    
        move.l (loc_latest), %a1      | address of latest header in a1      

words_loop:     
        move.b 4(%a1), %d2                   | get length in d2
        andi.b #F_LENMASK, %d2       | get length and hidden flag
        move.l %a1, %a2
        addq.l #5, %a2

        PUSH %a1
        PUSH %a2
        PUSH %d2

        bsr telln

        PUSH #' '
        bsr emit
        POP %a1
        
        tst.l (%a1)
        beq words_done
        move.l (%a1), %a1
        bra words_loop        

words_done:     
        bra after_words


        DEFWORD "CREATE",6,create,0, "( strAddr strLen -- ) Creates a new dictionary entry named by string at strAddr starting at CP."

        move.l (loc_cp), %a0                    | grab pointer to last entry
        move.l (loc_latest), %a1                | grab next word slot in dictionary
        POP %d1                                 | get name length
        POP %a2                                 | get name address

        | write empty doc fields
        move.w #0, (%a0)+                       | empty doc length
        move.l #0, (%a0)+                       | null docstring pointer
        
        
        move.l %a0, (loc_latest)                | update 'latest' pointer
        move.l %a1, (%a0)+                      | write link pointer
        move.b %d1, (%a0)+                      | write name length

        
        subq.l #1, %d1                            
_create_char_copy_loop:  
        move.b (%a2)+, (%a0)+                     | copy char byte
        dbra %d1, _create_char_copy_loop        | branch until d1 (length) == 0
        move.l %a0, %d1
        btst #0, %d1                            | if least significant bit is 0, we're aligned to 2byte boundary
        beq _create_update_cp
        move.b #0, (%a0)+
        |#addq.l #1, %a0
_create_update_cp:
        move.l %a0, (loc_cp)
        rts

        
        DEFWORD ",", 1,comma, 0, "( a -- ) Writes a longword value to CP, incrementing the address by 4."
        POP %d1
        move.l (loc_cp), %a0                  | get 'cp' ptr
        move.l %d1, (%a0)+                    | write 32-bit val
        move.l %a0, (loc_cp)                  | increment 'cp' by 4
        rts

        DEFWORD "W,",2,wordComma,0, "( a -- ) Writes a word value to CP, incrementing the address by 2." 
        POP %d1
        move.l (loc_cp), %a0
        move.w %d1, (%a0)+
        move.l %a0, (loc_cp)
        rts

        DEFWORD "<JSR>",5,cjsr,0, "( addr -- ) Compiles a JSR instruction jumping control to given longword address. Increments CP by 6"
        | ( addr -- )
        POP %d1
        move.l (loc_cp), %a2
        move.w #0x4EB9, (%a2)+
        move.l %d1, (%a2)+
        move.l %a2, (loc_cp)
        rts

        
        DEFWORD "[PUSH]",6,compile_push,0, "( a -- ) Compiles a literal push of longword value a onto the stack. Increments CP by 8."
        | compiles a literal push onto the stack
        | todo, more safety
        PUSH #0x2D00
        bsr wordComma           | compile `move.l d0, -(a6)`
        PUSH #0x203C
        bsr wordComma           | compile PUSH dest (d0)   
        bra comma               | compile PUSH src (literal)
        
        

                
       
        
        DEFWORD "[",1,lbrac,0, "( -- ) Switches to interpret mode."
        | set state to 0, interpret mode
        move.b #0, (loc_state)
        rts
        
        DEFWORD "]",1,rbrac,0, "( -- ) Switches to compile mode."
        | set state to 1, compile mode
        move.b #1, (loc_state)
        rts
        
        DEFWORD ":",1,colon,0, "( -- ) Starts a new colon-word definition."
        bsr word
        bsr create
        bsr latest
        bsr fetch
        bsr hidden
        bra rbrac

        DEFWORD "EXIT",4,exit,0, "( -- ) Exits the current word."
        addq #4, %a7
        jmp (%a7) 
        
        
        DEFWORD ";",1,semicolon,F_IMMED, "( -- ) Ends a definition."
        PUSH #0x4E75
        bsr wordComma
        bsr latest
        bsr fetch
        bsr hidden
        bra lbrac

        DEFWORD "IMMEDIATE",9,immediate,F_IMMED, "( -- ) Toggles the immediate flag of the last defined word."
        move.l (loc_latest), %a0
        addq.l #4, %a0
        eor.b #F_IMMED, (%a0)
        rts

        DEFWORD "HIDDEN",6,hidden,0, "( a -- ) Toggles the hidden flag of the word at a."
        POP %a0
        addq.l #4, %a0
        eor.b #F_HIDDEN, (%a0)
        rts

        DEFWORD "HIDE",4,hide,0, "( -- ) Toggles the hidden flag of the next word in the input stream."
        bsr word
        bsr find
        bra hidden
        
        DEFWORD "'",1,tick,0, "( -- cfa ) Gets the cfa of the next word in the input stream."
        bsr word
        bsr find
        bra tcfa

        | todo implement for literal strings
        | DEFWORD "LITSTRING",9,litstring,0


        
        DEFWORD "QUIT",4,quit,0, "( -- ) Returns to the toplevel interepreter loop."
        move.l #0xFFFE00, %a7           | reset return stack
        bsr interpret
        bra quit
        
        
        DEFWORD "INTERPRET",9,interpret,0, "( -- ) Interprets the next word in the input stream."
        | read word
        bsr word                        | ( wordAddr wordLen )
        bsr tdup                        | ( wordAddr wordLen wordAddr wordLen )
        
        bsr find                        | ( wordAddr wordLen entryAddr )
        
        tst.l %d0                         | ( wordAddr wordLen entryAddr )         
        beq interp_not_in_dict

        | in dictionary, is it an immediate word?
interp_in_dict: 
        bsr nrot                        | ( entryAddr wordAddr wordLen )
        bsr tdrop                       | ( entryAddr )
        bsr dup
        bsr qimmediate                  | ( entryAddr entryIsImmediate )
        POP %d2
        tst.l %d2
        bne interp_execute              | if /= 0, immediate word
        
        tst.b (loc_state)                 
        bne interp_compile              | 1 == compile, 0 == immediate
                
interp_execute:
        bsr tcfa
        | bra execute
        POP %a0
        jmp (%a0)
        
        .balign 2
compile_string:
        .ascii "Compiled word "
        .balign 2
compile_number:
        .ascii "Compiled push "
        .balign 2
interp_compile:
        bsr tcfa
        bsr cjsr
        | PUSH #compile_string
        | PUSH #13
        | bra tell
        rts
        
        
        
interp_not_in_dict:
        bsr drop                                | ( wordAddr wordLen ) trash result of find
        | try to parse as number
        bsr tdup                                | ( wordAddr wordLen wordAddr wordLen )
        bsr number                              | ( wordAddr wordLen number success )
        | if number
        POP %d2
        tst.l %d2                                 | 0 == no-error
        bne interp_parse_error        

        |   if compiling, compile a push
        POP %d2
        bsr tdrop
        PUSH %d2
        move.l (loc_state), %d1                 | 1 == compile
        bne interp_compile_literal
        | otherwise, it's on the stack as if we pushed it directly, so just return
        rts

interp_compile_literal:         
        bsr compile_push                        | ( number -- )
        | PUSH #compile_number
        | PUSH #13
        | bra tell
        rts
        
        .balign 2
ip_error_string:
        .ascii "Couldnt parse "
        .balign 2
interp_parse_error:
        PUSH #ip_error_string
        PUSH #14
        bsr telln
        bsr telln
        rts

        DEFWORD "(",1,parenOpen,F_IMMED, "( -- ) Ignores all input until a ')' is found."
        move.l #1, %d1
parenLoop:
        RPUSH %d1        
        bsr word
        RPOP %d1
        POP %d2
        POP %a0  | trash word

        cmp.b #1, %d2
        bne parenLoop
        
        cmp.b #40, (%a0)
        beq incParen
        
        
        cmp.b #41, (%a0)
        beq decParen
        bra parenLoop

decParen:       
        subq #1, %d1
        bne parenLoop
        rts     
incParen:
        addq.l #1, %d1
        bra parenLoop
        

        | TODO figure out if this is necessary
|         DEFWORD "PAD",3,pad,0,
|         move.l (loc_cp), %d1
|         add.l #300, %d1
|         PUSH %d1
|         rts
        
|         DEFWORD "TSAVE",5,task_save_area,0
|         | gets the current task's save area
|         PUSH %a5
|         rts

|         DEFWORD "TBUFFER",7, terminal_input_buffer,0
|         PUSH %a5
|         add.l #140, %d0
|         rts

|         DEFWORD "TVAR",4, user_variable_area,0
|         PUSH %a5
|         add.l #(140+128),%d0
|         | get user_var_area

|         DEFWORD "SWITCH",6,switch,0
|         | ( task_num )
|         move.l #16, %d1

|         bsr depth
|         cmp.l %d1, %d0
|         bgt pstack_task_switch_error

|         bsr rdepth
|         cmp.l %d1, %d0
|         bgt rstack_task_switch_error 
        
        
|         bsr save_current_task_data   | shouldn't touch the current data stack
|         POP %a5
|         bsr load_current_task_data
|         rts

| pstack_task_switch_error_string:     
|         .ascii "Too much data on PSTACK to yield!"
| rstack_task_switch_error_string:     
|         .ascii "Too much data on RSTACK to yield!"
| pstack_task_switch_error:
|         PUSH #pstack_task_switch_error_string
|         PUSH #33
|         bsr telln
| ptse_lp:
|         bra ptse_lp
        
| rstack_task_switch_error:
|         PUSH #rstack_task_switch_error_string
|         PUSH #33
|         bsr telln
| rtse_lp:
|         bra rtse_lp 
        
| save_current_task_data:
|         | save data for old task
|         | %a5 points to task save area
|         move.l %a5, %a4                 | temp
|         | save registers
|         movem.l %d0/%a6/%a7, (%a4)
|         | save top 16 elements of return stack
|         | and top 16 elements of param stack
|         add.l #12, %a4
        
|         moveq.l #3, %d1
|         | needs to handle less than 16 elements
| copy_rstack_loop:               
|         move.l (%a7)+, (%a4)+
|         move.l (%a7)+, (%a4)+
|         move.l (%a7)+, (%a4)+
|         move.l (%a7)+, (%a4)+
|         dbra %d1, copy_rstack_loop

|         moveq.l #3, %d1
| copy_pstack_loop:
|         | needs to handle less than 16 elements 
|         move.l %d0, (%a4)+
|         move.l (%a6)+, %d0
|         move.l %d0, (%a4)+
|         move.l (%a6)+, %d0
|         move.l %d0, (%a4)+
|         move.l (%a6)+, %d0
|         move.l %d0, (%a4)+
|         move.l (%a6)+, %d0
|         dbra %d1, copy_pstack_loop

|         | reset return stack to base
|         move.l %a7, RSTACK_END

load_current_task_data:
        | load registers
        | load data stack
        | load return stack

init_latest:    
        PUSH #LAST_WORD
        move.l #LAST_WORD, (loc_latest)
        rts

init_cp:        
        move.l #start_user_dict, (loc_cp)
        rts

init_uvo:
        move.l #USER_VAR_OFFSET, (loc_user_var_offset)
        rts

init_up:
        move.l #user_variables_start, %a5
        rts

init_ucnt:
        move.l #USER_VAR_COUNT, (loc_user_var_count)

        
start_user_dict:        
        .space 32768, 'D'                       | 32kB space for dictionary


       
user_variables_start:   

        .space 15360, 'U'               | space for 20 user tasks

        
        | Task Save Area, 140 bytes
        | for saving
        | d0, a6, a7  == 12 bytes
        | 32 return stack addresses
        
        | Terminal Input Buffer, 128 bytes
        | User variables area, 128 bytes
        | HOLD area, 40 bytes
        | PAD buffer, 88 bytes
        | Leave stack, 128 bytes
user_variables_end:     
        
