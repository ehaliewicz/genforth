primitive (code) words
-----

CHAR (gets character code of a character following this word in the iinput stream)

FIND (searches dictionary for matching entries)

>CFA (gets code address from dictionary header)

CREATE (creates a dictionary header)

, (comma) compiles a word address into a jump instruction at the current dictionary slot

[, ], :, and ;  (various tools for handling dictionary entries)

IMMEDIATE, HIDDEN, HIDE (for setting flags on dictonary entries)

' (tick) gets code address of word

EXECUTE (executes a code address)

LITSTRING   PUSHES a string address on te stack (usually the string is compiled inline in the instruction stream) 

QUIT        (returns to interpreter)

INTERPRET   (evaluates lines of input, finds words (jumping to them or compiling their addresses) or numbers (either pushing them on stack or compiling to inline pushes) 


colon words
-----------

'\n', BL, CR, SPACE

TRUE, FALSE, NOT

various character constants ('A', '0', '(', ')', '[', ']', ':', ';', etc)



immediate/compile and parsing words
---------------------------

[COMPILE] (compiles an immediate word into dictionary entry currently being compiled, aka POSTPONE)

RECURSE (compiles a literal jump to the start of the dictionary entry currently being compiled)

IF/UNLESS/ELSE/THEN (which compile to native branch instructions)

BEGIN, UNTIL, REPEAT, DO, etc (loop words)

(, ) (comments are delimited by parens)
