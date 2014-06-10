primitive (code) words
-----

CHAR (gets character code of a character following this word in the iinput stream)

LITSTRING   PUSHES a string address on te stack (usually the string is compiled inline in the instruction stream) 


colon words
-----------

'\n', BL, CR, SPACE

TRUE, FALSE, NOT

various character constants ('A', '0', '(', ')', '[', ']', ':', ';', etc)



immediate/compile and parsing words
---------------------------

\[COMPILE\] (compiles an immediate word into dictionary entry currently being compiled, aka POSTPONE)

RECURSE (compiles a literal jump to the start of the dictionary entry currently being compiled)

IF/UNLESS/ELSE/THEN (which compile to native branch instructions)

BEGIN, UNTIL, REPEAT, DO, etc (loop words)

(, ) (comments are delimited by parens)



clean up input code
