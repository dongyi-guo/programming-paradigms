grammar Scanner;
prog:   'let' IDENT '=' term
    |   term
    ;
term:   factor
    |   '+' factor
    |   '-' factor
    ;
factor: primitive
    |   '*' primitive
    |   '/' primitive
    ;
primitive: IDENT 
    |    NUMBER
    |   '(' term ')'
    ;
IDENT:  [a-zA-Z]+;
NUMBER: [0-9]+;
WS: [ \t\r\n]+ -> skip;
