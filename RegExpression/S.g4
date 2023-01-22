grammar S;

statement: statement ';' statement
        | IDENT ':=' expr
        | 'while' expr statement
        ;
expr: expr '-' expr
    | IDENT
    | NUMBER
    ;

IDENT:  [a-zA-Z]+;
NUMBER: [0-9]+;
WS: [ \t\r\n]+ -> skip;
