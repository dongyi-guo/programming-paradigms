grammar Exercise;
expr: term1 term2
    ;
term1: func func2 ;
term2: '+' term1 term2
    | '-' term1 term2
    |
    ;
func2: '*' func func2
    | '/' func func2
    |
    ;
func:  '-' func
    | '(' expr ')'
    | NUMBER
    ;

NUMBER: [0-9]+;
WS: [ \t\r\n]+ -> skip;
