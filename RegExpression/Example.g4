grammar Example;

statement:KW_BEGIN statement_list KW_END
| KW_SEQ statement
| KW_PAR statement
| KW_SEND
| KW_RECEIVE
;
statement_list : statement (';' statement)*
    ;

KW_BEGIN: 'begin';
KW_END: 'end';
KW_SEQ: 'seq';
KW_PAR: 'par';
KW_SEND: 'send';
KW_RECEIVE: 'receive';
WS : [\t\r\n]+ -> skip;
