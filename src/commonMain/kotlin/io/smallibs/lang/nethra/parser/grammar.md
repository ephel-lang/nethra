top ::= 
    "type" ID "=" type
    "sig"  ID ":" type
    "def"  ID "=" expr 

type ::=
    ptype "->" type    
    stype ("-> type)?

ptype ::=
    "(" id ":" type ")"
    "{" id ":" type "}"

stype ::=
    "*"
    "int"
    "char"
    "(" type ")"



