%token<int> NUMBER
%token<string> IDENT
%token COLON LAMBDA RIGHT_ARROW UNIVERSE DOT
%token LPAREN RPAREN
%token EOF

%type<Abs.expr option> top

%left IDENT
%left RIGHT_ARROW
%left LPAREN RPAREN

%start top

%%

top
    : expr DOT { Some $1 }
    | EOF { None }
    ;

expr
    : UNIVERSE { Uni 0 }
    | UNIVERSE NUMBER { Uni $2 }
    | IDENT { Var ($1, 0) }
    | expr IDENT { App ($1, Var ($2, 0)) }
    | lambda { $1 }
    | expr LPAREN lambda RPAREN { App ($1, $3) }
    | LPAREN expr RPAREN { $2 }
    ;

lambda
    : LAMBDA LPAREN IDENT COLON expr RPAREN RIGHT_ARROW expr
    { Lam ($3, $5, $8) }
    ;

%%
