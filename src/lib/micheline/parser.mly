
%{
  open Node

  let pos = Loc.loc_of_lexbuf_positions
%}

%token <Bigint.t> NUM
%token <Bytes.t> HEX
%token <string> STRING
%token <string> IDENT
%token <string> ANNOT
%token LB RB LP RP
%token SEMICOLON MINUS 
%token EOF

%start <(Loc.t, string) node> start

%%

start:
    LB e_1=application SEMICOLON e_2=application SEMICOLON e_3=application SEMICOLON? RB EOF
  | e_1=application SEMICOLON e_2=application SEMICOLON e_3=application SEMICOLON? EOF
      { Seq (pos $startpos $endpos, [e_1; e_2; e_3]) }

application:
    p=IDENT annots=ANNOT* args=arg*
      { Prim (pos $startpos(p) $endpos(p), p, args, annots) }

arg:
    LP a=application RP
      { a }
  | p=IDENT annots=ANNOT*
      { Prim (pos $startpos(p) $endpos(p), p, [], annots) }
  | e=expr_arg
      { e }

expr_arg:
    i=int
      { Int (pos $startpos $endpos, i) }
  | h=HEX
      { Bytes (pos $startpos $endpos, h) }
  | s=STRING
      { String (pos $startpos $endpos, s) }
  | LB e=seq RB
      { Seq (pos $startpos $endpos, e) }

expr:
    a=application { a }
  | e=expr_arg    { e }

int:
    n=NUM       { n }
  | MINUS n=NUM { Bigint.neg n }

seq:
      { [] }
  | e=expr
      { [e] }
  | e=expr SEMICOLON s=seq
      { e::s }
