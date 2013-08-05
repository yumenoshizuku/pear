type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | LITERAL of (int)
  | STRLIT of (string)
  | CHAR of (char)
  | ID of (string)
  | OBJECT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 37 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* LEQ *);
  272 (* GT *);
  273 (* GEQ *);
  274 (* RETURN *);
  275 (* IF *);
  276 (* ELSE *);
  277 (* FOR *);
  278 (* WHILE *);
  279 (* INT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  280 (* LITERAL *);
  281 (* STRLIT *);
  282 (* CHAR *);
  283 (* ID *);
  284 (* OBJECT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\003\000\003\000\005\000\005\000\
\004\000\004\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\008\000\008\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\009\000\009\000\010\000\
\010\000\000\000"

let yylen = "\002\000\
\000\000\004\000\003\000\007\000\000\000\001\000\002\000\004\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\003\000\000\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\002\000\007\000\000\000\000\000\009\000\
\000\000\000\000\008\000\000\000\004\000\009\000\000\000\000\000\
\000\000\000\000\021\000\022\000\023\000\000\000\000\000\010\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\000\013\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\000\000\027\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\017\000\000\000\000\000\000\000\015\000\
\000\000\000\000\016\000"

let yydgoto = "\002\000\
\003\000\005\000\010\000\018\000\011\000\032\000\033\000\060\000\
\064\000\065\000"

let yysindex = "\007\000\
\000\000\000\000\229\254\021\255\006\255\027\255\074\255\000\000\
\062\255\094\255\085\255\000\000\000\000\098\255\083\255\000\000\
\091\255\050\255\000\000\011\255\000\000\000\000\011\255\106\255\
\117\255\123\255\000\000\000\000\000\000\122\255\022\255\000\000\
\005\000\132\255\129\255\077\255\017\000\011\255\011\255\011\255\
\011\255\011\255\134\255\000\000\011\255\011\255\011\255\011\255\
\011\255\011\255\011\255\011\255\011\255\011\255\000\000\000\000\
\000\000\144\255\028\000\142\255\159\255\028\000\028\000\146\255\
\149\255\000\000\041\255\041\255\000\000\000\000\039\000\039\000\
\037\255\037\255\037\255\037\255\088\255\011\255\088\255\000\000\
\011\255\115\255\157\255\000\000\028\000\088\255\011\255\000\000\
\147\255\088\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\164\000\000\000\000\000\162\255\000\000\000\000\
\000\000\000\000\167\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\114\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\172\255\000\000\
\000\000\176\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\255\000\000\000\000\067\255\080\255\000\000\
\180\255\000\000\174\255\189\255\000\000\000\000\004\255\081\255\
\204\255\219\255\234\255\249\255\000\000\172\255\000\000\000\000\
\000\000\038\255\000\000\000\000\082\255\000\000\182\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\162\000\000\000\181\255\236\255\183\255\
\000\000\000\000"

let yytablesize = 312
let yytable = "\035\000\
\004\000\082\000\037\000\084\000\083\000\020\000\029\000\001\000\
\020\000\029\000\088\000\008\000\020\000\089\000\091\000\029\000\
\029\000\058\000\059\000\061\000\062\000\063\000\006\000\042\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\027\000\028\000\029\000\030\000\034\000\014\000\
\014\000\014\000\014\000\045\000\046\000\047\000\048\000\007\000\
\043\000\047\000\048\000\020\000\021\000\022\000\009\000\014\000\
\014\000\059\000\014\000\014\000\085\000\014\000\014\000\014\000\
\014\000\014\000\059\000\023\000\024\000\035\000\025\000\026\000\
\035\000\027\000\028\000\029\000\030\000\031\000\020\000\012\000\
\022\000\056\000\040\000\030\000\041\000\040\000\030\000\041\000\
\013\000\020\000\015\000\022\000\030\000\030\000\023\000\024\000\
\014\000\025\000\026\000\016\000\027\000\028\000\029\000\030\000\
\031\000\023\000\024\000\038\000\025\000\026\000\017\000\027\000\
\028\000\029\000\030\000\031\000\024\000\019\000\039\000\024\000\
\024\000\024\000\024\000\024\000\040\000\024\000\024\000\024\000\
\024\000\024\000\024\000\055\000\041\000\042\000\086\000\045\000\
\046\000\047\000\048\000\066\000\049\000\050\000\051\000\052\000\
\053\000\054\000\077\000\078\000\080\000\090\000\045\000\046\000\
\047\000\048\000\081\000\049\000\050\000\051\000\052\000\053\000\
\054\000\079\000\087\000\042\000\005\000\045\000\046\000\047\000\
\048\000\006\000\049\000\050\000\051\000\052\000\053\000\054\000\
\025\000\019\000\038\000\025\000\025\000\025\000\039\000\036\000\
\019\000\025\000\025\000\025\000\025\000\025\000\025\000\026\000\
\000\000\000\000\026\000\026\000\026\000\000\000\000\000\000\000\
\026\000\026\000\026\000\026\000\026\000\026\000\031\000\000\000\
\000\000\031\000\000\000\000\000\000\000\000\000\000\000\031\000\
\031\000\031\000\031\000\031\000\031\000\032\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\032\000\032\000\
\032\000\032\000\032\000\032\000\033\000\000\000\000\000\033\000\
\000\000\000\000\000\000\000\000\000\000\033\000\033\000\033\000\
\033\000\033\000\033\000\034\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
\034\000\034\000\044\000\045\000\046\000\047\000\048\000\000\000\
\049\000\050\000\051\000\052\000\053\000\054\000\057\000\045\000\
\046\000\047\000\048\000\000\000\049\000\050\000\051\000\052\000\
\053\000\054\000\045\000\046\000\047\000\048\000\000\000\049\000\
\050\000\051\000\052\000\053\000\054\000\045\000\046\000\047\000\
\048\000\000\000\000\000\000\000\051\000\052\000\053\000\054\000"

let yycheck = "\020\000\
\028\001\077\000\023\000\079\000\078\000\003\001\003\001\001\000\
\006\001\006\001\086\000\006\001\002\001\087\000\090\000\012\001\
\013\001\038\000\039\000\040\000\041\000\042\000\002\001\002\001\
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\024\001\025\001\026\001\027\001\028\001\002\001\
\003\001\004\001\005\001\007\001\008\001\009\001\010\001\027\001\
\027\001\009\001\010\001\002\001\003\001\004\001\028\001\018\001\
\019\001\078\000\021\001\022\001\081\000\024\001\025\001\026\001\
\027\001\028\001\087\000\018\001\019\001\003\001\021\001\022\001\
\006\001\024\001\025\001\026\001\027\001\028\001\002\001\006\001\
\004\001\005\001\003\001\003\001\003\001\006\001\006\001\006\001\
\027\001\002\001\006\001\004\001\012\001\013\001\018\001\019\001\
\003\001\021\001\022\001\002\001\024\001\025\001\026\001\027\001\
\028\001\018\001\019\001\002\001\021\001\022\001\028\001\024\001\
\025\001\026\001\027\001\028\001\003\001\027\001\002\001\006\001\
\007\001\008\001\009\001\010\001\002\001\012\001\013\001\014\001\
\015\001\016\001\017\001\003\001\011\001\002\001\020\001\007\001\
\008\001\009\001\010\001\006\001\012\001\013\001\014\001\015\001\
\016\001\017\001\003\001\006\001\003\001\003\001\007\001\008\001\
\009\001\010\001\006\001\012\001\013\001\014\001\015\001\016\001\
\017\001\003\001\006\001\000\000\003\001\007\001\008\001\009\001\
\010\001\003\001\012\001\013\001\014\001\015\001\016\001\017\001\
\003\001\006\001\003\001\006\001\007\001\008\001\003\001\022\000\
\003\001\012\001\013\001\014\001\015\001\016\001\017\001\003\001\
\255\255\255\255\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\003\001\255\255\255\255\006\001\
\255\255\255\255\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\003\001\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\006\001\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\006\001\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\007\001\008\001\009\001\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\014\001\015\001\016\001\017\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  STRLIT\000\
  CHAR\000\
  ID\000\
  OBJECT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
                 ( [], [] )
# 263 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 29 "parser.mly"
                           ( (_3 :: fst _1), snd _1 )
# 272 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    Obj.repr(
# 30 "parser.mly"
                       ( fst _1, (_2 :: snd _1) )
# 280 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 34 "parser.mly"
     ( { fname = _1;
	 formals = _3;
	 (*locals = List.rev $6;*)
	 body = List.rev _6 } )
# 292 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                  ( [] )
# 298 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 41 "parser.mly"
                  ( List.rev _1 )
# 305 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                                ( [(_1^" "^_2)] )
# 313 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                                ( (_3^" "^_4) :: _1 )
# 322 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                   ( [] )
# 328 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                   ( _2 :: _1 )
# 336 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
               ( Expr(_1) )
# 343 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                      ( Return(_2) )
# 350 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                            ( Block(List.rev _2) )
# 357 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 365 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "parser.mly"
                                            ( If(_3, _5, _7) )
# 374 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 384 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 66 "parser.mly"
                                  ( While(_3, _5) )
# 392 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "parser.mly"
                    ( Declare(_1, _2) )
# 400 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                  ( Noexpr )
# 406 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                  ( _1 )
# 413 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
                     ( Literal(_1) )
# 420 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
                     ( StrLit(_1) )
# 427 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 76 "parser.mly"
                     ( Char(_1) )
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                     ( Id(_1) )
# 441 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 449 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 457 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 465 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 473 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 481 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 489 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 497 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 505 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 513 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 521 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                     ( Assign(_1, _3) )
# 529 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 89 "parser.mly"
                                     ( Call(_1, _3) )
# 537 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                       ( _2 )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                  ( [] )
# 550 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 94 "parser.mly"
                  ( List.rev _1 )
# 557 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                            ( [_1] )
# 564 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                            ( _3 :: _1 )
# 572 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
