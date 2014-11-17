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
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\010\000\010\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\000\000\003\000\003\000\008\000\000\000\001\000\002\000\004\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\003\000\000\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\003\000\000\000\000\000\000\000\011\000\007\000\000\000\
\000\000\009\000\000\000\000\000\008\000\000\000\010\000\000\000\
\000\000\004\000\012\000\000\000\000\000\000\000\000\000\024\000\
\025\000\026\000\000\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\016\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\000\000\
\000\000\030\000\031\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\018\000\000\000\000\000\019\000"

let yydgoto = "\002\000\
\003\000\005\000\006\000\012\000\020\000\024\000\013\000\037\000\
\038\000\065\000\069\000\070\000"

let yysindex = "\007\000\
\000\000\000\000\229\254\021\255\006\255\049\255\052\255\083\255\
\000\000\000\000\064\255\094\255\102\255\000\000\000\000\098\255\
\097\255\000\000\084\255\104\255\000\000\108\255\000\000\050\255\
\011\255\000\000\000\000\011\255\136\255\142\255\143\255\000\000\
\000\000\000\000\148\255\022\255\000\000\114\255\149\255\155\255\
\077\255\031\000\011\255\011\255\011\255\011\255\011\255\154\255\
\000\000\011\255\011\255\011\255\011\255\011\255\011\255\011\255\
\011\255\011\255\011\255\000\000\000\000\000\000\170\255\042\000\
\160\255\185\255\042\000\042\000\158\255\168\255\000\000\041\255\
\041\255\000\000\000\000\053\000\053\000\037\255\037\255\037\255\
\037\255\088\255\011\255\088\255\000\000\011\255\156\255\169\255\
\000\000\042\000\088\255\011\255\000\000\186\255\088\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\181\000\000\000\000\000\000\000\187\255\000\000\
\000\000\000\000\000\000\000\000\188\255\000\000\000\000\000\000\
\000\000\000\000\000\000\115\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\140\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\190\255\000\000\000\000\201\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\255\
\000\000\000\000\067\255\080\255\000\000\202\255\000\000\200\255\
\215\255\000\000\000\000\004\255\081\255\230\255\245\255\004\000\
\019\000\000\000\190\255\000\000\000\000\000\000\038\255\000\000\
\000\000\082\255\000\000\206\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\190\000\000\000\000\000\000\000\184\000\000\000\176\255\
\231\255\178\255\000\000\000\000"

let yytablesize = 326
let yytable = "\040\000\
\004\000\087\000\042\000\089\000\088\000\023\000\032\000\001\000\
\023\000\032\000\093\000\009\000\025\000\094\000\096\000\032\000\
\032\000\063\000\064\000\066\000\067\000\068\000\007\000\047\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\032\000\033\000\034\000\035\000\039\000\017\000\
\017\000\017\000\017\000\050\000\051\000\052\000\053\000\008\000\
\048\000\052\000\053\000\025\000\026\000\027\000\010\000\017\000\
\017\000\064\000\017\000\017\000\090\000\017\000\017\000\017\000\
\017\000\017\000\064\000\028\000\029\000\038\000\030\000\031\000\
\038\000\032\000\033\000\034\000\035\000\036\000\025\000\011\000\
\027\000\061\000\043\000\033\000\044\000\043\000\033\000\044\000\
\014\000\025\000\015\000\027\000\033\000\033\000\028\000\029\000\
\016\000\030\000\031\000\018\000\032\000\033\000\034\000\035\000\
\036\000\028\000\029\000\017\000\030\000\031\000\021\000\032\000\
\033\000\034\000\035\000\036\000\012\000\012\000\012\000\049\000\
\050\000\051\000\052\000\053\000\019\000\054\000\055\000\056\000\
\057\000\058\000\059\000\022\000\012\000\012\000\008\000\012\000\
\012\000\043\000\012\000\012\000\012\000\012\000\027\000\044\000\
\045\000\027\000\027\000\027\000\027\000\027\000\047\000\027\000\
\027\000\027\000\027\000\027\000\027\000\060\000\046\000\071\000\
\085\000\050\000\051\000\052\000\053\000\083\000\054\000\055\000\
\056\000\057\000\058\000\059\000\082\000\086\000\092\000\091\000\
\050\000\051\000\052\000\053\000\045\000\054\000\055\000\056\000\
\057\000\058\000\059\000\084\000\095\000\005\000\006\000\050\000\
\051\000\052\000\053\000\022\000\054\000\055\000\056\000\057\000\
\058\000\059\000\028\000\041\000\042\000\028\000\028\000\028\000\
\022\000\023\000\041\000\028\000\028\000\028\000\028\000\028\000\
\028\000\029\000\000\000\000\000\029\000\029\000\029\000\000\000\
\000\000\000\000\029\000\029\000\029\000\029\000\029\000\029\000\
\034\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\035\000\
\000\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
\035\000\035\000\035\000\035\000\035\000\035\000\036\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\036\000\
\036\000\036\000\036\000\036\000\036\000\037\000\000\000\000\000\
\037\000\000\000\000\000\000\000\000\000\000\000\037\000\037\000\
\037\000\037\000\037\000\037\000\062\000\050\000\051\000\052\000\
\053\000\000\000\054\000\055\000\056\000\057\000\058\000\059\000\
\050\000\051\000\052\000\053\000\000\000\054\000\055\000\056\000\
\057\000\058\000\059\000\050\000\051\000\052\000\053\000\000\000\
\000\000\000\000\056\000\057\000\058\000\059\000"

let yycheck = "\025\000\
\028\001\082\000\028\000\084\000\083\000\003\001\003\001\001\000\
\006\001\006\001\091\000\006\001\002\001\092\000\095\000\012\001\
\013\001\043\000\044\000\045\000\046\000\047\000\002\001\002\001\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\024\001\025\001\026\001\027\001\028\001\002\001\
\003\001\004\001\005\001\007\001\008\001\009\001\010\001\027\001\
\027\001\009\001\010\001\002\001\003\001\004\001\006\001\018\001\
\019\001\083\000\021\001\022\001\086\000\024\001\025\001\026\001\
\027\001\028\001\092\000\018\001\019\001\003\001\021\001\022\001\
\006\001\024\001\025\001\026\001\027\001\028\001\002\001\028\001\
\004\001\005\001\003\001\003\001\003\001\006\001\006\001\006\001\
\006\001\002\001\027\001\004\001\012\001\013\001\018\001\019\001\
\003\001\021\001\022\001\002\001\024\001\025\001\026\001\027\001\
\028\001\018\001\019\001\006\001\021\001\022\001\027\001\024\001\
\025\001\026\001\027\001\028\001\002\001\003\001\004\001\006\001\
\007\001\008\001\009\001\010\001\028\001\012\001\013\001\014\001\
\015\001\016\001\017\001\028\001\018\001\019\001\027\001\021\001\
\022\001\002\001\024\001\025\001\026\001\027\001\003\001\002\001\
\002\001\006\001\007\001\008\001\009\001\010\001\002\001\012\001\
\013\001\014\001\015\001\016\001\017\001\003\001\011\001\006\001\
\003\001\007\001\008\001\009\001\010\001\006\001\012\001\013\001\
\014\001\015\001\016\001\017\001\003\001\006\001\006\001\020\001\
\007\001\008\001\009\001\010\001\000\000\012\001\013\001\014\001\
\015\001\016\001\017\001\003\001\003\001\003\001\003\001\007\001\
\008\001\009\001\010\001\006\001\012\001\013\001\014\001\015\001\
\016\001\017\001\003\001\003\001\003\001\006\001\007\001\008\001\
\003\001\020\000\027\000\012\001\013\001\014\001\015\001\016\001\
\017\001\003\001\255\255\255\255\006\001\007\001\008\001\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\006\001\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001"

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
# 267 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 29 "parser.mly"
                       ( (_2 :: fst _1), snd _1 )
# 275 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    Obj.repr(
# 30 "parser.mly"
                       ( fst _1, (_2 :: snd _1) )
# 283 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 34 "parser.mly"
     ( { fname = _1;
	 formals = _3;
	 (*locals = List.rev $6;*)
	 body = List.rev _6 } )
# 296 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                  ( [] )
# 302 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 41 "parser.mly"
                  ( List.rev _1 )
# 309 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                                ( [(_1^" "^_2)] )
# 317 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                                ( (_3^" "^_4) :: _1 )
# 326 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                     ( [] )
# 332 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 49 "parser.mly"
                     ( _2 :: _1 )
# 340 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
                   ( _2 )
# 348 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                   ( [] )
# 354 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                   ( _2 :: _1 )
# 362 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
               ( Expr(_1) )
# 369 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                      ( Return(_2) )
# 376 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                            ( Block(List.rev _2) )
# 383 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 391 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "parser.mly"
                                            ( If(_3, _5, _7) )
# 400 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 410 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 66 "parser.mly"
                                  ( While(_3, _5) )
# 418 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "parser.mly"
                    ( Declare(_1, _2) )
# 426 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                  ( Noexpr )
# 432 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                  ( _1 )
# 439 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
                     ( Literal(_1) )
# 446 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
                     ( StrLit(_1) )
# 453 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 76 "parser.mly"
                     ( Char(_1) )
# 460 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                     ( Id(_1) )
# 467 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 475 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 483 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 491 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 499 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 507 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 515 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 523 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 531 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 539 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 547 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                     ( Assign(_1, _3) )
# 555 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 89 "parser.mly"
                                     ( Call(_1, _3) )
# 563 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                       ( _2 )
# 570 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                  ( [] )
# 576 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 94 "parser.mly"
                  ( List.rev _1 )
# 583 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                            ( [_1] )
# 590 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                            ( _3 :: _1 )
# 598 "parser.ml"
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
