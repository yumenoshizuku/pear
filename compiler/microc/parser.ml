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
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 35 "parser.ml"
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
  282 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\010\000\010\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\011\000\011\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\002\000\003\000\000\000\
\000\000\011\000\007\000\000\000\000\000\000\000\000\000\009\000\
\008\000\000\000\010\000\000\000\000\000\012\000\004\000\000\000\
\000\000\000\000\000\000\023\000\024\000\000\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\000\016\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\000\000\000\000\000\000\020\000\
\000\000\000\000\000\000\018\000\000\000\000\000\019\000"

let yydgoto = "\002\000\
\003\000\006\000\007\000\012\000\018\000\020\000\013\000\031\000\
\032\000\057\000\060\000\061\000"

let yysindex = "\005\000\
\000\000\000\000\238\254\242\254\032\255\000\000\000\000\035\255\
\017\255\000\000\000\000\045\255\059\255\066\255\047\255\000\000\
\000\000\052\255\000\000\033\255\007\255\000\000\000\000\007\255\
\075\255\077\255\080\255\000\000\000\000\255\254\000\000\144\255\
\022\000\042\255\161\255\007\255\007\255\007\255\007\255\007\255\
\000\000\007\255\007\255\007\255\007\255\007\255\007\255\007\255\
\007\255\007\255\007\255\000\000\000\000\000\000\037\000\063\000\
\083\255\052\000\063\000\087\255\102\255\063\000\040\255\040\255\
\000\000\000\000\074\000\074\000\104\255\104\255\104\255\104\255\
\101\255\007\255\101\255\000\000\007\255\090\255\106\255\000\000\
\063\000\101\255\007\255\000\000\114\255\101\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\124\000\000\000\000\000\000\000\000\000\000\000\
\126\255\000\000\000\000\000\000\128\255\000\000\000\000\000\000\
\000\000\067\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\127\255\000\000\000\000\
\000\000\000\000\000\000\000\000\131\255\000\000\135\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\255\
\000\000\000\000\093\255\000\000\143\255\039\255\178\255\195\255\
\000\000\000\000\001\255\103\255\212\255\229\255\246\255\007\000\
\000\000\131\255\000\000\000\000\000\000\076\255\000\000\000\000\
\115\255\000\000\145\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\129\000\000\000\000\000\000\000\127\000\000\000\001\000\
\235\255\193\255\000\000\000\000"

let yytablesize = 347
let yytable = "\033\000\
\039\000\030\000\035\000\030\000\004\000\001\000\030\000\005\000\
\021\000\040\000\079\000\008\000\030\000\030\000\055\000\056\000\
\058\000\059\000\062\000\085\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\028\000\029\000\
\030\000\009\000\021\000\010\000\022\000\023\000\022\000\036\000\
\022\000\036\000\011\000\021\000\036\000\022\000\053\000\014\000\
\044\000\045\000\024\000\025\000\056\000\026\000\027\000\081\000\
\028\000\029\000\030\000\024\000\025\000\056\000\026\000\027\000\
\015\000\028\000\029\000\030\000\012\000\016\000\012\000\012\000\
\017\000\078\000\004\000\080\000\036\000\017\000\037\000\017\000\
\017\000\038\000\084\000\074\000\012\000\012\000\087\000\012\000\
\012\000\076\000\012\000\012\000\012\000\017\000\017\000\041\000\
\017\000\017\000\041\000\017\000\017\000\017\000\021\000\031\000\
\022\000\031\000\083\000\077\000\031\000\082\000\042\000\043\000\
\044\000\045\000\031\000\031\000\086\000\042\000\024\000\025\000\
\042\000\026\000\027\000\043\000\028\000\029\000\030\000\025\000\
\005\000\025\000\006\000\021\000\025\000\025\000\025\000\025\000\
\025\000\039\000\025\000\025\000\025\000\025\000\025\000\025\000\
\041\000\040\000\019\000\021\000\034\000\000\000\042\000\043\000\
\044\000\045\000\000\000\046\000\047\000\048\000\049\000\050\000\
\051\000\054\000\000\000\000\000\000\000\000\000\000\000\042\000\
\043\000\044\000\045\000\000\000\046\000\047\000\048\000\049\000\
\050\000\051\000\026\000\000\000\026\000\000\000\000\000\026\000\
\026\000\026\000\000\000\000\000\000\000\026\000\026\000\026\000\
\026\000\026\000\026\000\027\000\000\000\027\000\000\000\000\000\
\027\000\027\000\027\000\000\000\000\000\000\000\027\000\027\000\
\027\000\027\000\027\000\027\000\032\000\000\000\032\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\032\000\
\032\000\032\000\032\000\032\000\032\000\033\000\000\000\033\000\
\000\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
\033\000\033\000\033\000\033\000\033\000\033\000\034\000\000\000\
\034\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\035\000\
\000\000\035\000\000\000\000\000\035\000\000\000\000\000\000\000\
\000\000\000\000\035\000\035\000\035\000\035\000\035\000\035\000\
\052\000\000\000\000\000\000\000\042\000\043\000\044\000\045\000\
\000\000\046\000\047\000\048\000\049\000\050\000\051\000\073\000\
\000\000\000\000\000\000\042\000\043\000\044\000\045\000\000\000\
\046\000\047\000\048\000\049\000\050\000\051\000\075\000\000\000\
\000\000\000\000\042\000\043\000\044\000\045\000\000\000\046\000\
\047\000\048\000\049\000\050\000\051\000\042\000\043\000\044\000\
\045\000\000\000\046\000\047\000\048\000\049\000\050\000\051\000\
\042\000\043\000\044\000\045\000\000\000\000\000\000\000\048\000\
\049\000\050\000\051\000"

let yycheck = "\021\000\
\002\001\001\001\024\000\003\001\023\001\001\000\006\001\026\001\
\002\001\011\001\074\000\026\001\012\001\013\001\036\000\037\000\
\038\000\039\000\040\000\083\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\024\001\025\001\
\026\001\002\001\002\001\001\001\004\001\005\001\001\001\001\001\
\003\001\003\001\026\001\002\001\006\001\004\001\005\001\003\001\
\009\001\010\001\018\001\019\001\074\000\021\001\022\001\077\000\
\024\001\025\001\026\001\018\001\019\001\083\000\021\001\022\001\
\006\001\024\001\025\001\026\001\002\001\004\001\004\001\005\001\
\026\001\073\000\023\001\075\000\002\001\002\001\002\001\004\001\
\005\001\002\001\082\000\001\001\018\001\019\001\086\000\021\001\
\022\001\003\001\024\001\025\001\026\001\018\001\019\001\003\001\
\021\001\022\001\006\001\024\001\025\001\026\001\002\001\001\001\
\004\001\003\001\001\001\006\001\006\001\020\001\007\001\008\001\
\009\001\010\001\012\001\013\001\003\001\003\001\018\001\019\001\
\006\001\021\001\022\001\000\000\024\001\025\001\026\001\001\001\
\003\001\003\001\003\001\001\001\006\001\007\001\008\001\009\001\
\010\001\003\001\012\001\013\001\014\001\015\001\016\001\017\001\
\001\001\003\001\018\000\003\001\022\000\255\255\007\001\008\001\
\009\001\010\001\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\001\001\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\001\001\255\255\003\001\255\255\255\255\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\001\001\
\255\255\003\001\255\255\255\255\006\001\255\255\255\255\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\003\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001"

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
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
                 ( [], [] )
# 264 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 27 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 272 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 28 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 280 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 32 "parser.mly"
     ( { fname = _1;
	 formals = _3;
	 locals = List.rev _6;
	 body = List.rev _7 } )
# 293 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                  ( [] )
# 299 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 39 "parser.mly"
                  ( List.rev _1 )
# 306 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
                         ( [_1] )
# 313 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
                         ( _3 :: _1 )
# 321 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                     ( [] )
# 327 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 47 "parser.mly"
                     ( _2 :: _1 )
# 335 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 50 "parser.mly"
               ( _2 )
# 342 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                   ( [] )
# 348 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 54 "parser.mly"
                   ( _2 :: _1 )
# 356 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
              ( Expr(_1) )
# 363 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                     ( Return(_2) )
# 370 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 59 "parser.mly"
                            ( Block(List.rev _2) )
# 377 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 385 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                                            ( If(_3, _5, _7) )
# 394 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 404 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
                                  ( While(_3, _5) )
# 412 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                  ( Noexpr )
# 418 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                  ( _1 )
# 425 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "parser.mly"
                     ( Literal(_1) )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                     ( StrLit(_1) )
# 439 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                     ( Id(_1) )
# 446 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 454 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 462 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 470 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 478 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 486 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 494 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 502 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 510 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 518 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 526 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                     ( Assign(_1, _3) )
# 534 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 85 "parser.mly"
                                 ( Call(_1, _3) )
# 542 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                       ( _2 )
# 549 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                  ( [] )
# 555 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 90 "parser.mly"
                  ( List.rev _1 )
# 562 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                            ( [_1] )
# 569 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                            ( _3 :: _1 )
# 577 "parser.ml"
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
