
type binop = Add | Sub | Mul | Div | Conj | Disj | Equal
type uniop = Neg

type expr=
	Literal of int
	| Id of string
	| Binop of expr * binop * expr
    	| Uniop of uniop * expr
	| Assign of string * expr

type stmt = Block of stmt list
	 | Expr of expr
	 | Return of expr
	 | If of expr * stmt * stmt

type program = string list 
