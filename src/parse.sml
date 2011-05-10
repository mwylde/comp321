structure L1Parse =
struct

structure T = Tokens
structure A = Ast

datatype Operator = NoOp | Plus | Minus | Times | Div | Neg |
                    Lt | Leq | Gt | Geq | Eq | Neq | And | Or |
                    Cons | Not | Head | Tail | 
                    If | Then | Else | Endif

datatype Associativity = Left | Right
datatype Arrity = Unary | Binary

exception parse_error of string
exception undefined_error
exception unhandled_op
exception unexpected_token

fun opPrec NoOp  = 0
  | opPrec (If | Then | Else | Endif) = 0
  | opPrec (And | Or) = 10
  | opPrec (Lt | Leq | Gt | Geq | Eq | Neq) = 20
  | opPrec Cons = 30
  | opPrec (Plus | Minus) = 40
  | opPrec (Times | Div) = 50
  | opPrec (Neg | Head | Tail | Not) = 60

fun opAssoc (NoOp | Plus | Minus | Times | Div)         = Left
  | opAssoc (And | Or | Lt | Leq | Gt | Geq | Eq | Neq) = Left
  | opAssoc (Neg | Head | Tail | Not | Cons)            = Right
  | opAssoc (If | Then | Else | Endif)                  = Left

fun opArrity (NoOp | Plus | Minus | Times | Div | Cons)  = Binary
  | opArrity (And | Or | Lt | Leq | Gt | Geq | Eq | Neq) = Binary
  | opArrity (Neg | Head | Tail | Not)                   = Unary
  | opArrity (If | Then | Else | Endif)                  = Binary

fun opFromBinOp A.PLUS  = Plus
  | opFromBinOp A.SUB   = Minus
  | opFromBinOp A.TIMES = Times
  | opFromBinOp A.DIV   = Div
  | opFromBinOp A.LT    = Lt
  | opFromBinOp A.LE    = Leq
  | opFromBinOp A.GT    = Gt
  | opFromBinOp A.GE    = Geq
  | opFromBinOp A.EQ    = Eq
  | opFromBinOp A.NE    = Neq
  | opFromBinOp A.AND   = And
  | opFromBinOp A.OR    = Or
  | opFromBinOp A.CONS  = Cons

fun binOpFromOp Plus  = A.PLUS
  | binOpFromOp Minus = A.SUB
  | binOpFromOp Times = A.TIMES
  | binOpFromOp Div   = A.DIV
  | binOpFromOp Lt    = A.LT
  | binOpFromOp Leq   = A.LE
  | binOpFromOp Gt    = A.GT
  | binOpFromOp Geq   = A.GE
  | binOpFromOp Eq    = A.EQ
  | binOpFromOp Neq   = A.NE
  | binOpFromOp And   = A.AND
  | binOpFromOp Or    = A.OR
  | binOpFromOp Cons  = A.CONS
  | binOpFromOp _ = raise unhandled_op

fun opFromUnOp A.NEG  = Neg
  | opFromUnOp A.NOT  = Not
  | opFromUnOp A.HEAD = Head
  | opFromUnOp A.TAIL = Tail

fun unOpFromOp Neg = A.NEG
  | unOpFromOp Not = A.NOT
  | unOpFromOp Head = A.HEAD
  | unOpFromOp Tail = A.TAIL
  | unOpFromOp _ = raise unhandled_op
                       

fun force_op ps op' =
    let
        fun force_unop (x::trees, ops) unop =
            ((A.UnOp(unOpFromOp unop, x))::trees, ops)
          | force_unop _ _ = raise (parse_error "need argument")

        fun force_binop (x::y::trees, ops) binop =
            ((A.BinOp(binOpFromOp binop, y, x))::trees, ops)
          | force_binop _ _ = raise (parse_error "need argument")
    in
        case (opArrity op') of Unary => force_unop ps op'
                             | Binary => force_binop ps op'
    end

fun force_ops (trees, []) = (trees, [])
  | force_ops (trees, op'::[]) = force_op (trees, []) op'
  | force_ops (trees, op1::op2::ops) =
    let 
        val comp = case (opAssoc op1) of Left => op <=
                                       | Right => op <
    in
        if comp(opPrec op1, opPrec op2) 
        then force_ops (force_op (trees, op2::ops) op1)
        else (trees, op1::op2::ops)
    end

fun combine_trees (t::trees, NoOp::ops) = (t::trees, ops)
  | combine_trees (t::trees, If::ops) = (t::trees, If::ops)
  | combine_trees (t::trees, Then::ops) = (t::trees, Then::ops)
  | combine_trees (t::trees, Else::ops) = (t::trees, Else::ops)
  | combine_trees (trees, op'::ops) = combine_trees (force_op (trees, ops) op')
  | combine_trees _ = raise (parse_error "Unexpected end of input")

fun handle_binop (trees, ops) binop =
    (print "BinOP"; force_ops (trees, (opFromBinOp binop)::ops))

fun handle_unop (trees, ops) unop =
    force_ops (trees, (opFromUnOp unop)::ops)

fun handle_lparen (trees, ops) =
    (trees, NoOp::ops)

val handle_rparen = combine_trees

fun handle_ident (trees, ops) s =
    ((A.Ident s)::trees, ops)

fun handle_num (trees, ops) x =
    ((A.Number x)::trees, ops)

fun handle_bool (trees, ops) x =
    ((A.Boolean x)::trees, ops)

(* conditional handling code *)
fun handle_if (trees, ops) =
    (trees, If::ops)

fun handle_then (trees, ops) = 
    (trees, Then::ops)

fun handle_else (trees, ops) =
    (trees, Else::ops)

fun handle_endif (t::t'::t''::trees, op'::op''::op'''::ops) =
    (* make sure we have the right stuff on the op stack *)
    if (op''', op'', op') = (If, Then, Else) then
       (A.Cond(t'', t', t)::trees, ops)
    else raise (parse_error "Invalid conditional")
  | handle_endif (t::t'::t''::trees, _) = raise undefined_error
  | handle_endif _ = raise (parse_error "Invalid conditional")


fun handle_token ps (T.Unop unop)   = handle_unop ps unop
  | handle_token ps (T.Binop binop) = handle_binop ps binop
  | handle_token ps T.LParen        = handle_lparen ps
  | handle_token ps T.RParen        = handle_rparen ps
  | handle_token ps (T.Ident s)     = handle_ident ps s
  | handle_token ps (T.Num x)       = handle_num ps x
  | handle_token ps T.True          = handle_bool ps true
  | handle_token ps T.False         = handle_bool ps false
  | handle_token ps T.If            = handle_if ps
  | handle_token ps T.Then          = handle_then (combine_trees ps)
  | handle_token ps T.Else          = handle_else (combine_trees ps)
  | handle_token ps T.Endif         = handle_endif (combine_trees ps)
  | handle_token _ _ = raise (parse_error "unknown token")


fun parse_expression t =
    let
      fun parse_rec (trees, ops) =
          ((print ((A.ast2str (hd trees)) ^ "\n")); case t() of T.EOF => (#1 (combine_trees (trees, ops)))
                    | T.EOS => (#1 (combine_trees (trees, ops)))
                    | tok => parse_rec (handle_token (trees, ops) tok))
    in
      hd (parse_rec ([A.Ident "a"], [NoOp]))
    end

fun parse_program t = raise undefined_error

end
