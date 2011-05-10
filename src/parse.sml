structure L1Parse =
struct

structure T = Tokens
structure A = Ast

datatype operator = NoOp | Plus | Minus | Times | Div | Neg |
                    Lt | Leq | Gt | Geq | Eq | Neq | And | Or |
                    Cons | Not | Head | Tail | 
                    If | Then | Else | Endif |
                    Lambda of string |
                    ILParen

type ident = string

datatype expr = Ident of ident | 
    Number of int | Boolean of bool | 
    UnOp of operator*expr | BinOp of operator*expr*expr | 
    NilList |
    Cond of expr*expr*expr |
    Abs of ident*expr | App of expr*expr |
    BGroup

datatype Associativity = Left | Right
datatype Arrity = Unary | Binary

exception parse_error of string
exception undefined_error
exception unhandled_op
exception unexpected_token

fun opPrec (NoOp | ILParen) = 0
  | opPrec (Lambda _) = 0
  | opPrec (If | Then | Else | Endif) = 5
  | opPrec (And | Or) = 10
  | opPrec (Lt | Leq | Gt | Geq | Eq | Neq) = 20
  | opPrec Cons = 30
  | opPrec (Plus | Minus) = 40
  | opPrec (Times | Div) = 50
  | opPrec (Neg | Head | Tail | Not) = 60

fun opAssoc (NoOp | ILParen | Plus | Minus | Times | Div) = Left
  | opAssoc (And | Or | Lt | Leq | Gt | Geq | Eq | Neq)   = Left
  | opAssoc (Neg | Head | Tail | Not | Cons)              = Right
  | opAssoc (If | Then | Else | Endif)                    = Left
  | opAssoc (Lambda _)                                    = Right

fun opArrity (ILParen | NoOp | Plus | Minus | Times | Div | Cons)  = Binary
  | opArrity (And | Or | Lt | Leq | Gt | Geq | Eq | Neq)           = Binary
  | opArrity (Neg | Head | Tail | Not)                             = Unary
  | opArrity (If | Then | Else | Endif)                            = Binary
  | opArrity (Lambda _)                                            = Unary

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

fun expToAST (Ident x) = A.Ident x
  | expToAST (Number x) = A.Number x
  | expToAST (Boolean b) = A.Boolean b
  | expToAST (UnOp(unop, e)) = A.UnOp(unOpFromOp unop, expToAST e)
  | expToAST (BinOp(b, e1, e2)) = A.BinOp(binOpFromOp b, expToAST e1, expToAST e2)
  | expToAST NilList = A.NilList
  | expToAST (Cond(e1, e2, e3)) = A.Cond(expToAST e1, expToAST e2, expToAST e3)
  | expToAST (Abs(s, e)) = A.Abs (s, expToAST e)
  | expToAST (App(e1, e2)) = A.App (expToAST e1, expToAST e2)
  | expToAST BGroup = raise (parse_error "unexpected BGroup")

fun collect_apps true (BGroup::trees) = BGroup::trees
  | collect_apps false (BGroup::trees) = trees
  | collect_apps _ (t::BGroup::trees) = t::trees
  | collect_apps _ [] = []
  | collect_apps r (t::trees) = 
    case (collect_apps r trees) of (BGroup::trees') => t::BGroup::trees'
                                 | (t'::trees') => (App(t', t))::trees'
                                 | ([]) => [t]

fun force_op (t::trees, (Lambda x)::ops) = ((Abs (x, t))::trees, ops)
  | force_op (trees, If::ops) = (trees, ops)
  | force_op (trees, Then::ops) = (trees, ops)
  | force_op (trees, Else::ops) = (trees, ops)
  | force_op (trees, op'::ops) =
    let
        fun force_unop (x::trees, ops) unop =
            (UnOp(unop, x)::trees, ops)
          | force_unop _ _ = raise (parse_error "need argument")

        fun force_binop (x::y::trees, ops) binop =
            (BinOp(binop, y, x)::trees, ops)
          | force_binop _ _ = raise (parse_error "need argument")
    in
        case (opArrity op') of Unary => force_unop (trees, ops) op'
                             | Binary => force_binop (trees, ops) op'
    end
  | force_op ps = ps

fun force_ops NoOp (BGroup::trees, NoOp::ops) = (trees, ops)
  | force_ops NoOp (BGroup::trees, ILParen::ops) = force_ops NoOp (trees, ops)
  | force_ops _ (trees, NoOp::ops) = (collect_apps true trees, ops)
  | force_ops op1 (trees, ILParen::ops) = force_ops op1 (collect_apps false trees, ops)
  | force_ops _ (trees, []) = (trees, [])
  | force_ops op1 (trees, op2::ops) =
    let 
        val comp = case (opAssoc op1) of Left => op <=
                                       | Right => op <
    in
        if comp(opPrec op1, opPrec op2) 
        then force_ops op1 (force_op (trees, op2::ops))
        else (trees, op2::ops)
    end

fun force_all_ops (t::trees, If::ops) = (t::trees, If::ops)
  | force_all_ops (t::trees, Then::ops) = (t::trees, Then::ops)
  | force_all_ops (t::trees, Else::ops) = (t::trees, Else::ops)
  | force_all_ops (trees, ops) = force_ops NoOp (trees, ops)

fun handle_binop (trees, ops) binop =
    let 
        val op' = (opFromBinOp binop)
        val (trees', ops') = force_ops op' (trees, ops)
    in
        (BGroup::trees', ILParen::op'::ops')
    end

fun handle_unop (trees, ops) unop =
    let
        val op' = (opFromUnOp unop)
        val (trees', ops') = force_ops op' (trees, ops)
    in
        (BGroup::trees', ILParen::op'::ops')
    end

fun handle_lparen (trees, ops) =
    (trees, NoOp::ops)

val handle_rparen = force_all_ops

fun handle_ident (trees, ops) s =
    ((Ident s)::trees, ops)

fun handle_num (trees, ops) x =
    ((Number x)::trees, ops)

fun handle_bool (trees, ops) x =
    ((Boolean x)::trees, ops)

(* list handling code *)
fun handle_cons (trees, ops) =
    let
        val (trees, ops) = force_ops Cons (trees, ops)
    in
        (trees, Cons::ops)
    end

fun handle_nil (trees, ops) =
    (NilList::trees, ops)

(* conditional handling code *)
fun handle_if (trees, ops) =
    (trees, If::ops)

fun handle_then (trees, ops) = 
    (trees, Then::ops)

fun handle_else (trees, ops) =
    (trees, Else::ops)

fun handle_endif (t::t'::t''::trees, ops) =
    (Cond(t'', t', t)::trees, ops)
  | handle_endif _ = raise (parse_error "Invalid conditional")

(* abstraction *)
fun handle_lambda ps x =
    let
        val (trees, ops) = force_ops (Lambda x) ps
    in
        (trees, (Lambda x)::ops)
    end

fun handle_token ps (T.Unop unop)   = handle_unop ps unop
  | handle_token ps (T.Binop binop) = handle_binop ps binop
  | handle_token ps T.LParen        = handle_lparen ps
  | handle_token ps T.RParen        = handle_rparen ps
  | handle_token ps (T.Ident s)     = handle_ident ps s
  | handle_token ps (T.Num x)       = handle_num ps x
  | handle_token ps T.True          = handle_bool ps true
  | handle_token ps T.False         = handle_bool ps false
  | handle_token ps T.If            = handle_if ps
  | handle_token ps T.Then          = handle_then (force_all_ops ps)
  | handle_token ps T.Else          = handle_else (force_all_ops ps)
  | handle_token ps T.Endif         = handle_endif (force_all_ops ps)
  | handle_token ps (T.Lambda x)    = handle_lambda ps x
  | handle_token ps T.Nil           = handle_nil ps
  | handle_token ps T.Cons          = handle_cons ps
  | handle_token _ _ = raise (parse_error "unknown token")

fun exp2str BGroup = "BGroup"
  | exp2str e =  A.ast2str (expToAST e)
    
fun parse_expression t =
    let 
        fun parse_rec (trees, ops) =
            ((print ((exp2str (hd trees)) ^ "\n")); 
             case t() of T.EOF => (#1 (force_all_ops (trees, ops)))
                       | T.EOS => (#1 (handle_rparen (trees, ops)))
                       | tok => parse_rec (handle_token (trees, ops) tok))
    in
        expToAST (hd (parse_rec ([BGroup], [NoOp])))
    end

fun parse_program t = raise undefined_error

end
