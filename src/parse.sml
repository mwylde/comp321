structure L1Parse =
struct

structure T = Tokens
structure A = Ast

datatype Operator = NoOp | Plus | Minus | Times | Div | Neg
datatype Associativity = Left | Right
datatype Arrity = Unary | Binary

exception parse_error of string
exception undefined_error
exception unhandled_op

(* type ParseState = (List of AST.Expr, List of Operator) *)

fun opPrec NoOp  = 0
  | opPrec Plus  = 10
  | opPrec Minus = 10
  | opPrec Times = 20
  | opPrec Div   = 20
  | opPrec Neg   = 30

fun opAssoc Plus  = Left
  | opAssoc Minus = Left
  | opAssoc Times = Left
  | opAssoc Div   = Left
  | opAssoc Neg   = Right

fun opArrity Plus  = Binary
  | opArrity Minus = Binary
  | opArrity Times = Binary
  | opArrity Div   = Binary
  | opArrity Neg   = Unary

fun opFromBinOp A.PLUS = Plus
  | opFromBinOp A.SUB = Minus
  | opFromBinOp A.TIMES = Times
  | opFromBinOp A.DIV = Div
  | opFromBinOp _ = raise unhandled_op

fun binOpFromOp Plus = A.PLUS
  | binOpFromOp Minus = A.SUB
  | binOpFromOp Times = A.TIMES
  | binOpFromOp Div = A.DIV
  | binOpFromOp _ = raise unhandled_op

fun opFromUnOp A.NEG = Neg
  | opFromUnOp _ = raise unhandled_op

fun unOpFromOp Neg = A.NEG
  | unOpFromOp _ = raise unhandled_op
                       

fun force_op ps op' =
    let
        fun force_unop (x::trees, ops) unop =
            ((A.UnOp(unOpFromOp unop, x))::trees, ops)

        fun force_binop (x::y::trees, ops) binop =
            ((A.BinOp(binOpFromOp binop, x, y))::trees, ops)
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
  | combine_trees (trees, op'::ops) = combine_trees (force_op (trees, ops) op')

fun handle_binop (trees, ops) binop =
    force_ops (trees, (opFromBinOp binop)::ops)

fun handle_unop (trees, ops) unop =
    force_ops (trees, (opFromUnOp unop)::ops)

fun handle_lparen (trees, ops) =
    (trees, NoOp::ops)

val handle_rparen = combine_trees

fun handle_ident (trees, ops) s =
    ((A.Ident s)::trees, ops)

fun handle_num (trees, ops) x =
    ((A.Number x)::trees, ops)


fun handle_token ps (T.Unop unop)   = handle_unop ps unop
  | handle_token ps (T.Binop binop) = handle_binop ps binop
  | handle_token ps T.LParen        = handle_lparen ps
  | handle_token ps T.RParen        = handle_rparen ps
  | handle_token ps (T.Ident s)     = handle_ident ps s
  | handle_token ps (T.Num x)       = handle_num ps x
  | handle_token _ _ = raise unhandled_op


fun parse_expression t =
    let
      fun parse_rec (trees, ops) =
          case t() of T.EOF => (#1 (combine_trees (trees, ops)))
                    | T.EOS => (#1 (combine_trees (trees, ops)))
                    | tok => parse_rec (handle_token (trees, ops) tok)
    in
      parse_rec ([], [NoOp])
    end

fun parse_program t = raise undefined_error

end
