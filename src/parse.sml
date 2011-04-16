structure Parse =
struct

structure T = Tokens
structure A = Ast

datatype Operator = NoOp | Plus | Minus | Times | Div | Neg
datatype Associativity = Left | Right
datatype Arrity = Unary | Binary

exception parse_error of string
exception undefined_error
exception unhandled_op

type ParseState = ([AST.Expr], [Operator])

val undefined = raise undefined_error

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

fun opFromBinOp A.Plus = Plus
  | opFromBinOp A.Minus = Minus
  | opFromBinOp A.Times = Times
  | opFromBinOp A.Div = Div
  | _ = raise unhandled_op

fun opFromUnOp A.Neg = Neg
  | _ = raise unhandled_op

fun compareOp a b = Int.compare(opPrecs a, opPrecs b)

fun parse_expression t =
  let
      fun parse_rec (tree::tress, ops) =
          case t() of T.EOF = tree
                    | T.EOF = tree
                    | tok = parse_rec (handle_token (tree::trees, ops) tok)
          
      fun handle_token ps (T.Unop unop)   = handle_unop ps unop
        | handle_token ps (T.Binop binop) = handle_binop ps binop
        | handle_token ps T.LParen        = handle_lparen ps
        | handle_token ps T.RParen        = handle_rparen ps
        | handle_token ps (T.Ident s)     = handle_ident ps s
        | handle_token ps (T.Num x)       = handle_num ps x

      fun handle_binop (trees, ops) binop =
          force_ops (trees, (opFromBinOp binop)::ops)

      fun handle_unop (trees, ops) unop =
          force_ops (trees, (opFromUnOp unop)::ops)

      fun handle_lparen (trees, ops) =
          (trees, NoOp::ops)

      val handle_rparen = force_all_ops

      fun handle_ident (trees, ops) s =
          ((A.Ident s)::trees, ops)

      fun handle_num (trees, ops) x =
          ((A.Number x)::trees, ops)

      fun force_unop (x::trees, ops) unop =
          ((A.UnOp(unop, x)::trees, ops)

      fun force_binop (x::y::tree, ops) binop =
          ((A.BinOp(binop, x, y)::trees, ops)

      fun force_op ps op' =
          case (opArrity op') of Unary => force_unop ps op'
                               | Binary => force_binop ps op'

      fun force_ops (trees, []) = (trees, [])
        | force_ops (trees, op'::Nil) = force_op (trees, Nil) op'
        | force_ops (trees, op1::op2::ops) =
          let 
              val comp = case (opAssoc op1) of Left => op >=
                                             | Right => op >
          in
              if comp(opPrec op1, opPrec op2) 
              then force_ops (force_op (trees, op2::ops) op1)
              else (trees, op1::op2::ops)
          end

      fun force_all_ops (trees, []) = (trees, [])
        | force_all_ops (trees, op'::ops) = 
          force_all_ops (force_op (trees, ops) op')
  in
      parse_rec ([], [])
  end

fun parse_program t = undefined

end
