structure Typing =
struct

structure A = Ast

exception infer_error
exception undefined

type var = int
type ident = string

datatype t = V of var | Int | Bool | Arrow of t*t | List of t

(* labeled expression type *)
datatype lexpr = Ident of ident*t |
    Number of int*t | Boolean of bool*t | 
    UnOp of A.unop*lexpr*t | BinOp of A.binop*lexpr*lexpr*t | 
    NilList of t |
    Cond of lexpr*lexpr*lexpr*t |
    Abs of (ident*t)*lexpr*t | App of lexpr*lexpr*t

fun binop_t A.PLUS _ _  = Arrow (Int, Arrow (Int, Int))
  | binop_t A.SUB _ _   = Arrow (Int, Arrow (Int, Int))
  | binop_t A.TIMES _ _ = Arrow (Int, Arrow (Int, Int))
  | binop_t A.DIV _ _   = Arrow (Int, Arrow (Int, Int))
  | binop_t A.LT _ _    = Arrow (Int, Arrow (Int, Bool))
  | binop_t A.LE _ _    = Arrow (Int, Arrow (Int, Bool))
  | binop_t A.GT _ _    = Arrow (Int, Arrow (Int, Bool))
  | binop_t A.GE _ _    = Arrow (Int, Arrow (Int, Bool))
  | binop_t A.EQ a _    = Arrow (a, Arrow (a, Bool))
  | binop_t A.NE a _    = Arrow (a, Arrow (a, Bool))
  | binop_t A.AND _ _   = Arrow (Bool, Arrow (Bool, Bool))
  | binop_t A.OR _ _    = Arrow (Bool, Arrow (Bool, Bool))
  | binop_t A.CONS a _  = Arrow (a, Arrow(List a, List a))

fun unop_t A.NEG _ = Arrow (Int, Int)
  | unop_t A.NOT _ = Arrow (Bool, Bool)
  | unop_t A.HEAD (List a) = Arrow (List a, a)
  | unop_t A.HEAD _ = raise infer_error
  | unop_t A.TAIL (List a) = Arrow (List a, List a)
  | unop_t A.TAIL _ = raise infer_error

datatype 'a result = Found of 'a | NotFound

(* Finds the specified key in a (k, v) list *)
fun find [] a = NotFound
  | find ((k, v)::xs) a = if k = a then Found v else find xs a

(* inserts the value at the key, replacing what's there if neccessary *)
fun insert [] (k, v) = [(k, v)]
  | insert ((k', v')::xs) (k, v) = if k' = k then (k, v)::xs 
                                   else (k', v')::(insert xs (k, v))

(* labels an AST node using c as its label *)
fun label (A.Ident s) c m = 
    (case find m s of 
        (Found v) => (Ident (s, v), c, m)
      | _ => (Ident (s, V c), c+1, insert m (s, V c)))
  | label (A.Number x) c m = (Number (x, Int), c, m)
  | label (A.Boolean b) c m = (Boolean (b, Bool), c, m)
  | label (A.UnOp (unop, e)) c m = 
    let
        val (e', c', m') = label e (c+1) m
    in
        (UnOp (unop, e', V c), c', m')
    end
  | label (A.BinOp (binop, e1, e2)) c m =
    let
        val (e1', c', m') = label e1 (c+1) m
        val (e2', c'', m'') = label e2 c' m'
    in
        (BinOp (binop, e1', e2', V c), c'', m'')
    end
  | label A.NilList c m = (NilList (V c), c+1, m)
  | label (A.Cond (e1, e2, e3)) c m =
    let
        val (e1', c', m') = label e1 (c+1) m
        val (e2', c'', m'') = label e2 c' m'
        val (e3', c''', m''') = label e3 c'' m''
    in
        (Cond (e1', e2', e3', V c), c''', m''')
    end
  | label (A.Abs (x, e)) c m =
    let
        val (Ident (x', l), c', m') = label (A.Ident x) (c+1) m
        val (e', c'', m'') = label e c' m'
    in
        (Abs ((x', l), e', V c), c', m')
    end
  | label (A.App (e1, e2)) c m =
    let
        val (e1', c', m') = label e1 (c+1) m
        val (e2', c'', m'') = label e2 c' m'
    in
        (App (e1', e2', V c), c'', m'')
    end

fun lex2lab (Number (_, l)) = l
  | lex2lab (Boolean (_, l)) = l
  | lex2lab (Ident (_, l)) = l
  | lex2lab (UnOp (_, _,  l)) = l
  | lex2lab (BinOp (_, _, _, l)) = l
  | lex2lab (NilList l) = l
  | lex2lab (Cond (_, _, _, l)) = l
  | lex2lab (Abs (_, _, l)) = l
  | lex2lab (App (_, _, l)) = l

fun gen_es (Number (x, l)) es = (l, Int)::es
  | gen_es (Boolean (b, l)) es = (l, Bool)::es
  | gen_es (Ident (s, l)) es = es
 (* | gen_es (NilList l) es = (List l)::es *)
  | gen_es (UnOp (unop, e, l)) es = 
    (l, unop_t unop (lex2lab e))::((gen_es e []) @ es)
  | gen_es (BinOp (binop, e1, e2, l)) es =
    (l, binop_t binop (lex2lab e1) (lex2lab e2))::((gen_es e1 [] )@(gen_es e2 [])@es)
  | gen_es (Abs ((x, lx), e, l)) es =
    (l, Arrow (lx, lex2lab e))::((gen_es e [] ) @ es)
  | gen_es (App (e1, e2, l)) es =
    (lex2lab e1, Arrow (lex2lab e2, l))::((gen_es e1 [])@(gen_es e2 [])@es)

fun infer e = Int

fun toString _ = raise undefined

end
