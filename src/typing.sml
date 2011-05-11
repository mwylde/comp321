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

fun binop_t A.PLUS  _ _ = (Int, Int, Int)
  | binop_t A.SUB   _ _ = (Int, Int, Int)
  | binop_t A.TIMES _ _ = (Int, Int, Int)
  | binop_t A.DIV   _ _ = (Int, Int, Int)
  | binop_t A.LT    _ _ = (Int, Int, Bool)
  | binop_t A.LE    _ _ = (Int, Int, Bool)
  | binop_t A.GT    _ _ = (Int, Int, Bool)
  | binop_t A.GE    _ _ = (Int, Int, Bool)
  | binop_t A.EQ    a b = (a, a, Bool)
  | binop_t A.NE    a b = (a, a, Bool)
  | binop_t A.AND   _ _ = (Bool, Bool, Bool)
  | binop_t A.OR    _ _ = (Bool, Bool, Bool)
  | binop_t A.CONS a (List b) = (a, List a, List a)
  | binop_t _ _ _ = raise infer_error

fun unop_t A.NEG _ = (Int, Int)
  | unop_t A.NOT _ = (Bool, Bool)
  | unop_t A.HEAD (List a) = (List a, a)
  | unop_t A.TAIL (List a) = (List a, List a)
  | unop_t _ _ = raise infer_error

datatype 'a result = Found of 'a | NotFound

fun remove [] a = []
  | remove (x::xs) a = if x = a then remove xs a else x::(remove xs a)

(* Finds the specified key in a (k, v) list *)
fun find [] a = NotFound
  | find ((k, v)::xs) a = if k = a then Found v else find xs a

(* inserts the value at the key, replacing what's there if neccessary *)
fun insert [] (k, v) = [(k, v)]
  | insert ((k', v')::xs) (k, v) = if k' = k then (k, v)::xs 
                                   else (k', v')::(insert xs (k, v))

fun findv env v = find env (V v)
fun insertv env (k, v) = insert env ((V k), v)

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
  | gen_es (NilList l) es = (l, List l)::es
  | gen_es (UnOp (unop, e, l)) es = 
    let
        val (t1, t2) = unop_t unop (lex2lab e)
    in
        (l, t2)::(lex2lab e, t1)::(gen_es e es)
    end
  | gen_es (BinOp (binop, e1, e2, l)) es =
    let
        val (t1, t2, t3) = binop_t binop (lex2lab e1) (lex2lab e2)
        val es' = (gen_es e1 (gen_es e2 es))
    in
        (l, t3)::(lex2lab e1, t1)::(lex2lab e2, t2)::es'
    end
  | gen_es (Abs ((x, lx), e, l)) es =
    (l, Arrow (lx, lex2lab e))::((gen_es e [] ) @ es)
  | gen_es (App (e1, e2, l)) es =
    (lex2lab e1, Arrow (lex2lab e2, l))::((gen_es e1 [])@(gen_es e2 [])@es)


fun unify (t1, t2) envx =
    let
       val env' = remove envx (t1, t2)
       fun subst env (Arrow (a, b)) = Arrow (subst env a, subst env b)
          | subst env (V v) = (case findv env v of (Found x) => x
                                                | _ => (V v))
          | subst env t = t
                          
       fun tsubst env (V v) = (case findv env v of (Found x) => x
                                                 | _ => (V v))
         | tsubst _ t = t

       fun bound v Int _ = false
         | bound v Bool _ = false
         | bound v1 (V v2) env = (case findv env v2 of (Found t) => bound v1 t env
                                                     | NotFound => v1 = v2)
         | bound v (Arrow (t1, t2)) env = bound v t1 env orelse bound v t2 env
                                                                 
       fun unify_var v1 (V v2) env = if v1 = v2 then env
                                     else insertv env (v1, V v2)
         | unify_var v1 t2 env = if bound v1 t2 env
                                 then insertv env (v1, subst env t2)
                                 else insertv env (v1, subst env t2)

       fun unify' Bool Bool env = env
         | unify' Int Int env = env
         | unify' (Arrow (a, b)) (Arrow (c, d)) env =
           unify (a, c) (unify (b, d) env)
         | unify' (V v) t env = unify_var v t env
         | unify' t (V v) env = unify_var v t env
         | unify' t1 t2 _ = raise infer_error

    in
        unify' (tsubst env' t1) (tsubst env' t2) env'
    end

fun toString (V v) =
    if v < 26 then Char.toString(Char.chr(97+v))
    else if v < 52 then Char.toString(Char.chr(64+(v-26)))
         else "(a{" ^ Int.toString(v-52) ^ "})"
  | toString (Int) = "int"
  | toString (Bool) = "bool"
  | toString (Arrow(t1, t2)) = "(" ^ toString(t1) ^ ") -> (" ^ 
                                   toString(t2) ^ ")"
  | toString (List(sigma)) = "[" ^ toString(sigma) ^ "]"

fun print_eq (t1, t2) = print ((toString t1) ^ " = " ^ (toString t2) ^ "\n")

fun infer e = 
    let
        val (labeled, _, _) = label e 0 []
        val eqns = gen_es labeled []
        fun try [] env = env
          | try (eq::eqs) env =
            ((print_eq eq) ; 
             try eqs (unify eq env))
        fun run env = 
            let
                val env' = try env env
            in
                if env' = env then env'
                else run env'
            end
    in
        case findv (run (run eqns)) 0 of (Found t) => t
                                       | _ => raise infer_error
    end
end
