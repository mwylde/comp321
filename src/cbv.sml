structure L1Cbv =
struct

open Ast

exception undefined
exception expected_number
exception expected_bool
exception expected_list
exception only_idents

exception free_var_in_expr

datatype 'a result = Found of 'a | NotFound

(* Finds the specified key in a (k, v) list *)
fun find [] a = NotFound
  | find ((k, v)::xs) a = if k = a then Found v else find xs a

(* inserts the value at the key, replacing what's there if neccessary *)
fun insert [] (k, v) = [(k, v)]
  | insert ((k', v')::xs) (k, v) = if k' = k then (k, v)::xs 
                                   else (k', v')::(insert xs (k, v))

datatype value = V of expr*((ident*value) list) | C of value*value

fun eval_val (C (head, tail)) = C (eval_val head, eval_val tail)
  | eval_val (V (ve, e)) = 
    let 
        fun eval_num_bin f (V (Number x, _)) (V (Number y, _)) = 
            V (Number (f(x, y)), [])
          | eval_num_bin _ _ _ = raise expected_number

        fun eval_num_bool_bin f (V (Number x, _)) (V (Number y, _)) =
            V (Boolean (f(x, y)), [])
          | eval_num_bool_bin _ _ _ = raise expected_number

        fun eval_bool_bin f (V (Boolean a, _)) (V (Boolean b, _)) = 
            V (Boolean (f(a, b)), [])
          | eval_bool_bin _ _ _ = raise expected_bool

        val eval_plus = eval_num_bin (op +)
        val eval_sub = eval_num_bin (op -)
        val eval_times = eval_num_bin (op * )
        val eval_div = eval_num_bin (op div)

        val eval_lt = eval_num_bool_bin (op <)
        val eval_le = eval_num_bool_bin (op <=)
        val eval_gt = eval_num_bool_bin (op >)
        val eval_ge = eval_num_bool_bin (op >=)
        val eval_eq = eval_num_bool_bin (op =)
        val eval_ne = eval_num_bool_bin (op <>)
        val eval_and = eval_bool_bin (fn (a, b) => a andalso b)
        val eval_or = eval_bool_bin (fn (a, b) => a orelse b)

        fun eval_cons v1 v2 = 
            let
                val head = eval_val v1
                val tail = eval_val v2
            in
                C (head, tail)
            end

        fun eval_neg (V (Number x, _)) = V (Number (~x), [])
          | eval_neg _ = raise expected_number
        fun eval_not (V (Boolean a, _)) = V (Boolean (not a), [])
          | eval_not _ = raise expected_bool

        fun eval_hd (C (v1, v2)) = eval_val v1
          | eval_hd _ = raise expected_list

        fun eval_tl (C (v1, v2)) = eval_val v2
          | eval_tl _ = raise expected_list

        fun eval_unop unop e =
            let 
                val v = eval_val e;
            in
                case unop of NEG => eval_neg v
                           | NOT => eval_not v
                           | HEAD => eval_hd v
                           | TAIL => eval_tl v
            end

        fun eval_binop binop e1 e2 =
            let 
                val e1' = eval_val e1
                val e2' = eval_val e2
            in
                case binop of PLUS => eval_plus e1' e2'
                            | SUB => eval_sub e1' e2'
                            | TIMES => eval_times e1' e2'
                            | DIV => eval_div e1' e2'
                            | LT => eval_lt e1' e2'
                            | LE => eval_le e1' e2'
                            | GT => eval_gt e1' e2'
                            | GE => eval_ge e1' e2'
                            | EQ => eval_eq e1' e2'
                            | NE => eval_ne e1' e2'
                            | AND => eval_and e1' e2'
                            | OR => eval_or e1' e2'
                            | CONS => eval_cons e1' e2'
            end

        fun eval_cond v1 v2 v3 =
            let 
                val (V (Boolean a, _)) = eval_val v1
                val (V (e2, _)) = eval_val v2
                val (V (e3, _)) = eval_val v3
            in
                if a then V (e2, []) else V (e3, [])
            end

        fun eval_app lv v =
            let
                val (V (Abs (x, e0), env)) = eval_val lv
                val v1 = eval_val v
                val env' = insert env (x, v1)
            in
                eval_val (V (e0, env'))
            end

        fun eval_ident v =
            let
                val (V (Ident x, env)) = v
            in
                case find env x of (Found v) => eval_val v
                                 | NotFound => raise free_var_in_expr
            end
            
    in case ve 
        of (UnOp(unop, ve)) => eval_unop unop (V (ve, e))
         | (BinOp(binop, e1, e2)) => eval_binop binop (V (e1, e)) (V (e2, e))
         | (Cond(e1, e2, e3)) => eval_cond (V (e1, e)) (V (e2, e)) (V (e3,e))
         | (App(l, ve)) => eval_app (V (l, e)) (V (ve, e))
         | (Ident _) => eval_ident (V (ve, e))
         | _ => (V (ve, e))
    end

fun eval_expr e = eval_val (V (e, []))
                  
fun combine_stmts ((Assign (x, e))::[]) = e
  | combine_stmts ((Assign (x, e))::ss) = 
    App (Abs (x, (combine_stmts ss)), e)

fun eval_pgm (Program p)=
    eval_expr (combine_stmts (List.rev p))

fun value2ast (V (Ident x, env)) = 
    (case find env x of (Found v) => value2ast(v)
                     | NotFound => Ident x)    
  | value2ast (V (UnOp(unop, ve), env)) =
    UnOp (unop, value2ast (V (ve, env)))
  | value2ast (V (BinOp(binop, e1, e2), env)) =
    BinOp (binop, value2ast (V (e1, env)), value2ast (V (e2, env)))
  | value2ast (V (Cond(e1, e2, e3), env)) =
    Cond (value2ast (V (e1, env)), value2ast(V (e2, env)), value2ast (V (e3, env)))
  | value2ast (V (App(l, ve), env)) =
    App (value2ast (V (l, env)), value2ast (V (ve, env)))
  | value2ast (V (Abs (x, e0), env)) =
    Abs (x, value2ast (V (e0, env)))
  | value2ast (V (ve, _)) = ve
  | value2ast (C (v1, v2)) = BinOp(CONS, value2ast v1, value2ast v2)

end
