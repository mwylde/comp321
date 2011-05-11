structure L1Cbv =
struct

open Ast

exception undefined
exception expected_number
exception expected_bool

type value = A.expr


fun eval_expr e = 
    let 
        fun eval_num_bin f (Number x) (Number y) = Number (f(x, y))
          | eval_num_bin _ _ _ = raise expected_number

        fun eval_num_bool_bin f (Number x) (Number y) = Boolean (f(x, y))
          | eval_num_bool_bin _ _ _ = raise expected_number

        fun eval_bool_bin f (Boolean a) (Boolean b) = Boolean (f(a, b))
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

        fun eval_neg (Number x) = Number (~x)
        fun eval_not (Boolean a) = Boolean (not a)

        fun eval_unop unop e =
            let 
                val e' = eval_expr e;
            in
                case unop of NEG => eval_neg e'
                           | NOT => eval_not e'
            (* | HEAD => eval_head e' *)
            (* | TAIL => eval_tail e' *)
            end

        fun eval_binop binop e1 e2 =
            let 
                val e1' = eval_expr e1
                val e2' = eval_expr e2
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
            (*                  | CONS => eval_cons e1' e2' *)
            end

        fun eval_cond e1 e2 e3 =
            let 
                val (Boolean a) = eval_expr e1
                val e2' = eval_expr e2
                val e3' = eval_expr e3
            in
                if a then e2' else e3'
            end
    in
        case e of (UnOp(unop, e)) => eval_unop unop e
                | (BinOp(binop, e1, e2)) => eval_binop binop e1 e2
                | (Cond(e1, e2, e3)) => eval_cond e1 e2 e3
                | e => e
    end


fun eval_pgm _ = raise undefined
fun value2ast _ = raise undefined

end
