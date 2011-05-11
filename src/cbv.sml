structure L1Cbv =
struct

open Ast

exception undefined

type value = A.expr


fun eval_expr e = 
    let 
        fun eval_plus (Number x) (Number y) = Number (x + y)
        fun eval_sub (Number x) (Number y) = Number (x - y)
        fun eval_times (Number x) (Number y) = Number (x * y)
        fun eval_div (Number x) (Number y) = Number (x div y)
        fun eval_lt (Number x) (Number y) = Boolean (x < y)
        fun eval_le (Number x) (Number y) = Boolean (x <= y)
        fun eval_gt (Number x) (Number y) = Boolean (x > y)
        fun eval_ge (Number x) (Number y) = Boolean (x >= y)
        fun eval_eq (Number x) (Number y) = Boolean (x = y)
        fun eval_ne (Number x) (Number y) = Boolean (x <> y)
        fun eval_and (Boolean a) (Boolean b) = Boolean (a andalso b)
        fun eval_or (Boolean a) (Boolean b) = Boolean (a orelse b)

        fun eval_neg (Number x) = Number (~x)
        fun eval_not (Boolean a) = Boolean (not a)

        fun eval_unop unop e = 
            case unop of NEG => eval_neg e
                       | NOT => eval_not e
        (* | HEAD => eval_head e *)
        (* | TAIL => eval_tail e *)

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
    in
        case e of (UnOp(unop, e)) => eval_unop unop e
                | (BinOp(binop, e1, e2)) => eval_binop binop e1 e2
                (*| (Cond(e1, e2, e3) => eval_cond(e1, e2, e3) *)
                | e => e
    end


fun eval_pgm _ = raise undefined
fun value2ast _ = raise undefined

end
