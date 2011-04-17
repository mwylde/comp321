structure Eval =
struct

exception undefined

fun eval_expr _  = raise undefined
fun eval_pgm _ = raise undefined
fun value2ast _ = raise undefined

end
