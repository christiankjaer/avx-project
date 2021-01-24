local
type m256d = string

fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))

fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)

fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
fun adds (a: m256d, b: real): m256d = add(a, broadcast(b))

fun sum (a: m256d): real = prim("__m256d_sum", a)
fun product (a: m256d): real = prim("__m256d_product", a)
fun printReal (n:real): unit = prim("printReal",n)

in

(* should print 30 and 16 *)
val _ = (
  printReal (sum (adds (mk (1.0, 2.0, 3.0, 4.0), 5.0)));
  printReal (product (broadcast 2.0))
)

end
