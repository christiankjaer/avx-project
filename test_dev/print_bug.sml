type m256d = string

fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))

fun index (v: m256d, i: int): real = prim("__blockf64_sub_real", (v, i))

fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))

fun printReal (n:real): unit = prim("printReal",n)

fun f () =
let
  val a: m256d = mk (1.0, 2.0, 3.0, 4.0)
  val b: m256d = add(a,a)
  val _ = printReal(index (b, 0))
  val _ = printReal(index (b, 1))
  val _ = printReal(index (b, 2))
  val _ = printReal(index (b, 3))
in 
  ()
end

val _ = f ()
