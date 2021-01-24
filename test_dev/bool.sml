local
type m256d = string

type element = real
type interface = real * real * real * real
type simd = m256d
type mask = m256d

fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))

fun index (v: m256d, i: int): real = prim("__blockf64_sub_real", (v, i))

fun read (v: m256d): real * real * real * real =
  (index (v,0), index (v,1), index (v,2), index (v,3))

fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)

fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
fun adds (a: m256d, b: real): m256d = add(a, broadcast(b))

fun mul (a: m256d, b: m256d): m256d = prim("__m256d_mul", (a, b))
fun muls (a: m256d, b: real): m256d = mul(a, broadcast(b))

fun sub (a: m256d, b: m256d): m256d = prim("__m256d_minus", (a, b))
fun subs (a: m256d, b: real): m256d = sub(a, broadcast(b))

fun true_ (): mask = prim("__m256d_true", ())
fun false_ (): mask = prim("__m256d_false", ())
fun mask_and (a: mask, b: mask): mask = prim("__m256d_and", (a, b))
fun mask_or (a: mask, b: mask): mask = prim("__m256d_or", (a, b))
fun mask_not (a: mask): mask = prim("__m256d_not", a)

fun printReal (n:real): unit = prim("printReal",n)

fun printM256d (x: m256d) =
let
  val x1 = index(x, 0)
  val x2 = index(x, 1)
  val x3 = index(x, 2)
  val x4 = index(x, 3)
in printReal(x1); printReal(x2); printReal(x3); printReal(x4)
end
in

val _ = printM256d (mask_or (true_ (), false_ ()))
end
