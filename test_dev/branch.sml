type m256d = string

type simd = m256d
type mask = m256d

fun mk (a: real, b: real, c: real, d: real): simd = prim("__blockf64", (a,b,c,d))

fun broadcast (a: real): simd = prim("__m256d_broadcast", a)
fun lt (a: simd, b: simd): mask = prim("__m256d_less", (a,b))
fun le (a: simd, b: simd): mask = prim("__m256d_lesseq", (a,b))
fun gt (a: simd, b: simd): mask = prim("__m256d_greater", (a,b))
fun ge (a: simd, b: simd): mask = prim("__m256d_greatereq", (a,b))


fun all (a: mask): bool = prim("__m256d_all", a)
fun any (a: mask): bool = prim("__m256d_any", a)

fun printReal (n:real): unit = prim("printReal",n)

fun f () =
let
  val x = mk (2.0, 3.0, 4.0, 5.0)
  val y = mk (3.0, 3.0, 3.0, 3.0)
  val b = any (gt (x, y))
in 
  if b then printReal 1.0 else printReal 0.0
end

val _ = f ()
