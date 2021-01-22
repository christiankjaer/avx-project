local
type m256d = string

type simd = m256d
type mask = m256d

fun mk (a: real, b: real, c: real, d: real): simd = prim("__blockf64", (a,b,c,d))

fun broadcast (a: real): simd = prim("__m256d_broadcast", a)
fun lt (a: simd, b: simd): mask = prim("__m256d_less", (a,b))
fun le (a: simd, b: simd): mask = prim("__m256d_lesseq", (a,b))
fun gt (a: simd, b: simd): mask = prim("__m256d_greater", (a,b))
fun ge (a: simd, b: simd): mask = prim("__m256d_greatereq", (a,b))

fun true_ (): mask = prim("__m256d_true", ())
fun false_ (): mask = prim("__m256d_false", ())


fun all (a: mask): bool = prim("__m256d_all", a)
fun any (a: mask): bool = prim("__m256d_any", a)

fun printReal (n:real): unit = prim("printReal",n)

fun f () =
let
  val b = all (false_ ())
in 
  if b then printReal 1.0 else printReal 0.0
end
in

val _ = f ()
end
