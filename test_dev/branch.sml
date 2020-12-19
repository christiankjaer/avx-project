type m256d = string

val size = 4

type element = real
type interface = real * real * real * real
type simd = m256d
type mask = m256d

fun mk (a: real, b: real, c: real, d: real): simd = prim("__blockf64", (a,b,c,d))

fun index (v: simd, i: int): real = prim("__blockf64_sub_real", (v, i))

fun read (v: simd): real * real * real * real =
  (index (v,1), index (v,2), index (v,3), index (v,4))

fun broadcast (a: real): simd = prim("__m256d_broadcast", a)
fun lt (a: simd, b: simd): mask = prim("__m256d_less", (a,b))
fun le (a: simd, b: simd): mask = prim("__m256d_lesseq", (a,b))
fun gt (a: simd, b: simd): mask = prim("__m256d_greater", (a,b))
fun ge (a: simd, b: simd): mask = prim("__m256d_greatereq", (a,b))

fun blend (a: simd, b: simd, m: mask): simd = prim("__m256d_blend", (a,b,m))

fun all (a: mask): bool = prim("__m256d_all", a)
fun any (a: mask): bool = prim("__m256d_any", a)

fun printReal (n:real): unit = prim("printReal",n)

fun printM256d (x: simd) =
let
  val x1 = index(x, 0)
  val x2 = index(x, 1)
  val x3 = index(x, 2)
  val x4 = index(x, 3)
in printReal(x1); printReal(x2); printReal(x3); printReal(x4)
end

fun f () =
let
  val x = mk (2.0, 3.0, 4.0, 5.0)
  val y = mk (3.0, 3.0, 3.0, 3.0)
  val ones = broadcast 1.0
  val zeros = broadcast 0.0
  val cmp = gt (x, y)
in 
  printM256d (blend (ones, zeros, cmp))
end

val _ = f ()
