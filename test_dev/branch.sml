type m256d = string

val size = 4

type element = real
type interface = real * real * real * real
type simd = m256d
type mask = m256d

fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))

fun index (v: m256d, i: int): real = prim("__blockf64_sub_real", (v, i))

fun read (v: m256d): real * real * real * real =
  (index (v,1), index (v,2), index (v,3), index (v,4))

fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)
fun lt (a: m256d, b: m256d): mask = prim("__m256d_less", (a,b))

(* fun blend (a: m256d, b: m256d, m: mask): simd = prim("__m256d_blend", (a,b,m))

fun all (a: mask): bool = prim("__m256d_all", a)
fun any (a: mask): bool = prim("__m256d_any", a)
  *)

fun printReal (n:real): unit = prim("printReal",n)

fun printM256d (x: m256d) =
let
  val x1 = index(x, 0)
  val x2 = index(x, 1)
  val x3 = index(x, 2)
  val x4 = index(x, 3)
in printReal(x1); printReal(x2); printReal(x3); printReal(x4)
end

fun f () =
let
  val x: simd = mk (1.0, 2.0, 3.0, 4.0)
  val y: simd = mk (4.0, 3.0, 2.0, 1.0)
in 
  lt (x, y)  
end

val _ = printM256d (f ())
