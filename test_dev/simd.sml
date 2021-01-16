local
type m256d = string

infix  4  = <> > >= < <=

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

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

fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
fun adds (a: m256d, b: real): m256d = add(a, broadcast(b))

fun mul (a: m256d, b: m256d): m256d = prim("__m256d_mul", (a, b))
fun muls (a: m256d, b: real): m256d = mul(a, broadcast(b))

fun sub (a: m256d, b: m256d): m256d = prim("__m256d_minus", (a, b))
fun subs (a: m256d, b: real): m256d = sub(a, broadcast(b))

fun printReal (n:real): unit = prim("printReal",n)

fun printM256d (x: m256d) =
let
  val x1 = index(x, 0)
  val x2 = index(x, 1)
  val x3 = index(x, 2)
  val x4 = index(x, 3)
in printReal(x1); printReal(x2); printReal(x3); printReal(x4)
end

fun g (x: m256d) = add(x, mk (0.0, 1.0, 2.0, 3.0) )
in

fun f (x: m256d) =
if (ref false = ref true) then f x else
let
  val a: m256d = mk (10.0, 20.0, 30.0, 40.0)
  val b: m256d = mul(a, x)
  val c: m256d = adds(b, 3.0)
  val d: m256d = sub(c, a)
  val e: m256d = g(d)
in 
  e
end

val _ = printM256d (f (broadcast 3.0))
end
