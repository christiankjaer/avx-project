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

fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
fun adds (a: m256d, b: real): m256d = add(a, broadcast(b))

fun mul (a: m256d, b: m256d): m256d = prim("__m256d_mul", (a, b))
fun muls (a: m256d, b: real): m256d = mul(a, broadcast(b))

fun sub (a: m256d, b: m256d): m256d = prim("__m256d_minus", (a, b))
fun subs (a: m256d, b: real): m256d = sub(a, broadcast(b))

fun lt (a: simd, b: simd): mask = prim("__m256d_less", (a,b))
fun le (a: simd, b: simd): mask = prim("__m256d_lesseq", (a,b))
fun gt (a: simd, b: simd): mask = prim("__m256d_greater", (a,b))
fun ge (a: simd, b: simd): mask = prim("__m256d_greatereq", (a,b))

fun blend (a: simd, b: simd, m: mask): simd = prim("__m256d_blend", (a,b,m))

fun all (a: mask): bool = prim("__m256d_all", a)
fun any (a: mask): bool = prim("__m256d_any", a)

fun printReal (n:real): unit = prim("printReal",n)

fun printM256d (x: m256d) =
let
  val x1 = index(x, 0)
  val x2 = index(x, 1)
  val x3 = index(x, 2)
  val x4 = index(x, 3)
in printReal(x1); printReal(x2); printReal(x3); printReal(x4)
end

fun g () =
  let
    fun loop (acc: m256d): m256d =
      let val x = acc
          val zeros = broadcast 0.0
      in
        if (all (le (x, zeros))) then x else loop (subs (x, 1.0))
      end
  in
    loop (broadcast 100.0)
  end

val _ = printM256d (g ())
