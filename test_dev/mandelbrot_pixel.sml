local
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

type m256d = string

val size = 4.0

val width: real = 400.0
val height: real = 250.0
val left = ~2.5
val bottom = ~1.0
val top = 1.0
val right = 1.0

type element = real
type interface = real * real * real * real
type simd = m256d
type mask = m256d

fun (x: real) / (y: real): real = prim ("divFloat", (x, y))

fun mk (a: real, b: real, c: real, d: real): simd = prim("__blockf64", (a,b,c,d))

fun index (v: simd, i: int): real = prim("__blockf64_sub_real", (v, i))

fun read (v: simd): real * real * real * real =
  (index (v,1), index (v,2), index (v,3), index (v,4))

fun broadcast (a: real): simd = prim("__m256d_broadcast", a)

fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
fun adds (a: m256d, b: real): m256d = add(a, broadcast(b))
fun sub (a: m256d, b: m256d): m256d = prim("__m256d_minus", (a, b))
fun subs (a: m256d, b: real): m256d = sub(a, broadcast(b))
fun mul (a: m256d, b: m256d): m256d = prim("__m256d_mul", (a, b))
fun muls (a: m256d, b: real): m256d = mul(a, broadcast(b))
fun divv (a: m256d, b: m256d): m256d = prim("__m256d_div", (a, b))
fun divs (a: m256d, b: real): m256d = divv(a, broadcast(b))

fun lt (a: m256d, b: m256d): mask = prim("__m256d_less", (a,b))
fun lts (a: m256d, b: real): mask = prim("__m256d_less", (a, broadcast(b)))
fun le (a: m256d, b: m256d): mask = prim("__m256d_lesseq", (a,b))
fun gt (a: m256d, b: m256d): mask = prim("__m256d_greater", (a,b))
fun ge (a: m256d, b: m256d): mask = prim("__m256d_greatereq", (a,b))

fun blend (a: m256d, b: m256d, m: mask): m256d = prim("__m256d_blend", (a,b,m))

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

fun scale (from: real, to: real, dom: real) (x: real): real =
  (x / dom) * (to - from) + from

fun scaleX (x: real): m256d =
  let
    val xr = x * size
    val x0 = mk (xr, xr + 1.0, xr + 2.0, xr + 3.0)
  in
    adds (muls (divs (x0, width), (right - left)), left)
  end

fun scaleY (y: real) = scale (bottom, top, height) y
in

(* 4 at a time *)
fun mandelbrot_simd (py: real, px4: real): m256d =
  let
    val one = broadcast 1.0
    val zero = broadcast 0.0
    val x0 = scaleX px4
    val y0 = scaleY py
    fun go (iter, mask, iters, x, y) =
      if (any mask andalso iter < 1000)
      then
        let
          val x2 = mul (x, x)
          val y2 = mul (y, y)
          val newX = add ((sub (x2, y2)), x0)
          val newY = adds (muls (mul (x,y), 2.0), y0)

          val cmp = add (x2, y2)
          val newMask = lts (cmp, 4.0)
        in
          go ((iter + 1), newMask, (blend (iters, add (iters, one), mask)), (blend
          (x, newX, mask)), (blend (y, newY, mask)))
        end
      else iters
  in
    go (0, (le (zero, zero)), zero, zero, zero)
  end

val _ =
  let
    val res = printM256d (mandelbrot_simd (20.0, 70.0))
  in
    ()
  end
  end
