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

fun printReal (n:real): unit = prim("printReal",n)
fun intToReal (n:int): real = prim("realInt",n)


fun getrealtime () : {sec : int, usec : int} =
  prim("sml_getrealtime", ())

val timebase: int = prim("get_time_base", 0)

type time = {sec : int, usec : int}

fun toReal {sec, usec} = (intToReal sec - intToReal timebase) * 1000000.0 + intToReal usec

fun timestamp () = toReal (getrealtime ())

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

fun true_ (): mask = prim("__m256d_true", ())
fun false_ (): mask = prim("__m256d_false", ())

fun printM256d (x: m256d) =
let
  val x1 = index(x, 0)
  val x2 = index(x, 1)
  val x3 = index(x, 2)
  val x4 = index(x, 3)
in printReal(x1); printReal(x2); printReal(x3); printReal(x4)
end


fun mandelbrot (y0: real, x0: real): int =
  let
    fun go (iter, x, y) =
      if (x*x + y*y <= 4.0 andalso iter < 1000)
      then go (iter + 1, x*x - y*y + x0, 2.0*x*y + y0)
      else iter
  in
    go (0, 0.0, 0.0)
  end

val zero = broadcast 0.0
val one = broadcast 1.0
val two = broadcast 2.0
val four = broadcast 4.0

(* 4 at a time *)
fun mandelbrot_simd (y' : real, x4': m256d): m256d =
  let
    val y'' = broadcast y'
    fun go (iter, mask, iters, x, y) =
      if (any mask andalso iter < 1000)
      then
        let
          val x2 = mul (x, x)
          val y2 = mul (y, y)
          val newX = add ((sub (x2, y2)), x4')
          val newY = add (mul (mul (x,y), two), y'')

          val cmp = add (x2, y2)
          val newMask = lt (cmp, four)
          val newIters = blend (iters, add (iters, one), mask)
          val newX = blend (blah, newX, mask)
          val newY = blend (y, newY, mask)
        in
          go ((iter + 1), newMask, newIters, newX, newY)
        end
      else iters
  in
    go (0, true_ (), zero, zero, zero)
  end

in
val _ =
  let
    val start1 = timestamp ()
    val _ = mandelbrot_simd (~0.6, mk (~0.4, ~0.39, ~0.38, ~0.37))
    val _ = printReal (timestamp () - start1)

    val start2 = timestamp ()
    val _ = mandelbrot (~0.6, ~0.4)
    val _ = mandelbrot (~0.6, ~0.39)
    val _ = mandelbrot (~0.6, ~0.38)
    val _ = mandelbrot (~0.6, ~0.37)
    val _ = printReal (timestamp () - start2)
  in
    ()
  end
  end
