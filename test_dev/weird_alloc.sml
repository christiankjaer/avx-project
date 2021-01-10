infix  6  + - ^
infix  4  = <> > >= < <=

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

type m256d = string

fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))
fun index (v: m256d, i: int): real = prim("__blockf64_sub_real", (v, i))
fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a,b))

fun printReal (n:real): unit = prim("printReal",n)
fun printM256d (x: m256d) =
let
  val x1 = index(x, 0)
  val x2 = index(x, 1)
  val x3 = index(x, 2)
  val x4 = index(x, 3)
in printReal(x1); printReal(x2); printReal(x3); printReal(x4)
end


fun g (i: int) (v: m256d) =
  if i = 0 then v else
  let
    val x = mk (1.1, 1.2, 1.3, 1.4)
  in g (i - 1) (add (x, v))
  end

val _ =
  let
    val res = printM256d (g 100000 (mk (1.0, 1.0, 1.0, 1.0)))
  in ()
  end
