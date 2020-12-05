val a = (1.0, 2.0, 3.0, 4.0)
type m256d = string

fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)

fun f () =
let
  val x1 = #1(a)
  val x2 = #2(a)
  val x3 = #3(a)
  val x4 = #4(a)
  val a: m256d = broadcast(x1)
  val b: m256d = broadcast(x2)
  val c: m256d = broadcast(x3)
  val d: m256d = broadcast(x4)
in add (add (a, b), add (c, d))
end

val _ = f ()
