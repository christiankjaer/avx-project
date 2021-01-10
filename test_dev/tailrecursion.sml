infix  6  + - ^
infix  4  = <> > >= < <=

type m256d = string
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)
fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a,b))

fun g () =
  let
    fun go (i: int) (acc: m256d) =
      if i = 0 then acc else go (i - 1) (add (broadcast 1.0, acc))
  in go 10 (broadcast 0.0)
  end

val _ = g ()
