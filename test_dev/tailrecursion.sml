local
infix  6  + - ^
infix  4  = <> > >= < <=

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

(*type m256d = string
fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))
fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)
fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a,b))
  *)
in
fun g () =
  let
    fun go (i: int) (acc: string) =
      if i = 0 then acc else go (i - 1) ""
  in go 0 "asdfasdfasdfasdfasdfasdfasasdfasdfasdf"
  end

val _ = g ()
end
