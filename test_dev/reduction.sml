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

fun product (a: m256d): real = prim("__m256d_product", a)
fun sum (a: m256d): real = prim("__m256d_sum", a)

fun printReal (n:real): unit = prim("printReal",n)

in
val _ = printReal (product (mk (1.0, 2.0, 3.0, 4.0)))

val _ = printReal (sum (mk (1.0, 2.0, 3.0, 4.0)))
end
