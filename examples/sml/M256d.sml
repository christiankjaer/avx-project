structure M256d : REAL4 = struct

  val size: int = 4

  type m256d = string

  type element = real
  type interface = real * real * real * real

  type simd = m256d
  type mask = simd

  fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))

  fun index (v: m256d, i: int): real = prim("__blockf64_sub_real", (v, i))

  fun read (v: m256d): real * real * real * real =
    (index (v,0), index (v,1), index (v,2), index (v,3))

  fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)

  fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
  fun adds (a: m256d, b: real): m256d = add(a, broadcast(b))

  fun mul (a: m256d, b: m256d): m256d = prim("__m256d_mul", (a, b))
  fun muls (a: m256d, b: real): m256d = mul(a, broadcast(b))

  fun sub (a: m256d, b: m256d): m256d = prim("__m256d_minus", (a, b))
  fun subs (a: m256d, b: real): m256d = sub(a, broadcast(b))

  fun divv (a: m256d, b: m256d): m256d = prim("__m256d_div", (a, b))
  fun divs (a: m256d, b: real): m256d = divv(a, broadcast(b))

  fun lt (a: simd, b: simd): mask = prim("__m256d_less", (a,b))
  fun le (a: simd, b: simd): mask = prim("__m256d_lesseq", (a,b))
  fun gt (a: simd, b: simd): mask = prim("__m256d_greater", (a,b))
  fun ge (a: simd, b: simd): mask = prim("__m256d_greatereq", (a,b))

  fun lts (a: simd, b: element): mask = lt (a, broadcast b)
  fun les (a: simd, b: element): mask = le (a, broadcast b)
  fun gts (a: simd, b: element): mask = gt (a, broadcast b)
  fun ges (a: simd, b: element): mask = ge (a, broadcast b)

  fun blend (a: simd, b: simd, m: mask): simd = prim("__m256d_blend", (a,b,m))

  fun all (a: mask): bool = prim("__m256d_all", a)
  fun any (a: mask): bool = prim("__m256d_any", a)

  fun product (a: simd): element = prim("__m256d_product", a)
  fun sum (a: simd): element = prim("__m256d_sum", a)

  val true_: mask = prim("__m256d_true", ())
  val false_: mask = prim("__m256d_false", ())

end
