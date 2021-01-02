structure M256d : SIMD = struct

  val size: int = 4

  type element = real
  type interface = real * real * real * real
  type m256d = string

  type simd = m256d
  type mask = simd

  fun add (a: m256d, b: m256d): m256d = prim("mm256_add", (a, b))
  fun adds (a: m256d, b: real): m256d = prim("mm256_adds", (a, b))

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

  fun divv (a: m256d, b: m256d): m256d = prim("__m256d_div", (a, b))
  fun divs (a: m256d, b: real): m256d = divv(a, broadcast(b))

  fun lt (a: simd, b: simd): mask = prim("__m256d_less", (a,b))
  fun le (a: simd, b: simd): mask = prim("__m256d_lesseq", (a,b))
  fun gt (a: simd, b: simd): mask = prim("__m256d_greater", (a,b))
  fun ge (a: simd, b: simd): mask = prim("__m256d_greatereq", (a,b))

  fun lts (a: simd, b: element): mask = lt (a, broadcast b)
  fun ges (a: simd, b: element): mask = ge (a, broadcast b)

  fun blend (a: simd, b: simd, m: mask): simd = prim("__m256d_blend", (a,b,m))

  fun all (a: mask): bool = prim("__m256d_all", a)
  fun any (a: mask): bool = prim("__m256d_any", a)

  (* TODO: Make optimized version of these constants *)
  val false_: mask = lt (broadcast 0.0, broadcast 0.0)
  val true_: mask = le (broadcast 0.0, broadcast 0.0)

end
