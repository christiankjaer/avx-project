structure M256d : SIMD = struct

  val size = 4

  type element = real
  type interface = real * real * real * real
  type simd = Real64Block4.t
  type mask = Word32.word

  fun add (a: m256d, b: m256d): m256d = prim("mm256_add", (a, b))
  fun adds (a: m256d, b: real): m256d = prim("mm256_adds", (a, b))

  (* and so on *)


end
