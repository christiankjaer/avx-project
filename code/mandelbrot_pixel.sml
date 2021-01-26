structure V = M256d
structure M = Mandelbrot(V)
structure U = VectorUtils(V)

val _ =
  let
    val m = bench("Mandelbrot simd", 10, fn () => (), fn () =>
    M.mandelbrot_simd (V.mk (~0.4, ~0.39, ~0.38, ~0.37), ~0.60))

    val mtup = bench("Mandelbrot simple", 10, fn () => (), fn () =>
      let 
        val m1 = mandelbrot (~0.40, ~0.60)
        val m2 = mandelbrot (~0.39, ~0.60)
        val m3 = mandelbrot (~0.38, ~0.60)
        val m4 = mandelbrot (~0.37, ~0.60)
      in (m1, m2, m3, m4) end)
  in 
    print (U.toString m ^ "\n");
    print (U.toString (V.mk mtup) ^ "\n")
  end
