fun mandelbrot (re: real, im: real): real =
  let
    fun go iter x y =
      if (x*x + y*y <= 4.0 andalso iter < 1000.0)
      then go (iter + 1.0) (x*x - y*y + re) (2.0*x*y + im)
      else iter
  in
    go 0.0 0.0 0.0
  end

functor Mandelbrot(Real4 : REAL4) =
struct

val zero = Real4.broadcast 0.0
fun square (x: Real4.simd): Real4.simd = Real4.mul (x, x)

(* 4 at a time *)
fun mandelbrot_simd (re: Real4.simd, im: real): Real4.simd =
  let
    val one = Real4.broadcast 1.0
    val four = Real4.broadcast 4.0
    val two = Real4.broadcast 2.0
    fun go (iter, iters, re', im') =
      let
        val re2 = square re'
        val im2 = square im'
        val mask = Real4.le (Real4.add (re2, im2), four)
      in
        if (Real4.any mask andalso iter < 1000)
        then
          let 
            val re'' = Real4.add ((Real4.sub (re2, im2)), re)
            val im'' = Real4.adds (Real4.mul (Real4.mul (re', im'), two), im)
          in
            go ( iter + 1
               , Real4.blend (iters, Real4.add (iters, one), mask)
               , Real4.blend (re', re'', mask)
               , Real4.blend (im', im'', mask)
               )
          end
      else iters
    end
  in
    go (0, zero, zero, zero)
  end

end

structure M = Mandelbrot(M256d)
structure U = VectorUtils(M256d)

val _ =
  let
    val m = bench("Mandelbrot simd", 100, fn () => (), fn () =>
    M.mandelbrot_simd (M256d.mk (~0.4, ~0.39, ~0.38, ~0.37), ~0.6))

    val mtup = bench("Mandelbrot simple", 1000, fn () => (), fn () =>
      let 
        val m1 = mandelbrot (~0.40, ~0.6)
        val m2 = mandelbrot (~0.39, ~0.6)
        val m3 = mandelbrot (~0.38, ~0.6)
        val m4 = mandelbrot (~0.37, ~0.6)
      in (m1, m2, m3, m4) end)
  in 
    print (U.toString m ^ "\n");
    print (U.toString (M256d.mk mtup) ^ "\n")
  end
