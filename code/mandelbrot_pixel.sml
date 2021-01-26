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
open Real4

val zero = broadcast 0.0
fun square (x: simd): simd = mul (x, x)

(* 4 at a time *)
fun mandelbrot_simd (re: simd, im: real): simd =
  let
    val one = broadcast 1.0
    val four = broadcast 4.0
    val two = broadcast 2.0
    fun go (iter, iters, re', im') =
      let
        val re2 = square re'
        val im2 = square im'
        val mask = le (add (re2, im2), four)
      in
        if (iter < 1000 andalso any mask)
        then
          let 
            val re'' = add ((sub (re2, im2)), re)
            val im'' = adds (mul (mul (re', im'), two), im)
          in
            go ( iter + 1
               , blend (iters, add (iters, one), mask)
               , blend (re', re'', mask)
               , blend (im', im'', mask)
               )
          end
      else iters
    end
  in
    go (0, zero, zero, zero)
  end

end

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
