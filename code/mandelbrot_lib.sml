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
    val vim = broadcast im
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
            val im'' = add (mul (mul (re', im'), two), vim)
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
