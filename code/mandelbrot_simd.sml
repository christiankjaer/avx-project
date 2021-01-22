val width: int = 100
val height: int = 250

functor Mandelbrot(Real4 : REAL4) = struct


val imageWidth = Real.fromInt (width * Real4.size)

val left = ~2.5
val bottom = ~1.0
val top = 1.0
val right = 1.0

fun scale (from: real, to: real, dom: real) (x: int): real =
  let
    val x0 = Real.fromInt x
  in
    (x0 / dom) * (to - from) + from
  end

fun scaleX (x: int): Real4.simd =
  let
    val xr = Real.fromInt (x * Real4.size)
    val x0 = Real4.mk (xr, xr + 1.0, xr + 2.0, xr + 3.0)
  in
    Real4.adds (Real4.muls (Real4.divs (x0, imageWidth), (right - left)), left)
  end

val scaleY = scale (bottom, top, Real.fromInt height)

fun zero () = Real4.broadcast 0.0
fun one () = Real4.broadcast 1.0


(* 4 at a time *)
fun mandelbrot (py: int, px4: int): Real4.simd =
  let
    val x0 = scaleX px4
    val y0 = scaleY py
    fun go (iter, mask, iters, x, y) =
      if (Real4.any mask andalso iter < 1000)
      then
        let
          val x2 = Real4.mul (x, x)
          val y2 = Real4.mul (y, y)
          val newX = Real4.add ((Real4.sub (x2, y2)), x0)
          val newY = Real4.adds (Real4.muls (Real4.mul (x,y), 2.0), y0)

          val cmp = Real4.add (x2, y2)
          val newMask = Real4.lts (cmp, 4.0)
        in
          go (iter + 1, newMask, Real4.blend (iters, Real4.add (iters, one ()), mask), Real4.blend (x, newX, mask), Real4.blend (y, newY, mask))
        end
      else iters
  in
    go (0, Real4.true_, zero (), zero (), zero ())
  end

fun showSimd (x: Real4.simd) =
  let
    val (a,b,c,d) = Real4.read x
  in (Int.toString (Real.floor a)) ^ " " ^ (Int.toString (Real.floor b)) ^ " " ^
     (Int.toString (Real.floor c)) ^ " " ^ (Int.toString (Real.floor d))
  end

end

structure AVXMandel = Mandelbrot(M256d)

val set: M256d.simd Array2.array = Array2.tabulate Array2.RowMajor (height, width, AVXMandel.mandelbrot)

val _ =
  let
    val _ = print "P2\n"
    val _ = print ((Int.toString (width * M256d.size)) ^ " " ^ (Int.toString height) ^ "\n")
    val _ = print "1000\n"
    val _ = Array2.appi Array2.RowMajor
                       (fn (y, x, res) => print ((AVXMandel.showSimd res) ^ (if x = width - 1 then "\n" else " ")))
                       { base = set, row = 0, col = 0, nrows = NONE, ncols = NONE }
  in
    ()
  end
