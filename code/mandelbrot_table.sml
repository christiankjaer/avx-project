val width: int = 1200 (* assume is divisible by 4 *)
val height: int = 800

val left = ~2.5
val bottom = ~1.0
val top = 1.0
val right = 1.0

val stepX = (right - left) / (Real.fromInt width)
val stepY = (top - bottom) / (Real.fromInt height)

structure V = M256d
structure M = Mandelbrot(V)
structure U = VectorUtils(V)

fun mandel () = RealTable.tabulate_simd (width * height, fn i =>
  let
    val xstart = i mod width
    val x1 = left + (Real.fromInt xstart * stepX)
    val x2 = left + (Real.fromInt (xstart + 1) * stepX)
    val x3 = left + (Real.fromInt (xstart + 2) * stepX)
    val x4 = left + (Real.fromInt (xstart + 3) * stepX)
    val y = bottom + (Real.fromInt (i div width) * stepY)
  in
    M.mandelbrot_simd (V.mk (x1, x2, x3, x4), y)
  end
)

val _ =
  let
    val m = mandel ()
    val _ = print "P2\n"
    val _ = print ((Int.toString width) ^ " " ^ (Int.toString height) ^ "\n")
    val _ = print "1000\n"
    val _ = RealTable.appi
                       (fn (i, res) => print ((Int.toString (Real.floor res)) ^ (if (i mod width) = width - 1 then "\n" else " ")))
                       m
  in
    ()
  end
