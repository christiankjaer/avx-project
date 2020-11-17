val width: int = 1400
val height: int = 800

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

val scaleX = scale (left, right, Real.fromInt width)
val scaleY = scale (bottom, top, Real.fromInt height)


fun mandelbrot (px: int, py: int): int =
  let
    val x0 = scaleX px
    val y0 = scaleY py
    fun go iter x y =
      if (x*x + y*y <= 4.0 andalso iter < 1000)
      then go (iter + 1) (x*x - y*y + x0) (2.0*x*y + y0)
      else iter
  in
    go 0 0.0 0.0
  end

val set: int Array2.array = Array2.tabulate Array2.ColMajor (width, height, mandelbrot)

val _ =
  let
    val _ = print "P2\n"
    val _ = print ((Int.toString width) ^ " " ^ (Int.toString height) ^ "\n")
    val _ = print "1000\n"
    val _ = Array2.appi Array2.ColMajor
                       (fn (x, y, res) => print ((Int.toString res) ^ (if x = width - 1 then "\n" else " ")))
                       { base = set, row = 0, col = 0, nrows = NONE, ncols = NONE }
  in
    ()
  end
