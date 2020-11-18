val width: int = 200
val height: int = 500

structure Real4 = Tup4(
  struct
    type t = real
    fun add (x, y) = (x + y): real
    fun sub (x, y) = (x - y): real
    fun mul (x, y) = (x * y): real
    fun divi (x, y) = (x / y): real
  end)

structure Int4 = Tup4(
  struct
    type t = int
    fun add (x, y) = (x + y): int
    fun sub (x, y) = (x - y): int
    fun mul (x, y) = (x * y): int
    fun divi (x, y) = (x div y): int
  end)

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
    val xr = Real.fromInt x * 4.0
    val x0 = Real4.mk (xr, xr + 1.0, xr + 2.0, xr + 3.0)
  in
    Real4.adds (Real4.muls (Real4.divs (x0, imageWidth), (right - left)), left)
  end

val scaleY = scale (bottom, top, Real.fromInt height)

val zero = Real4.mk (0.0, 0.0, 0.0, 0.0)
val zeroi = Int4.mk (0, 0, 0, 0)

fun le (xs, y): Int4.simd =
  let
    val (a, b, c, d) = Real4.read xs
  in
    ( if a <= y then 1 else 0
    , if b <= y then 1 else 0
    , if c <= y then 1 else 0
    , if d <= y then 1 else 0
    )
  end

(* false when all zeroes *)
fun someMask (mask: Int4.simd): bool =
  let
    val (a, b, c, d) = Int4.read mask
  in
    a = 1 orelse b = 1 orelse c = 1 orelse d = 1
  end

fun withMask old new mask =
  let
    val (a,b,c,d) = Real4.read old
    val (e,f,g,h) = Real4.read new
    val (m1,m2,m3,m4) = Int4.read mask
  in
    ( if m1 = 1 then e else a
    , if m2 = 1 then f else b
    , if m3 = 1 then g else c
    , if m4 = 1 then h else d
    )
  end


(* 4 at a time *)
fun mandelbrot (px4: int, py: int): Int4.simd =
  let
    val x0 = scaleX px4
    val y0 = scaleY py
    fun go iter mask iters x y =
      if (someMask mask andalso iter < 1000)
      then
        let
          val newIters = Int4.add (iters, mask) (* increment with mask? *)
          (* only update where mask is 1 *)
          val newX = Real4.add ((Real4.sub (Real4.mul (x,x), Real4.mul (y,y))), x0)
          val newY = Real4.adds (Real4.muls (Real4.mul (x,y), 2.0), y0)
          val cmp = Real4.add (Real4.mul (x, x), Real4.mul (y, y))
          val newMask = le (cmp, 4.0)
        in
          go (iter + 1) newMask newIters (withMask x newX mask) (withMask y newY mask)
        end
      else iters
  in
    go 0 (Int4.mk (1,1,1,1)) zeroi zero zero
  end


val set: Int4.simd Array2.array = Array2.tabulate Array2.ColMajor (width, height, mandelbrot)


val _ =
  let
    fun showSimd (x: Int4.simd) =
      let
        val (a,b,c,d) = Int4.read x
      in (Int.toString a) ^ " " ^ (Int.toString b) ^ " " ^
         (Int.toString c) ^ " " ^ (Int.toString d)
      end
    val _ = print "P2\n"
    val _ = print ((Int.toString (width * Real4.size)) ^ " " ^ (Int.toString height) ^ "\n")
    val _ = print "1000\n"
    val _ = Array2.appi Array2.ColMajor
                       (fn (x, y, res) => print ((showSimd res) ^ (if x = width - 1 then "\n" else " ")))
                       { base = set, row = 0, col = 0, nrows = NONE, ncols = NONE }
  in
    ()
  end
