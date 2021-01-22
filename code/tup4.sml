structure Tup4 : REAL4 = struct

  val size = 4
  type t = real * real * real * real

  type element = real
  type interface = t


  type simd = t
  type mask = bool * bool * bool * bool
  
  fun mk a = a
  fun read a = a

  fun broadcast (a: real): t = (a, a, a, a)

  fun add ((a1, a2, a3, a4): t, (b1, b2, b3, b4): t) =
    (a1 + b1, a2 + b2, a3 + b3, a4 + b4)

  fun sub ((a1, a2, a3, a4): t, (b1, b2, b3, b4): t) =
    (a1 - b1, a2 - b2, a3 - b3, a4 - b4)

  fun subs ((a1, a2, a3, a4): t, b) =
    (a1 - b, a2 - b, a3 - b, a4 - b)

  fun adds ((a1, a2, a3, a4): t, b) =
    (a1 + b, a2 + b, a3 + b, a4 + b)

  fun mul ((a1, a2, a3, a4): t, (b1, b2, b3, b4): t) =
    (a1 * b1, a2 * b2, a3 * b3, a4 * b4)

  fun muls ((a1, a2, a3, a4): t, b) =
    (a1 * b, a2 * b, a3 * b, a4 * b)

  fun divv ((a1, a2, a3, a4): t, (b1, b2, b3, b4): t) =
    (a1 / b1, a2 / b2, a3 / b3, a4 / b4)

  fun divs ((a1, a2, a3, a4): t, b) =
    (a1 / b, a2 / b, a3 / b, a4 / b)

  fun blend ((a1, a2, a3, a4), (b1, b2, b3, b4), (m1, m2, m3, m4)) =
    ( if m1 then b1 else a1
    , if m2 then b2 else a2
    , if m3 then b3 else a3
    , if m4 then b4 else a4
    )

  fun all (m1, m2, m3, m4) = m1 andalso m2 andalso m3 andalso m4
  fun any (m1, m2, m3, m4) = m1 orelse m2 orelse m3 orelse m4

  fun sum ((m1, m2, m3, m4): t) = m1 + m2 + m3 + m4
  fun product ((m1, m2, m3, m4): t) = m1 * m2 * m3 * m4

  val true_ = (true, true, true, true)
  val false_ = (false, false, false, false)

  fun lt ((a1, a2, a3, a4): t, (b1, b2, b3, b4): t) =
    (a1 < b1, a2 < b2, a3 < b3, a4 < b4)

  fun lts ((a1, a2, a3, a4): t, b) =
    (a1 < b, a2 < b, a3 < b, a4 < b)

  fun le ((a1, a2, a3, a4): t, (b1, b2, b3, b4): t) =
    (a1 <= b1, a2 <= b2, a3 <= b3, a4 <= b4)

  fun les ((a1, a2, a3, a4): t, b) =
    (a1 <= b, a2 <= b, a3 <= b, a4 <= b)

  fun gt ((a1, a2, a3, a4): t, (b1, b2, b3, b4): t) =
    (a1 > b1, a2 > b2, a3 > b3, a4 > b4)

  fun gts ((a1, a2, a3, a4): t, b) =
    (a1 > b, a2 > b, a3 > b, a4 > b)

  fun ge ((a1, a2, a3, a4):t , (b1, b2, b3, b4): t) =
    (a1 >= b1, a2 >= b2, a3 >= b3, a4 >= b4)

  fun ges ((a1, a2, a3, a4): t, b) =
    (a1 >= b, a2 >= b, a3 >= b, a4 >= b)

end
