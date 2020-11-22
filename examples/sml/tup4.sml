functor Tup4(Elm: ELEMENT) : SIMD = struct

  val size = 4

  type element = Elm.t
  type interface = Elm.t * Elm.t * Elm.t * Elm.t
  type simd = Elm.t * Elm.t * Elm.t * Elm.t
  type mask = bool * bool * bool * bool
  
  fun mk a = a
  fun read a = a

  fun add ((a1, a2, a3, a4), (b1, b2, b3, b4)) =
    (Elm.add (a1, b1), Elm.add (a2, b2), Elm.add (a3, b3), Elm.add (a4, b4))

  fun sub ((a1, a2, a3, a4), (b1, b2, b3, b4)) =
    (Elm.sub (a1, b1), Elm.sub (a2, b2), Elm.sub (a3, b3), Elm.sub (a4, b4))

  fun subs ((a1, a2, a3, a4), b) =
    (Elm.sub (a1, b), Elm.sub (a2, b), Elm.sub (a3, b), Elm.sub (a4, b))

  fun adds ((a1, a2, a3, a4), b) =
    (Elm.add (a1, b), Elm.add (a2, b), Elm.add (a3, b), Elm.add (a4, b))

  fun mul ((a1, a2, a3, a4), (b1, b2, b3, b4)) =
    (Elm.mul (a1, b1), Elm.mul (a2, b2), Elm.mul (a3, b3), Elm.mul (a4, b4))

  fun muls ((a1, a2, a3, a4), b) =
    (Elm.mul (a1, b), Elm.mul (a2, b), Elm.mul (a3, b), Elm.mul (a4, b))

  fun divs ((a1, a2, a3, a4), b) =
    (Elm.divi (a1, b), Elm.divi (a2, b), Elm.divi (a3, b), Elm.divi (a4, b))

  fun blend ((a1, a2, a3, a4), (b1, b2, b3, b4), (m1, m2, m3, m4)) =
    ( if m1 then b1 else a1
    , if m2 then b2 else a2
    , if m3 then b3 else a3
    , if m4 then b4 else a4
    )

  fun all (m1, m2, m3, m4) = m1 andalso m2 andalso m3 andalso m4
  fun any (m1, m2, m3, m4) = m1 orelse m2 orelse m3 orelse m4

  val true_ = (true, true, true, true)
  val false_ = (false, false, false, false)

  fun eq ((a1, a2, a3, a4), (b1, b2, b3, b4)) =
    (Elm.eq (a1, b1), Elm.eq (a2, b2), Elm.eq (a3, b3), Elm.eq (a4, b4))

  fun eqs ((a1, a2, a3, a4), s) =
    (Elm.eq (a1, s), Elm.eq (a2, s), Elm.eq (a3, s), Elm.eq (a4, s))

  fun lt ((a1, a2, a3, a4), (b1, b2, b3, b4)) =
    (Elm.lt (a1, b1), Elm.lt (a2, b2), Elm.lt (a3, b3), Elm.lt (a4, b4))

  fun lts ((a1, a2, a3, a4), s) =
    (Elm.lt (a1, s), Elm.lt (a2, s), Elm.lt (a3, s), Elm.lt (a4, s))


end
