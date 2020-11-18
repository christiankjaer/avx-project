functor Tup4(Elm: ELEMENT) : SIMD = struct

  val size = 4

  type element = Elm.t
  type interface = Elm.t * Elm.t * Elm.t * Elm.t
  type simd = Elm.t * Elm.t * Elm.t * Elm.t
  
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

end
