fun bench (label: string, reps: int, init: unit -> 'b, f: 'b -> 'a): 'a =
let
  fun measure (): real * 'a =
    let
      val arg = init ()
      val start = Timer.startRealTimer ()
      val res = f arg
      val dur = Timer.checkRealTimer start
    in (Time.toReal dur, res) end
  val measurements = List.tabulate (reps, (fn _ => measure ()))

  val avg = (List.foldr (op +) 0.0 (List.map #1 measurements)) / (Real.fromInt reps)

  val _ = print (label ^ "\n    " ^ Real.toString (avg * 1000.0) ^ "ms\n")
in #2 (List.hd measurements)  end

