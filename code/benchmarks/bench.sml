fun bench (label: string, f: unit -> 'a): 'a =

let
  val start = Timer.startRealTimer ()
  val res = f ()
  val dur = Timer.checkRealTimer start
  val _ = print (label ^ "\n    " ^ Real.toString (Time.toReal dur * 1000.0) ^ "ms\n")
in res end

