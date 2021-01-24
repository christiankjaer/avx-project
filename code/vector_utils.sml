functor VectorUtils(Vec : REAL4) =
struct

fun toString v =
  let
    val (a,b,c,d): real * real * real * real = Vec.read v
  in
      "(" ^ Real.toString a  ^ ", "
          ^ Real.toString b  ^ ", "
          ^ Real.toString c  ^ ", "
          ^ Real.toString d  ^ ")"
  end

end
