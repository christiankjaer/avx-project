functor SimdTest(Vec: REAL4) =
struct

  fun printVec (x: Vec.simd) =
  let
    val (a,b,c,d): real * real * real * real = Vec.read x
    val str =
      "(" ^ Real.toString a  ^ ", "
          ^ Real.toString b  ^ ", "
          ^ Real.toString c  ^ ", "
          ^ Real.toString d  ^ ")\n"
  in
    print str
  end

  fun g (x: Vec.simd) = Vec.add(x, Vec.mk (0.0, 1.0, 2.0, 3.0))

  fun test (): unit =
  let
    val a: Vec.simd = Vec.mk (10.0, 20.0, 30.0, 40.0)
    val b: Vec.simd = Vec.mul (a, a)
    val c: Vec.simd = Vec.adds (b, 3.0)
    val d: Vec.simd = Vec.sub (c, a)
    val e: Vec.simd = g d
  in 
    printVec e;
    print ((Real.toString (Vec.sum e)) ^ "\n")
  end
end

structure Struct1 = SimdTest(M256d)
structure Struct2 = SimdTest(Tup4)

val _ = Struct1.test ()
val _ = Struct2.test ()
