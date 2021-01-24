functor SimdTest(Vec: REAL4) =
struct

  structure Utils = VectorUtils(Vec)

  fun g (x: Vec.simd) = Vec.add(x, Vec.mk (0.0, 1.0, 2.0, 3.0))

  fun max (v1: Vec.simd, v2: Vec.simd): Vec.simd =
    Vec.blend (v1, v2, Vec.lt (v1, v2))

  fun loop (x: Vec.simd): Vec.simd =
    if Vec.all (Vec.gts (x, 9000.0)) then x
    else loop (Vec.adds (x, 3.0))


  fun test (): unit =
  let
    val a: Vec.simd = Vec.mk (10.0, 20.0, 30.0, 40.0)
    val b: Vec.simd = Vec.mul (a, a)
    val c: Vec.simd = Vec.adds (b, 3.0)
    val d: Vec.simd = Vec.sub (c, a)
    val e: Vec.simd = g d
    val x = Vec.mk (1.0,5.0,7.0,9.0)
    val y = Vec.mk (3.0,4.0,7.0,10.0)
  in 
    print (Utils.toString (loop a) ^ "\n");
    print (Utils.toString (max (x, y)) ^ "\n");
    print (Utils.toString e ^ "\n");
    print ((Real.toString (Vec.sum e)) ^ "\n")
  end
end

structure Struct1 = SimdTest(M256d)
structure Struct2 = SimdTest(Tup4)

val _ = print "AVX version\n\n"
val _ = Struct1.test ()

val _ = print "\nTuple version\n\n"
val _ = Struct2.test ()
