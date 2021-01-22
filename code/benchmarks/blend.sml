open M256d

fun bench_blend size =
    let 
        (* Simple arithmetic *)
        val t = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val () = bench("Scalar branch", fn () =>
          RealTable.modify (fn x => if x > 8.0 then x else x * x) t)

        val t = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val () = bench("SIMD blend 1", fn () =>
          RealTable.modify_simd (fn x => blend (mul (x, x), x, gts (x, 8.0))) t)

        val eight = broadcast 8.0
        val t = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val () = bench("SIMD blend 2", fn () =>
          RealTable.modify_simd (fn x => blend (mul (x, x), x, gt (x, eight))) t)
    in
      ()
    end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_blend i)

val _ = List.app run [10000, 100000, 1000000, 10000000, 100000000]
