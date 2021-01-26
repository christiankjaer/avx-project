open M256d

fun bench_blend size reps =
    let 
        (* Simple arithmetic *)
        fun init () = RealTable.tabulate (size, Real.fromInt)

        val () = bench("Scalar branch", reps, init, fn t =>
          RealTable.modify (fn x => if x > 8.0 then x else x * x) t)

        val () = bench("SIMD blend 1", reps, init, fn t =>
          RealTable.modify_simd (fn x => blend (mul (x, x), x, gts (x, 8.0))) t)

        val eight = broadcast 8.0
        val () = bench("SIMD blend 2", reps, init, fn t =>
          RealTable.modify_simd (fn x => blend (mul (x, x), x, gt (x, eight))) t)
    in
      ()
    end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_blend i 10)

val _ = List.app run [10000, 100000, 1000000, 10000000]
