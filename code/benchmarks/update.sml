open M256d

fun bench_simple size reps =
    let 
        fun init () = RealTable.tabulate (size, Real.fromInt)
        (* Simple arithmetic *)
        val _ = bench("Scalar update", reps, init,
          fn t => ((RealTable.modify (fn x => (x + 2.0) * x) t); t))


        val _ = bench("SIMD update", reps, init, fn t =>
          ((RealTable.modify_simd (fn x => mul (adds (x, 2.0), x)) t); t))

        val two = broadcast 2.0
        val _ = bench("SIMD update without broadcast", reps, init, fn t =>
          ((RealTable.modify_simd (fn x => mul (add (x, two), x)) t); t))

    in
      ()
    end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_simple i 10)

val _ = List.app run [1000, 10000, 100000, 1000000, 10000000, 100000000]
