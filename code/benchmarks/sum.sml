open M256d

fun bench_sum size reps =
  let
        fun init () = RealTable.tabulate (size, Real.fromInt)

        val y = bench("Sum with horizontal add", reps, init, fn t =>
          RealTable.foldli_simd (fn (_, x, y) => (sum x) + y) 0.0 t
        )

        val z = bench("Vector sum", reps, init, fn t =>
          RealTable.foldli_simd (fn (_, x, y) => add (x, y)) (broadcast 0.0) t)

        val x = bench("Simple fold", reps, init, fn t =>
          RealTable.foldli (fn (_, x, y) => y + x) 0.0 t
        )

  in () end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_sum i 4)

val _ = List.app run [10000, 100000, 1000000, 10000000, 100000000]
