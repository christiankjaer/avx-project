open M256d

fun bench_reduction size reps =
  let
        fun init () = RealTable.tabulate (size, Real.fromInt)

        val x = bench("Scalar", reps, init, fn t =>
          RealTable.foldli (fn (_, x, y) => y andalso x < 500.0) true t)

        val foo = broadcast 500.0
        val y = bench("Vector 1", reps, init, fn t =>
          RealTable.foldli_simd (fn (_, x, y) => y andalso all (lt (x, foo)))
          true t)

        val z = bench("Vector 2", reps, init, fn t =>
          RealTable.foldli_simd (fn (_, x, y) => y andalso all (lts (x, 500.0)))
          true t)

  in () end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_reduction i 4)

val _ = List.app run [10000, 100000, 1000000, 10000000, 100000000]
