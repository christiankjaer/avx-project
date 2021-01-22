open M256d

fun bench_sum size =
  let
        val t1 = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val x = bench("Simple fold", fn () =>
          RealTable.foldli (fn (_, x, y) => y + x) 0.0 t1
        )

        val t2 = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val y = bench("Sum with horizontal add", fn () =>
          RealTable.foldli_simd (fn (_, x, y) => (sum x) + y) 0.0 t2
        )

        val t3 = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val z = bench("AVX2 sum", fn () =>
          RealTable.foldli_simd (fn (_, x, y) => add (x, y)) (broadcast 0.0) t3)
  in () end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_sum i)

val _ = List.app run [10000, 100000, 1000000, 10000000, 100000000]
