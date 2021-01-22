open M256d

fun bench_reduction size =
  let
        val t1 = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val x = bench("Scalar", fn () =>
          RealTable.foldli (fn (_, x, y) => y andalso x < 500.0) true t1)

        val t2 = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val foo = broadcast 500.0
        val y = bench("Vector 1", fn () =>
          RealTable.foldli_simd (fn (_, x, y) => y andalso all (lt (x, foo)))
          true t2)

        val t3 = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val z = bench("Vector 2", fn () =>
          RealTable.foldli_simd (fn (_, x, y) => y andalso all (lts (x, 500.0)))
          true t3)

  in () end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_reduction i)

val _ = List.app run [10000, 100000, 1000000, 10000000, 100000000]
