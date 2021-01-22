open M256d

fun bench_simple size =
    let 
        (* Simple arithmetic *)
        val t = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val () = bench("Scalar update", fn () =>
          RealTable.modify (fn x => (x + 2.0) * x) t)

        val () = bench("SIMD update", fn () =>
          RealTable.modify_simd (fn x => mul (adds (x, 2.0), x)) t)

        val two = broadcast 2.0
        val t = RealTable.tabulate (size, (fn x => Real.fromInt x))
        val () = bench("SIMD update without broadcast", fn () =>
          RealTable.modify_simd (fn x => mul (add (x, two), x)) t)
    in
      ()
    end

fun run i =
  (print ("\n" ^ Int.toString i ^ " elements\n");
  bench_simple i)

val _ = List.app run [10000, 100000, 1000000, 10000000, 100000000]
