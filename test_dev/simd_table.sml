local
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)
fun (a: real) / (b: real) : real = prim("divFloat", (a,b))
fun print (s:string) : unit = prim("printStringML", s)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))

type m256d = string
type mask = string

fun broadcast (a: real): m256d = prim("__m256d_broadcast", a)
fun printReal (n:real): unit = prim("printReal",n)
fun intToReal (n:int): real = prim("realInt",n)
fun mk (a: real, b: real, c: real, d: real): m256d = prim("__blockf64", (a,b,c,d))
fun mul (a: m256d, b: m256d): m256d = prim("__m256d_mul", (a, b))
fun add (a: m256d, b: m256d): m256d = prim("__m256d_plus", (a, b))
fun gt (a: m256d, b: m256d): m256d = prim("__m256d_greater", (a, b))
fun lt (a: m256d, b: m256d): m256d = prim("__m256d_less", (a, b))
fun all (a: mask): bool = prim("__m256d_all", a)
fun blend (a: m256d, b: m256d, m: mask): m256d = prim("__m256d_blend", (a,b,m))

fun product (a: m256d): real = prim("__m256d_product", a)
fun sum (a: m256d): real = prim("__m256d_sum", a)

fun index (v: m256d, i: int): real = prim("__blockf64_sub_real", (v, i))
fun read (v: m256d): real * real * real * real =
  (index (v,0), index (v,1), index (v,2), index (v,3))

fun getrealtime () : {sec : int, usec : int} =
  prim("sml_getrealtime", ())

val timebase: int = prim("get_time_base", 0)

type time = {sec : int, usec : int}

fun toReal {sec, usec} = (intToReal sec - intToReal timebase) * 1000000.0 + intToReal usec

fun timestamp () = toReal (getrealtime ())

structure BlockSimd = struct

type t = chararray

fun alloc (i:int) : t = prim("allocStringML", (8*i))
fun update (t:t,i:int,v:real) : unit =
    prim("__blockf64_update_real", (t,i,v))
fun update_m256d (t:t,i:int,v:m256d) : unit =
    prim("__blockf64_update_m256d", (t,i,v))
fun sub (t:t,i:int) : real =
    prim("__blockf64_sub_real", (t,i))
fun sub_m256d (t:t,i:int) : m256d =
    prim("__blockf64_sub_m256d", (t,i))
fun length (t:t) : int =
    prim ("__blockf64_size", t)
val maxLen = 268435456 (* arbitrarily chosen *)

end

structure RealTable = struct

structure B = BlockSimd
type elem = real
type m256d = string

type vector = B.t
type array = B.t

val maxLen = B.maxLen
val length = B.length

fun check_index (n, i) =
    if 0 <= i andalso i < n then ()
    else raise Subscript

local
  fun check_size n =
      if 0 <= n andalso n <= maxLen then ()
      else raise Size
in
fun alloc_table n =
    (check_size n; B.alloc n)
end

fun sub (t:B.t,i:int) : elem =
    (check_index(length t, i); B.sub(t,i))

fun update (t:B.t,i:int,x:real) : unit =
    (check_index(length t, i); B.update(t,i,x))

fun tabulate (n, f : int -> real) : B.t =
    let fun init (t, f, i) = if i >= n then t
			     else (B.update (t, i, f i); init (t, f, i+1))
    in init (alloc_table n, f, 0)
    end

fun tabulate_simd (n, f : int -> m256d) : B.t =
    let fun init (t, f, i) = if i >= n then t
			     else (B.update_m256d (t, i, f i); init (t, f, i+4))
    in init (alloc_table n, f, 0) (* TODO: bounds checking with write mask or so *)
    end


fun vector a = tabulate (length a, fn i => B.sub(a,i))

(* array n gives an initialised array with n elements. *)
(* Raise Size if n > maxLen.                           *)
fun array (n,x) =
    let val a = alloc_table n
        fun lr j =
	    if j < n then (B.update (a,j,x);
                           lr (j+1))
             else ()
    in lr 0; a
    end

fun updatev (t, i, x) =
    (check_index(length t, i);
     tabulate (length t,fn j => if i=j then x else B.sub(t,j)))

fun app f a =
    let val n = length a
        fun lr j =
	    if j < n then (f (B.sub (a, j));
                           lr (j+1))
	    else ()
    in lr 0
    end

fun foldli f e a =
    let val stop = length a
	fun lr j res =
	    if j < stop then lr (j+1) (f(j, B.sub(a,j), res))
	    else res
    in lr 0 e
    end

fun foldli_simd f e a =
    let val stop = length a
	fun lr j res =
	    if j < stop then lr (j+4) (f(j, B.sub_m256d(a,j), res))
	    else res
    in lr 0 e
    end

fun foldri f e a =
    let fun rl j res =
	    if j >= 0 then rl (j-1) (f(j, B.sub(a,j), res))
	    else res
    in rl (length a - 1) e
    end

fun foldri_simd f e a =
    let fun rl j res =
	    if j >= 0 then rl (j-4) (f(j, B.sub_m256d(a,j), res))
	    else res
    in rl (length a - 4) e
    end

fun appi f a =
    let val stop = length a
	fun lr j =
	    if j < stop then (f(j, B.sub(a,j));
                              lr (j+1))
	    else ()
    in lr 0
    end

fun mapi (f : int * elem -> elem) (a : B.t) : B.t =
    let val stop = length a
	val newvec = B.alloc stop  (* no check_size needed *)
	fun lr j =
	    if j < stop then
	      (B.update(newvec, j, f(j, B.sub(a,j)));
	       lr (j+1))
	    else ()
    in lr 0; newvec
    end

fun modifyi f a =
    let val stop = length a
	fun lr j =
	    if j < stop then (B.update(a,j,f(j, B.sub(a,j)));
                              lr (j+1))
	    else ()
    in lr 0
    end

fun modify f a =
    let val n = length a
        fun lr j =
	    if j < n then (B.update (a, j, f (B.sub (a, j)));
                           lr (j+1))
	    else ()
    in lr 0
    end

fun modify_simd f a =
    let val n = length a
        fun lr j =
	    if j < n then (B.update_m256d (a, j, f (B.sub_m256d (a, j)));
                           lr (j+4))
	    else ()
    in lr 0
    end

(* The following are only for the Vector structure: *)
fun map (f : elem -> elem) (a : B.t) : B.t =
    let val n = length a
        val b : B.t = B.alloc n   (* no check_size needed *)
	fun lr j =
	    if j < n then (B.update (b, j, f (B.sub (a, j)));
                           lr (j+1))
	    else b
    in lr 0
    end

fun map_simd (f : m256d -> m256d) (a : B.t) : B.t =
    let val n = length a
        val b : B.t = B.alloc n   (* no check_size needed *)
	fun lr j =
	    if j < n then (B.update_m256d (b, j, f (B.sub_m256d (a, j)));
                           lr (j+4))
	    else b
    in lr 0
    end

fun mapi (f : int * elem -> elem) (a : B.t) : B.t =
    let val stop = length a
	val newvec = B.alloc stop  (* no check_size needed *)
	fun lr j =
	    if j < stop then
	      (B.update(newvec, j, f(j, B.sub(a,j)));
	       lr (j+1))
	    else ()
    in lr 0; newvec
    end

end
in

fun bench_sum size =
  let
        val t1 = RealTable.tabulate (size, (fn x => intToReal x))
        val start1 = timestamp ()
        val x = RealTable.foldri (fn (_, x, y) => y + x) 0.0 t1
        val _ = printReal (timestamp () - start1)
        val _ = printReal x

        val t2 = RealTable.tabulate (size, (fn x => intToReal x))
        val start2 = timestamp ()
        val y = RealTable.foldri_simd (fn (_, x, y) => (sum x) + y) 0.0 t2
        val _ = printReal (timestamp () - start2)
        val _ = printReal y

        val t3 = RealTable.tabulate (size, (fn x => intToReal x))
        val start2 = timestamp ()
        val y = RealTable.foldri_simd (fn (_, x, y) => add (y, x)) (broadcast 0.0) t3
        val _ = printReal (timestamp () - start2)
        val _ = printReal (sum y)
  in () end

fun bench_reduction size =
  let
        val t1 = RealTable.tabulate (size, (fn x => intToReal x))
        val start1 = timestamp ()
        val x = RealTable.foldli (fn (_, x, y) => y andalso x < 500.0) true t1
        val _ = printReal (timestamp () - start1)

        val t2 = RealTable.tabulate (size, (fn x => intToReal x))
        val start2 = timestamp ()
        val blah = broadcast 500.0
        val y = RealTable.foldli_simd (fn (_, x, y) =>
                    let val tmp = all (lt (x, blah)) in y andalso tmp end) true
                    t2
        val _ = printReal (timestamp () - start2)
  in () end

fun bench_simple size =
    let 
        (* Simple arithmetic *)
        val t = RealTable.tabulate (size, (fn x => intToReal x))
        val t1 = timestamp ()
        val _ = RealTable.modify (fn x => (x + x) * x) t
        val _ = printReal (timestamp () - t1)

        val t = RealTable.tabulate (size, (fn x => intToReal x))
        val t2 = timestamp ()
        val _ = RealTable.modify_simd (fn x => mul (add (x, x), x)) t
        val _ = printReal (timestamp () - t2)

    in
      ()
    end

fun bench_branch size =
    let 
        (* Simple conditional *)
        val t = RealTable.tabulate (size, (fn x => intToReal x))
        val t3 = timestamp ()
        val _ = RealTable.modify (fn x => if x > 8.0 then x else x * x) t
        val _ = printReal (timestamp () - t3)

        val t = RealTable.tabulate (size, (fn x => intToReal x))
        val t4 = timestamp ()
        val l = broadcast 4096.0
        val _ = RealTable.modify_simd (fn x =>
          blend (mul (x, x), x, gt (x, l))
        ) t
        val _ = printReal (timestamp () - t4)

    in
      ()
    end

val _ =
  let
    val _ = bench_sum (8)
  in ()
  end

end
