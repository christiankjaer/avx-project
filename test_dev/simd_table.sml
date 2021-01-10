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
fun print (s:string) : unit = prim("printStringML", s)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))


structure BlockSimd = struct
type t = chararray
fun alloc (i:int) : t = prim("allocStringML", (8*i))
fun update (t:t,i:int,v:real) : unit =
    prim("__blockf64_update_real", (t,i,v))
fun update_m256 (t:t,i:int,v:m256) : unit =
    prim("__blockf64_update_real", (t,i,v))
fun sub (t:t,i:int) : real =
    prim("__blockf64_sub_real", (t,i))
fun length (t:t) : int =
    prim ("__blockf64_size", t)
val maxLen = 12345678 (* arbitrarily chosen *)
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
                           else (B.update_simd


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

fun foldri f e a =
    let fun rl j res =
	    if j >= 0 then rl (j-1) (f(j, B.sub(a,j), res))
	    else res
    in rl (length a - 1) e
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

fun map_simd (f : m256d -> m256d)

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
