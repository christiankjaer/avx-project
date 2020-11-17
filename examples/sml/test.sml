structure Int4 = Tup4(
  struct
    type t = int
    val add = op +
    val sub = op -
    val mul = op *
  end)

structure Real4 = Tup4(
  struct
    type t = real
    fun add (x, y) = (x + y): real
    fun sub (x, y) = (x - y): real
    fun mul (x, y) = (x * y): real
  end)


val x: Int4.simd = Int4.mk (1,2,3,4)

val y: Int4.simd = Int4.mk (5,6,7,8)

val res = Int4.read (Int4.mul (x, y))

val _ = print (Int.toString (#1 res))
val _ = print "\n"

val z: Real4.simd = Real4.mk (1.0,2.0,3.0,4.0)

val zz: Real4.simd = Real4.mk (5.0,6.0,7.0,8.0)

val res2 = Real4.read (Real4.mul (z, zz))

val _ = print (Real.toString (#1 res2))
