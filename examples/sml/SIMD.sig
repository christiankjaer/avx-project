signature SIMD = sig
  val size : int

  type element
  type interface
  type simd

  val mk : interface -> simd
  val read : simd -> interface

  val add : simd * simd -> simd
  val adds : simd * element -> simd

  val sub : simd * simd -> simd
  val subs : simd * element -> simd

  val mul : simd * simd -> simd
  val muls : simd * element -> simd

  val divs : simd * element -> simd
end
