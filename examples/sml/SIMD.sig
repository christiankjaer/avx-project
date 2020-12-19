signature SIMD = sig
  val size : int

  type element
  type interface

  type simd
  type mask

  val mk : interface -> simd
  val read : simd -> interface

  val add : simd * simd -> simd
  val adds : simd * element -> simd

  val sub : simd * simd -> simd
  val subs : simd * element -> simd

  val mul : simd * simd -> simd
  val muls : simd * element -> simd

  val divs : simd * element -> simd

  val lt : simd * simd -> mask
  val lts : simd * element -> mask

  val geq : simd * simd -> mask
  val geqs : simd * element -> mask

  val all : mask -> bool
  val any : mask -> bool

  val true_ : mask
  val false_ : mask

  val blend : simd * simd * mask -> simd


end
