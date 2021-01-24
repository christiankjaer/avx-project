signature REAL4 = sig

  val size: int

  type element = real
  type interface = real * real * real * real

  type simd
  type mask

  val mk : interface -> simd
  val read : simd -> interface

  val broadcast : element -> simd

  val add : simd * simd -> simd
  val adds : simd * element -> simd

  val sub : simd * simd -> simd
  val subs : simd * element -> simd

  val mul : simd * simd -> simd
  val muls : simd * element -> simd

  val divv : simd * simd -> simd
  val divs : simd * element -> simd

  val lt : simd * simd -> mask
  val lts : simd * element -> mask

  val le : simd * simd -> mask
  val les : simd * element -> mask

  val gt : simd * simd -> mask
  val gts : simd * element -> mask

  val ge : simd * simd -> mask
  val ges : simd * element -> mask

  val and_ : mask * mask -> mask
  val or_ : mask * mask -> mask
  val not_ : mask -> mask

  val all : mask -> bool
  val any : mask -> bool

  val product : simd -> element
  val sum : simd -> element

  val true_ : mask
  val false_ : mask

  val blend : simd * simd * mask -> simd

end
