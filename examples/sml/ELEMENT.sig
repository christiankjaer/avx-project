signature ELEMENT = sig
  type t
  val add : t * t -> t
  val sub : t * t -> t
  val mul : t * t -> t
  val divi : t * t -> t
  val eq : t * t -> bool
  val lt : t * t -> bool
  val ge : t * t -> bool
end
