signature BARESEQUENCE =
sig
  type 'a seq
  datatype 'a listview = NIL | CONS of 'a * 'a seq

  exception Range
  exception Size

  val empty : unit -> 'a seq
  val singleton : 'a -> 'a seq

  val nth : 'a seq -> int -> 'a
  val length : 'a seq -> int

  val tabulate : (int -> 'a) -> int -> 'a seq
  val fromList : 'a list -> 'a seq

  val subseq : 'a seq -> int * int -> 'a seq
  val take : 'a seq * int -> 'a seq
  val drop : 'a seq * int -> 'a seq
  
  val showl : 'a seq -> 'a listview
end
