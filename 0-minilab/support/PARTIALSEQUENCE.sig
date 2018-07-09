signature PARTIALSEQUENCE =
sig
  (* BARESEQUENCE *)
  include BARESEQUENCE

  (* New functions *)
  val rev : 'a seq -> 'a seq
  
  val map : ('a -> 'b) -> 'a seq -> 'b seq
  val enum : 'a seq -> (int * 'a) seq
  val mapIdx : ((int * 'a) -> 'b) -> 'a seq -> 'b seq
  val append : 'a seq * 'a seq -> 'a seq

  val iter : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
  val iterh : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq * 'b

  val toList : 'a seq -> 'a list
  val toString : ('a -> string) -> 'a seq -> string
end
