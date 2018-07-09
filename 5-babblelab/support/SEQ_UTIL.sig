signature SEQ_UTIL =
sig
  structure Seq : SEQUENCE

  type 'a hist = ('a * int) Seq.seq

  val histogram : 'a Seq.ord -> 'a Seq.seq -> 'a hist
  val choose : 'a hist -> real -> 'a
end
