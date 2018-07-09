signature SEQFUN =
sig
  type 'a seq
  exception BadHarmonics

  val allHarmonics : int -> real seq
  val groupedHarmonics : int -> int -> (int * real seq) seq
  val printGroups : (int * real seq) seq -> unit
end