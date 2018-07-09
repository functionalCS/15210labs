functor MkStringToken(structure Seq : SEQUENCE) = 
struct
  structure Seq = Seq
  structure Elt = StringElt

  type token = Elt.t

  fun tokenize corpus =
    Seq.fromList (String.tokens (not o Char.isAlphaNum) corpus)

  val compare = String.compare

  fun toString tok = tok

  fun toSentenceString toks = 
    String.concatWith " " (Seq.toList toks) ^ "."
end