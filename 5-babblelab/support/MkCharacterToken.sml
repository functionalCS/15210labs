functor MkCharacterToken(structure Seq : SEQUENCE) : TOKEN =
struct
  structure Seq = Seq
  (* I would make a CharElt structure, but Hashing structure huh?? *)
  structure Elt = StringElt

  type token = Elt.t

  fun tokenize corpus =
    let
      val words = String.tokens (not o Char.isAlphaNum) corpus
      val noSymbolsCorpus = String.concatWith " " words
    in
      Seq.map Char.toString (Seq.fromList (String.explode noSymbolsCorpus))
    end

  val compare = String.compare
  
  fun toString tok = tok

  fun toSentenceString toks =
    String.concat (Seq.toList toks) ^ "."
end