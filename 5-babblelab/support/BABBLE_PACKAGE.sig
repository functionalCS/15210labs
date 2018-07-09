signature BABBLE_PACKAGE =
sig
  structure Tok : TOKEN
  structure Seq : SEQUENCE
  structure TokSeqElt : ELEMENT
  structure Util : SEQ_UTIL
  structure Data : KGRAM_DATA
  structure Babble : BABBLE
  sharing Tok = Babble.Data.Tok
  sharing Data = Babble.Data
  sharing Seq = Tok.Seq
  sharing Seq = Util.Seq
  sharing Seq = Babble.Seq

  val babble : string            (* corpus *)
            -> int               (* kgram length *)
            -> int               (* number of sentences *)
            -> (int * int)       (* (low, high) sentence length range *)
            -> Babble.Rand.rand  (* seed for randomizing *)
            -> string

  val babbleFromFile : string            (* filename *)
                    -> int               (* kgram length *)
                    -> int               (* number of sentences *)
                    -> (int * int)       (* (low, high) sentence length range *)
                    -> Babble.Rand.rand  (* seed for randomizing *)
                    -> string

  (* both babbling functions call Babble.makeParagraph, and convert the
   * results to a string.
   *
   * note that both functions are staged. For example, (babble corpus k)
   * generates the kgram table, returning a function of type
   *   int -> (int * int) -> rand -> sentence seq
   * You can use this to save time by only generating the kgram table once,
   * then babbling multiple times using the same table:
   *
   *   val babble' = babbleFromFile file k
   *   val test1 = babble' 5 (15, 30) (Rand.fromInt 42)
   *   val test2 = babble' 3 (30, 50) (Rand.fromInt 43)
   *)
end