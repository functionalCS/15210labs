functor MkBabblePackage
  (structure Tok : TOKEN
   structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Tok.Seq
   sharing Seq = Rand.Seq) : BABBLE_PACKAGE =
struct
  structure Tok = Tok
  structure Seq = Seq

  structure Util = MkSeqUtil(structure Seq = Seq)

  structure TokSeqElt =
    MkSeqElt(structure Elt = Tok.Elt
             structure Seq = Seq)

  structure TokSeqTable =
    MkBSTTable(structure Tree = MkTreap(structure HashKey = TokSeqElt)
               structure Seq = Seq)

  structure Data =
    MkKGramData(structure Util = Util
                structure Tok = Tok
                structure Table = TokSeqTable)

  structure Babble =
    MkBabble(structure Rand = Rand
             structure Data = Data
             structure Util = Util)

  fun babble corpus k =
    let
      val data = Babble.Data.makeData corpus k
      fun babble' numSentences (low, high) seed =
        let
          val paragraph = Babble.makeParagraph data numSentences (low, high) seed
          val sentenceStrings = Seq.map Tok.toSentenceString paragraph
        in
          String.concatWith " " (Seq.toList sentenceStrings) ^ "\n"
        end
    in
      babble'
    end

  fun babbleFromFile file =
    babble (TextIO.inputAll (TextIO.openIn file))
end
