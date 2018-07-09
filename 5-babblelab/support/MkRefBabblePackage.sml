functor MkRefBabblePackage
  (structure Tok : TOKEN
   structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Tok.Seq
   sharing Seq = Rand.Seq) : BABBLE_PACKAGE =
struct
  structure Tok = Tok
  structure Seq = Seq

  structure Util = MkRefSeqUtil(structure Seq = Seq)

  structure TokSeqElt =
    MkSeqElt(structure Elt = Tok.Elt
             structure Seq = Seq)

  structure TokSeqTable =
    MkBSTTable(structure Tree = MkTreap(structure HashKey = TokSeqElt)
               structure Seq = Seq)

  structure Data =
    MkRefKGramData(structure Util = Util
                   structure Tok = Tok
                   structure Table = TokSeqTable)

  structure Babble =
    MkRefBabble(structure Rand = Rand
                structure Data = Data
                structure Util = Util)

  exception NotImplemented

  fun babble corpus k =
    raise NotImplemented

  fun babbleFromFile file =
    raise NotImplemented
end