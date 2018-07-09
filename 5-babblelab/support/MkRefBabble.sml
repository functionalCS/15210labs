functor MkRefBabble
  (structure Rand : RANDOM210
   structure Data : KGRAM_DATA
   structure Util : SEQ_UTIL
   sharing Data.Seq = Util.Seq
   sharing Data.Seq = Rand.Seq)
  : BABBLE =
struct
  structure Seq = Data.Seq
  structure Rand = Rand
  structure Data = Data
  open Seq

  exception NotImplemented

  type sentence = Data.token seq

  (* Implement the following functions *)
  fun makeSentence data len seed =
    raise NotImplemented

  fun makeParagraph data numSentences (lo, hi) seed =
    raise NotImplemented
end