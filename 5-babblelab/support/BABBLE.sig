signature BABBLE =
sig
  structure Seq : SEQUENCE
  structure Rand : RANDOM210
  structure Data : KGRAM_DATA

  type sentence = Data.token Seq.seq

  val makeSentence : Data.kgramdata -> int -> Rand.rand -> sentence
  val makeParagraph : Data.kgramdata -> int -> (int * int) -> Rand.rand -> sentence Seq.seq
end
