signature KGRAM_DATA =
sig
  structure Seq : SEQUENCE
  structure Tok : TOKEN

  type 'a hist = ('a * int) Seq.seq

  type token = Tok.token
  type kgram = token Seq.seq
  type kgramdata

  val makeData : string -> int -> kgramdata
  val lookupHist : kgramdata -> kgram -> token hist option
  val getK : kgramdata -> int
  val chooseStarter : kgramdata -> real -> kgram
end
