functor MkKGramData
  (structure Util : SEQ_UTIL
   structure Tok : TOKEN
   structure Table : TABLE where type Key.t = Tok.token Tok.Seq.seq
   sharing Table.Seq = Util.Seq
   sharing Table.Seq = Tok.Seq)
  : KGRAM_DATA =
struct
  structure Seq = Util.Seq
  structure Tok = Tok
  open Seq

  type 'a hist = ('a * int) seq
  type token = Tok.token
  type kgram = token seq

  (* redefine this type *)
  (* kgramdata is represented as a table of token sequences where the
   * keys are the first k-1 tokens in each kgram and the values are the
   * kth token in that kgram.  The int is the k that was used to create
   * the kgramdata. *)
  type kgramdata = token seq Table.table * int

  (* implement the following functions *)
  fun makeData (corpus : string) (k : int) =
    let
      val corpusTokens = Tok.tokenize corpus
      fun getKgram i =
          (take(drop(corpusTokens,i),k-1), nth corpusTokens (i+k-1))
      val kgramSeq = tabulate getKgram ((length corpusTokens)-(k-1))
      val dataSeq = collect (collate Tok.compare) kgramSeq
    in
      (Table.fromSeq dataSeq, k)
    end


  fun lookupHist ((data, k) : kgramdata) (gram : kgram) =
    case (Table.find data gram) of
          SOME a => SOME (Util.histogram Tok.compare a)
        | _ => NONE

  fun getK ((data, k) : kgramdata) = k

  fun chooseStarter ((data, k) : kgramdata) (r : real) =
    let 
      val dataSeq = Table.toSeq data
      val mappedSeq = map (fn (k,v) => tabulate (fn i => k) (length v))
                          dataSeq
      val flattenedSeq = flatten mappedSeq
      val hist = Util.histogram (collate Tok.compare) flattenedSeq
    in
      Util.choose hist r
    end

end
