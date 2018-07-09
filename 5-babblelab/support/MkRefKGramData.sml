(* reference solution - for testing only. DO NOT EDIT THIS FILE.
 * 
 * these solutions are purposefully stupid. do not try to imitate them in
 * your own solutions. *)

functor MkRefKGramData
  (structure Util : SEQ_UTIL
   structure Tok : TOKEN
   structure Table : TABLE where type Key.t = Tok.token Tok.Seq.seq
   sharing Table.Seq = Util.Seq
   sharing Table.Seq = Tok.Seq)
  : KGRAM_DATA =
struct
  structure Seq = Util.Seq
  structure Tok = Tok
  open Util
  open Seq

  exception NotImplemented

  type 'a hist = ('a * int) seq
  type token = Tok.token
  type kgram = token seq

  type kgramdata = token seq * int

  fun makeData corpus k =
    (Tok.tokenize corpus, k)

  fun both (p, q) = 
    p andalso q

  fun lookupHist (data, k) gram =
    if length gram <> (k - 1) then NONE else
    let
      val slen = length data

      fun tokEq (x, y) =
        Tok.compare (x, y) = EQUAL

      fun isMatchAt ind =
        let
          val indStats = drop (data, ind)
        in
          if (length indStats) < k then false
          else reduce both true (map2 tokEq gram indStats)
        end

      fun getExt ind =
        if isMatchAt ind
        then SOME (nth data (ind+k-1))
        else NONE

      val extS = map valOf (filter isSome (tabulate getExt slen)
        handle Range => (empty ()))
    in
      if (length extS = 0) then NONE
      else SOME (histogram Tok.compare extS)
    end

  fun getK (_, k) = k

  fun chooseStarter data p =
    raise NotImplemented
end