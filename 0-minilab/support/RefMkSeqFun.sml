functor RefMkSeqFun (structure Seq : PARTIALSEQUENCE) : SEQFUN =
struct
  open Seq

  exception Unimplemented
  exception BadHarmonics

  fun allHarmonics n =
      if n <= 0 then raise BadHarmonics
      else let
        fun genKthTrgle k =
            if k = 1 then 1.0 else 1.0 / (Real.fromInt k) + (genKthTrgle (k-1))
      in
        tabulate (fn i => genKthTrgle (i+1)) n
      end

  fun groupedHarmonics n k =
      let
        val (nDiv, nMod) = (n div k, n mod k)
        val nGrps = nDiv + (if nMod <> 0 then 1 else 0)
        val reps = tabulate (fn _ => allHarmonics n) nGrps
        fun mkPairs i =
            (i*k, if i = nGrps-1 andalso nMod <> 0 then nMod else k)
        val pairs = tabulate mkPairs nGrps
        fun mkGrps (i, S) =
            let
              val L = toList S
              val (st, m) = nth pairs i
              val L' = List.take (List.drop (L, st), m)
            in
              (i, fromList L')
            end
      in
        mapIdx mkGrps reps
      end

  fun printGroups G =
      raise Unimplemented
end
