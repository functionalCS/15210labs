functor MkSeqFun (structure Seq : PARTIALSEQUENCE) : SEQFUN =
struct
  open Seq

  exception BadHarmonics

  (* Task 5.3 *)

  fun allHarmonics n =
      case n<=0 of
          true => raise BadHarmonics
        | _ => let val (t, b) = iterh (fn (b, i) => b + 1.0/i) 1.0
                                          (tabulate (fn i => Real.fromInt(i + 2)) n)
                   in t
                   end
                     
  (* Task 5.4 *)

  fun groupedHarmonics n k =
  	  let val s = allHarmonics n
  	  in tabulate (fn i => (i, take(drop(s, k*i), Int.min(k, n - k*i))))
  	  			  ((n + k - 1) div k)
  	  end
		
  (* Task 5.6 *)

  fun printGroups G =
      let val groupToStr = fn (n, s) => "(" ^ (Int.toString n) ^ "," ^
      									(toString Real.toString s) ^ ")"
      in print ((toString groupToStr G) ^ "\n")
      end
      
end
