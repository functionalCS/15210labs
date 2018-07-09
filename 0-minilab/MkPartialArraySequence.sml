functor MkPartialArraySequence (structure BareSeq : BARESEQUENCE)
                                  : PARTIALSEQUENCE =
struct
  open BareSeq

  (* Task 5.1 *)

  fun rev s =
      let val n = length s
      in tabulate (fn i => nth s (n-1-i)) n
      end

  fun map f s =
      tabulate (fn i => f (nth s i)) (length s)

  fun enum s =
      tabulate (fn i => (i, (nth s i))) (length s)

  fun mapIdx f s =
      map f (enum s)

  fun append (s, t) =
      let
        val n = length s
        val m = length t
      in
        tabulate (fn i => case i < n of
                              true => nth s i
                            | _ => nth t (i - n)) (n + m)
      end

  fun iter f b s =
      case length s of
          0 => b
        | _ => iter f (f(b,(nth s 0))) (drop(s, 1))

  fun toList s =
      case length s of
          0 => []
        | _ => (nth s 0)::(toList (drop(s, 1)))

  fun iterhHelper f b [] = [b]
    | iterhHelper f b (x::L) = b::(iterhHelper f (f(b, x)) L)

  fun iterh f b s =
      let
        val s' = fromList (iterhHelper f b (toList s))
        val n = (length s') - 1
      in
        (take(s', n), (nth s' n))
      end

  (* Task 5.5 *)

  fun toString f s =
      let val l = toList (map f s)
      in "<" ^ (String.concatWith "," l) ^ ">"
      end
      
end 
