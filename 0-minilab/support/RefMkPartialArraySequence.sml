functor RefMkPartialArraySequence (structure BareSeq : BARESEQUENCE)
                                     : PARTIALSEQUENCE =
struct
  open BareSeq

  fun rev s =
      let val n = length s
      in tabulate (fn i => nth s (n-1-i)) n
      end

  (* Brought up here because functions below need it *)
  fun toList s = List.tabulate (length s, (fn i => nth s i))

  fun map f s = fromList (List.map f (toList s))

  fun enum s =
      let
        fun enum' i nil = nil
          | enum' i (x::xs) = List.@ ([(i,x)], (enum' (i+1) xs))
      in fromList (enum' 0 (toList s))
      end

  fun mapIdx f s =
      let
        fun mapIdx' i nil = nil
          | mapIdx' i (x::xs) = List.@ ([f (i,x)], (mapIdx' (i+1) xs))
      in fromList (mapIdx' 0 (toList s))
      end

  fun append (s, t) = fromList (List.@ (toList s, toList t))

  fun iter f b s =
      let
        val n = length s
        val ss = tabulate (fn i => take (s, i)) (n+1)
        fun each t =
            List.foldl (fn (a,b) => f (b,a)) b (toList t)
        val all = List.map each (toList ss)
      in List.nth (all, n)
      end

  fun iterh f b s =
      let
        val n = length s
        val ss = tabulate (fn i => subseq s (0, i)) (n+1)
        fun each t =
            List.foldl (fn (a,b) => f (b,a)) b (toList t)
        val all = map each ss
      in (take (all, n), nth all n)
      end

  fun toString f s =
      let
        fun interComma (str, e) = str ^ (f e) ^ ","
        val res = iter interComma "" s
        val right = Int.max(0, String.size res - 1)
      in
        "<" ^ String.substring (res, 0, right) ^ ">"
      end
end 
