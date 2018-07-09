functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Option210

  (* My brute force solution first generates a sequence of all subsequences of
   * paren.  For each subsequence it checks whether the first paren is a OPARN,
   * the last paren is a CPAREN, and that the inside of the subsequence is
   * matched. Each subsequence, s, is mapped to SOME (length s) if these are
   * true and NONE otherwise.  This mapped sequence is then reduced using
   * intMax to return the maximum parenthesis distance. 
   * 
   * This is a brute force algorithm beacuse it generates all possible
   * subsequences, filters out those that are not matched, and then determines
   * the maximum subsequence lenght of the remaining subsequences.
   *)

  fun subsets (s : 'a seq) : 'a seq seq=
      let
        val n = length s
      in
        flatten (tabulate (fn i =>
                           tabulate (fn len => subseq s (i, len+1)) (n-i))
                          n)                  
      end
  
  fun parenMatch (p : paren seq) : bool =
      let
        fun pm ((NONE, _) | (SOME 0, CPAREN)) = NONE
          | pm (SOME c, CPAREN) = SOME (c-1)
          | pm (SOME c, OPAREN) = SOME (c+1)
      in
        iter pm (SOME 0) p = (SOME 0)
      end
  
  fun parenClose (s : paren seq) : int option = 
      case (length s) mod 2 of
          0 => let
                 val s' = take (s, length s -1)
                 val s'' = drop (s', 1)
               in
                 case ((nth s 0), (nth s (length s - 1)), parenMatch s'') of
                     (OPAREN, CPAREN, true) => SOME (length s)
                   | _ => NONE
               end
        | _ => NONE
      

  fun parenDist (parens : paren seq) : int option =
      reduce intMax (NONE) (map parenClose (subsets parens))

end
