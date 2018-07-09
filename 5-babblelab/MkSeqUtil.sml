functor MkSeqUtil(structure Seq : SEQUENCE) : SEQ_UTIL =
struct
  structure Seq = Seq
  open Seq

  type 'a hist = ('a * int) seq

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (x,s) => (x, length s)) (collect cmp (map (fn x => (x,1)) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
    let
      val (a, _) = nth hist 0
      val (_, total) = reduce (fn ((x,i), (y,j)) => (x,i+j)) (a,0) hist
      val cumulative = scani (fn ((x,i), (y,j)) =>
                              (y,i+j))
                             (a,0) hist
      val realCumulative = map (fn (x,i) =>
                                (x,Real.fromInt(i)/Real.fromInt(total)))
                               cumulative
      fun leastAboveOrEqual ((x,i), (y,j)) = 
          case (Real.compare(i,p), Real.compare(j,p),
                Real.compare(i,j)) of
              (LESS, _, _) => (y,j)
            | (_, LESS, _) => (x,i)
            | (_, _, LESS) => (x,i)
            | _ => (y,j)
      val (closest, _) = reduce leastAboveOrEqual (a,0.0) realCumulative
    in
      closest
    end

end
