functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq
  

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Remove this exception when you're done! *)
  exception NotYetImplemented

  fun contains E (u,v,w) =
      if (length E = 0) then false
      else 
        if (length E = 1) then (u,v,w) = (nth E 0)
        else
          let
            val mid = (length E) div 2
            val (uMid, vMid, wMid) = nth E mid
          in
            case Int.compare(u, uMid) of
              EQUAL => v = vMid andalso w = wMid
            | LESS => contains (take(E, mid)) (u,v,w)
            | _ => contains (drop(E,mid)) (u,v,w)
          end

  fun minEdges n E R =
      let
        fun cmp ((_,_,w1), (_,_,w2)) =
            case Int.compare(w1,w2) of
                LESS => GREATER
              | EQUAL => EQUAL
              | _ => LESS
        val sortE = map (fn (u,v,w) => ((nth R u), (u,v,w))) (sort cmp E)
      in
        inject sortE (tabulate (fn i => (i,i,0)) n)
      end

  fun minStarContract n flips E C R T = 
      let
        val minE = minEdges n E R
        fun filterMinEdges (u,v,w) =
            ((nth flips (nth R u)) = 0) andalso ((nth flips (nth R v)) = 1)
        val contractE = filter filterMinEdges minE

        val injectR = inject (map (fn (u,v,w) =>
                                   ((nth R u),(nth R v))) contractE) R
        val R' = map (fn v => nth injectR v) injectR

        val T' = append(T, contractE)

        val collectC = collect Int.compare (map (fn (u,v,w) =>
                       ((nth R v), ((nth C (nth R u)),w))) contractE)
        fun reduceCredits ((c1,w1), (c2,w2)) =
            (Int.min(c1, c2), w1+w2)
        val mapC = map (fn (v,S) =>(v, reduce reduceCredits
                                       ((nth C v),0) S)) collectC
        val C' = inject (map (fn (v, (c,w)) => (v, c-w)) mapC) C

        val filterE = filter (not o (contains contractE)) E
        val filterRevE = filter (not o (contains contractE))
                        (map (fn (u,v,w) => (v,u,w)) filterE)
        fun filterEdges (u,v,w) = ((nth R' u) <> (nth R' v) andalso
                                     (nth C' (nth R' u) >= w) andalso
                                     (nth C' (nth R' v) >= w))
        val E' = filter filterEdges filterRevE
      in
        (E', C', R', T')
      end
           
  fun findSegments (E, n) initialCredit =
      let
        fun findSegments' seed E C R T = 
            if (length E = 0) then (R,T)
            else
              let
                val (flips, seed') = R.flip seed n
                val (E', C', R', T') = minStarContract n flips E C R T
            in
              findSegments' seed' E' C' R' T'
            end 
      in
        findSegments' (Rand.fromInt(n)) E (tabulate (fn _ => initialCredit) n) 
                      (tabulate (fn i => i) n) (empty())
      end
      
end
