functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  (* Uncomment this line once you're done *)
  exception NotYetImplemented

  (* Define this type yourself *)
  type graph = weight Table.table Table.table

  fun makeGraph (E : edge Seq.seq) : graph = 
      let
        val mapped = Seq.map (fn (v,u,w) => (v, (u,w))) E
        val collected = Table.collect mapped
      in
        Table.map (fn s => Table.fromSeq s) collected
      end

  fun findPath h G (S, T) = 
      let
        fun N(v) =
            case Table.find G v
              of NONE => Table.empty ()
               | SOME nbr => nbr

        fun findPath' D Q =
            case PQ.deleteMin Q
              of (NONE, _) => D
               | (SOME(_, (v,d)), Q') =>
                 case Table.find D v
                   of SOME _ => findPath' D Q'
                    | NONE =>
                      let
                        val insert = Table.insert (fn (x, _) => x)
                        val D' = insert (v, d) D
                        fun relax (q, (u, w)) = PQ.insert (d+w+h(u), (u,d+w)) q
                        val Q'' = Table.iter relax Q' (N v)
                      in findPath' D' Q''
                      end

        val initialQ = PQ.fromList(Seq.toList
                                   (Seq.map (fn s => (0.0, (s, 0.0)))
                                            (Set.toSeq S)))
        val paths = findPath' (Table.empty()) initialQ
        val filteredPaths = Table.filterk (fn (v,_) => Set.find T v) paths
        val mappedPaths = Table.mapk (fn (v,d) => SOME(v,d)) filteredPaths
        fun reducePaths (x, y) = 
            case (x, y) of
                (SOME(v1,d1), SOME(v2,d2)) => 
                    (case Real.compare(d1, d2) of
                         GREATER => SOME(v2, d2)
                       | _ => SOME(v1, d1))
              | (SOME(v1,d1), _) => SOME(v1,d1)
              | (_, SOME(v2,d2)) => SOME(v2,d2)
              | _ => NONE 
        val path = Table.reduce reducePaths NONE mappedPaths
      in
        path
      end                 


end
