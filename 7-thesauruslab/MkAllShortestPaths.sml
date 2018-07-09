functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* You must define the following two types *)
  type graph = set table
  type asp = vertex seq table

  fun makeGraph (E : edge seq) : graph =
      Table.map Set.fromSeq (Table.collect E)

  fun numEdges (G : graph) : int =
      reduce (op+) 0 (map (fn s => Set.size s) (range G))

  fun numVertices (G : graph) : int =
      let
        val domain = domain G
        val range = reduce Set.union (Set.empty()) (range G)
        val vertices = Set.union(domain, range)
      in
        Set.size vertices
      end

  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
      case (find G v) of
          SOME s => Set.toSeq s
        | NONE => (empty())

  fun N G F =
      Table.reduce Set.union (Set.empty()) (Table.extract(G, F))

  fun makeASP (G : graph) (v : vertex) : asp =
      let
        fun makeASP' (X, F, edges) = 
          if (Set.size F = 0) then Table.collect edges
          else
            let
              val X' = Set.union(X, F)
              val F' = Set.difference(N G F, X')
              val seqF = Set.toSeq F
              val neighbors = map (filter (not o (Set.find X')))
                                  (map (outNeighbors G) seqF)
              val neighborEdges = map2 (fn (v, s) =>
                                        (map (fn u => (u,v)) s))
                                       seqF neighbors
              val edges' = append (edges, flatten neighborEdges)
            in
              makeASP'(X', F', edges')
            end
      in
        makeASP'(Set.empty(), Set.singleton v, empty())
      end

  fun report (A : asp) (v : vertex) : vertex seq seq =
      let
        fun report' (A, v, paths) = 
            case Table.find A v of
                SOME s =>
                  flatten (map (fn u => report'(A, u,
                               (map (fn path => u::path) paths))) s)
              | _ => paths
      in
        map (fn path => fromList path)
            (report'(A, v, singleton [v]))
      end   
end
