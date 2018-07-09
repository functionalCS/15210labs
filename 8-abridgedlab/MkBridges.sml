functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph =
      let
        val allE = append (E, map (fn (v,u) => (u,v)) E)
        val collected = collect Int.compare allE
      in
        map (fn (v,s) => s) collected
      end

  fun findBridges (G : ugraph) : edges =
      if (length G = 0) then empty()
      else
        let
          fun findBridges' (X, i, v) = 
              let
                fun iterEdge ((iterX, j), u) =
                    let
                      val (uIndex, _, _) = STSeq.nth iterX u
                      val (iterX', j') =
                        if (uIndex = 0)
                        then findBridges' (STSeq.update (u, (j,j,v)) iterX,
                                           j+1, u)
                        else (iterX, j)
                      val (vIndex, vLow, vParent) = STSeq.nth iterX v
                      val (_, uLow, _) = STSeq.nth iterX u
                    in
                      if (vParent = u) then (iterX', j')
                      else (STSeq.update
                            (v, (vIndex,Int.min(vLow,uLow),vParent))
                            iterX', j')
                    end
              in
                iter iterEdge (X, i) (nth G v)
              end
          val n = length G
          val initialX = STSeq.fromSeq (tabulate (fn i => (0,n,0)) n)
          val (numbers, _) = findBridges'
                             (STSeq.update (0, (1,1,~1)) initialX, 2, 0)
          fun mapVertices (i, (index, low, parent)) = 
              if (i <> 0 andalso low >= index) then (parent, i)
              else (~1,~1)
        in
          filter (fn (v,u) => v >= 0 andalso u >= 0)
                 (mapIdx mapVertices (STSeq.toSeq numbers))
        end

end
