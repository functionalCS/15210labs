structure Tester =
struct
  open ArraySequence

  structure Bridges : BRIDGES =
    MkBridges(structure STSeq = MkSTSequence(structure Seq = ArraySequence))

  functor MkAStar(structure Vtx : HASHKEY) : ASTAR =
    MkAStarCore(structure Table = MkTreapTable(structure HashKey = Vtx)
                structure PQ = MkSkewBinomialHeapPQ(structure OrdKey = RealElt))

  structure IntAStar : ASTAR =
    MkAStar(structure Vtx = IntElt)

  structure StringAStar : ASTAR =
    MkAStar(structure Vtx = StringElt)

  structure BridgesEdge = 
    MkPairElt(structure EltA = IntElt
              structure EltB = IntElt)

  structure IntAStarEdge =
    MkTripleElt(structure EltA = IntElt
                structure EltB = IntElt
                structure EltC = RealElt)

  structure IntAStarFindPath = 
    MkOptionElt(structure Elt =  
                  MkPairElt(structure EltA = IntElt
                            structure EltB =RealElt))
  
  (* Tests *)

  val bridgeTests = [[],
                     [(0,1)],
                     [(0,1),(1,2)],
                     [(0,1),(1,2),(1,3),(2,3)],
                     [(0,1),(1,2),(0,3),(3,4),(3,5),(4,5),(5,6)],
                     [(0,1),(0,2),(0,4),(1,2),(1,3),(2,3),(4,5),(4,6),(4,9),
                      (5,6),(5,7),(5,8),(7,8)]]

  val intAStarTests = [([(0,1,1.0)], ([0],[1])),
                       ([(0,1,1.0),(1,2,2.0)], ([0],[2])),
                       ([(0,1,1.0),(0,2,4.0),(1,2,2.0)], ([0],[2])),
                       ([(0,1,1.0),(0,2,4.0),(1,2,2.0)], ([0],[1,2])),
                       ([(0,1,1.0),(0,2,2.0),(0,3,3.0),
                         (1,2,4.0),(1,3,5.0),(2,3,1.0)], ([0,1],[2,3]))]
  
  fun runTests (_, _, _, []) = ()
    | runTests (f, iToS, oToS, x::L) =
      let val () = print ("Input " ^ iToS x ^ "; got " ^ oToS (f x) ^ ".\n")
      in runTests (f, iToS, oToS,L)
      end

  fun testBridges () =
      let
        fun mapTests E = fromList E
        fun testBridges' E = Bridges.findBridges (Bridges.makeGraph E)
      in
        runTests(testBridges',
                 (toString BridgesEdge.toString),
                 (toString BridgesEdge.toString),
                 List.map mapTests bridgeTests)
      end

  fun testIntAStar () =
      let              
        fun mapTests (E, (S,T)) =
            ((fn _ => 0.0), fromList E,
             (IntAStar.Set.fromSeq (fromList S), IntAStar.Set.fromSeq (fromList T))) 
        fun testIntAStar' (h, E, (S,T)) = IntAStar.findPath h (IntAStar.makeGraph E) (S,T)
        fun iToS (h, E, (S,T)) = 
            "(" ^ (toString IntAStarEdge.toString) E ^ "," ^
            "(" ^ (toString Int.toString) (IntAStar.Set.toSeq S)  ^ "," ^
                  (toString Int.toString) (IntAStar.Set.toSeq T)  ^ ")" ^ ")"
      in
        runTests(testIntAStar',
                 iToS,
                 IntAStarFindPath.toString,
                 List.map mapTests intAStarTests)
      end
      
end
