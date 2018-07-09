functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (* You must define the following type *)
  type thesaurus = ASP.graph

  fun make (S : (string * string seq) seq) : thesaurus =
      let
        fun thesaurusMap (u, S) = map (fn v => (u,v)) S
        val edges = flatten (map thesaurusMap S)
      in
        ASP.makeGraph edges
      end

  fun numWords (T : thesaurus) : int =
      ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
      ASP.outNeighbors T w

  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
      let
        val w1ASP = ASP.makeASP T w1
      in
        ASP.report w1ASP w2
      end
end
