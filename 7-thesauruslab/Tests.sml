structure Tests =
struct
  structure T = MkTreapTable(structure HashKey = IntElt)
  structure S = T.Seq
  open S

  type edge = int * int

  val testfile = "support/thesaurus.txt"

  (* A trivial test that has a graph containing 2 vertices and an edge *)
  val edgeseq = [(1,2)]
  
  (* Tests *)
  
  val testsNum = [edgeseq];

  val testsOutNeighbors = [(edgeseq, 1)]

  val testsReport = [((edgeseq, 1), 2)]

  val testsNumWords =  [testfile]

  val testsSynonyms = [(testfile, "HELLO"), (testfile, "CLEAR"),
                       (testfile, "VAGUE"), (testfile, "LOGICAL"),
                       (testfile, "ILLOGICAL")]

  val testsQuery = [(testfile, ("GOOD", "BAD")),
                    (testfile, ("CLEAR", "VAGUE")),
                    (testfile, ("LOGICAL", "ILLOGICAL")),
                    (testfile, ("EARTHLY", "POISON"))]
end
