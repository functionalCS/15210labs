structure Tests =
struct
  (* testsMST: list of (E, n) pairs, where E is a list of edges (u,v,w) and
   *   n is the number of vertices
   *
   * note: sum of edge weights must be strictly less than the
   *   maximum integer (valOf Int.maxInt) *)
  val testsMST =
    [
      ([(0,2,10), (1,0,5)], 3)
    ]
end
