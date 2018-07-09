functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
      (* This is the template we recommend you use for your divide
       * and conquer algorithm. You do NOT have to use it, however.
       * Delete this comment when you have completed skyline.
       *)
      let    
        fun copy ((x1,y1), (x2,y2)) =
            case y2 of
                SOME _ => (x2,y2)
              | _ => (x2,y1)

        fun cmpSkyline ((x1,y1), (x2,y2)) = Int.compare(x1,x2)

        fun maxSkyline ((x1,y1), (x2,y2)) =
            case (y1,y2) of
                (SOME a, SOME b) => (x1, Int.max(a,b))
              | (SOME a, NONE) => (x1, a)
              | (NONE, SOME a) => (x1, a)
              | _ => (x1, 0)

        fun filterSkyline S =
            filterIdx (fn (i, (x,y)) =>
                       case i of 
                           0 => true
                         | _ => let val (_,prev) = (nth S (i-1))
                                in y <> prev
                                end) S

        fun computeSkyline S =
            case showt S of
                EMPTY => empty()
              | ELT (l,h,r) => append (singleton (l,h), singleton (r,0))
              | NODE(L, R) => 
                let
                  val (skylineL, skylineR) =
                    par (fn () => computeSkyline L,
                         fn () => computeSkyline R)
                  
                  val mergedL = merge cmpSkyline
                                (map (fn (x,y) => (x, SOME y)) skylineL)
                                (map (fn (x,y) => (x, NONE)) skylineR)
                  val mergedR = merge cmpSkyline
                                (map (fn (x,y) => (x, SOME y)) skylineR)
                                (map (fn (x,y) => (x, NONE)) skylineL)

                  val scannedL = scani copy (0,NONE) mergedL
                  val scannedR = scani copy (0,NONE) mergedR

                  val combined = map2 maxSkyline scannedL scannedR

                  val skyline = filterSkyline combined
                in
                  skyline
                end
      in
        computeSkyline buildings
      end

end
