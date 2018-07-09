functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Remove this before submitting! *)
  exception NYI

  (* Define this yourself *)
  type countTable = (Key.t * unit table) table

  fun makeCountTable (S : point seq) : countTable =
      let
        val sortedS = Seq.sort (fn ((x1,y1),(x2,y2)) => compareKey(x1,x2)) S
        val (sweepT, _) = Seq.iterh (fn (T,(x,y)) =>
                                     insert (fn (y1,y2) => y1) (y, ()) T)
                                    (empty()) sortedS 
        val S' = Seq.map2 (fn ((x,y),T) => (x,(y,T))) sortedS sweepT
      in
        fromSeq S'
      end


  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
      let
        val xLeftKV = case previous T xLeft of
                                 SOME (k,v) => (case next T k of
                                                    SOME kv => SOME kv
                                                  | _ => NONE)
                               | _ => first T
        val xRghtKV = case next T xRght of
                                 SOME (k,v) => (case previous T k of
                                                    SOME kv => SOME kv
                                                  | _ => NONE)
                               | _ => last T            
        val leftSize = case xLeftKV of
            SOME (k,(y,T)) => size (getRange T (yLo,yHi)) + 
                              (if ((compareKey(y, yLo) = LESS) orelse
                                  (compareKey(y, yHi) = GREATER))
                               then 1
                               else 0)
          | _ => size T
        val rghtSize = case xRghtKV of
            SOME (k,(y,T)) => size (getRange T (yLo,yHi)) +
                              (if ((compareKey(y, yLo) = LESS) orelse
                                  (compareKey(y, yHi) = GREATER))
                               then 0
                               else 1)
          | _ => 0
      in
        Int.max(rghtSize - leftSize, 0)
      end

end
