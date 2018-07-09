functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq

  (* Remove when you're done! *)
  exception NotYetImplemented

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun findSeam G =
      let
        val n = length G
        val m = length (nth G 0)
        fun gMin ((i1,L1),(i2,L2)) =
            case (Real.compare(i1,i2)) of
                LESS => (i1,L1)
              | _ => (i2,L2)
        fun findSeam' i prevM = 
            if (i >= n) then prevM
            else
              let
                fun mapG (j, g) =
                    let
                      val (prevG, prevJ) = 
                        (if (m = 1) then nth prevM 0
                         else if (j = 0) then
                                  gMin((nth prevM j),(nth prevM (j+1)))
                         else if (j = m-1) then
                                  gMin((nth prevM (m-2)),(nth prevM (m-1)))
                         else gMin(gMin((nth prevM (j-1)),(nth prevM j)),
                                   (nth prevM (j+1))))
                   in
                     (prevG + g, j::prevJ)
                   end
              in
                findSeam' (i+1) (mapIdx mapG (nth G i))
              end
        val finalM = findSeam' 0 (tabulate (fn i => (0.0, [])) m)
        val (_, bestSeam) = reduce gMin (nth finalM 0) finalM
      in
        fromList (List.rev bestSeam)
      end                                                                           
end
