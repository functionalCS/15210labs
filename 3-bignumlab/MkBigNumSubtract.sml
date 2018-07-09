functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++ --

  fun appendZeros (x, n) = 
      append(x, (tabulate (fn i => ZERO) n))

  fun makeSameLength (x, y) =
      let
        val lenX = length x
        val lenY = length y
      in
        case Int.compare(lenX, lenY) of
            LESS => ((appendZeros(x, lenY - lenX)), y)
          | GREATER => (x, (appendZeros(y, lenX - lenY)))
          | _ => (x, y)
      end

  fun flipBits b =
      case b of
          ZERO => ONE
        | _ => ZERO

  fun removeZeros x = 
      let
        fun mapZeros (i, x) =
            case x of
                ZERO => 0
              | _ => i
        val maxIdx = reduce Int.max 0 (mapIdx mapZeros x)
      in
        take(x, maxIdx + 1)
      end

  fun x ++ y = BNA.add (x, y)

  fun x -- y =
      let
        val (newX, newY) = makeSameLength(x, y)
        
        val flippedY = map flipBits newY

        val difference = newX ++ (flippedY ++ singleton(ONE))
      in
        removeZeros(take(difference, (length difference - 1)))
      end

  val sub = op--

end
