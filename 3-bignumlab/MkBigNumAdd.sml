functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++

  datatype carry = GEN | PROP | STOP

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

  fun x ++ y =
      let
        fun bitsToCarry (b1, b2) = 
            case (b1,b2) of 
                (ONE, ONE) => GEN
              | (ZERO, ZERO) => STOP
              | _ => PROP

        fun copyCarry (c1, c2) = 
           case (c1,c2) of
               (a, PROP) => a
             | (_, b) => b

        fun carryToBit (c1, c2) =
           case (c1,c2) of 
               (PROP, GEN) => ZERO
             | (_, GEN) => ONE
             | (PROP, STOP) => ONE
             | _ => ZERO

        val (newX, newY) = makeSameLength(x, y)

        val carrySum = map2 bitsToCarry newX newY

        val (scanCarrySum, lastCarry) = scan copyCarry STOP carrySum

        val bitSum = map2 carryToBit carrySum scanCarrySum
      in
        case lastCarry of
            STOP => bitSum
          | _ => append(bitSum, singleton(ONE))
      end

  val add = op++

end
