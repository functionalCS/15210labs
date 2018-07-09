functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

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

  fun makeEvenLength (x, y) =
      case (length x) mod 2 of
          0 => (x, y)
        | _ => (appendZeros(x, 1), appendZeros(y, 1))

  fun shiftLeft (x, n) =
      case length x of
          0 => empty()
        | _ => append((tabulate (fn i => ZERO) n), x)

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
      let
        val (newX, newY) = makeSameLength(x, y)
      in 
        case length newX of 
            0 => empty()
          | 1 => (case (nth newX 0, nth newY 0) of
                      (ONE, ONE) => singleton(ONE)
                    | _ => empty())
          | _ => let
                   val (newX, newY) = makeEvenLength(newX, newY)
                   val n = length newX

                   val p = drop(newX, n div 2)
                   val q = take(newX, n div 2)
                   val r = drop(newY, n div 2)
                   val s = take(newY, n div 2)
 
                   val (a,b,c) = par3(fn () => p ** r, fn () => q ** s,
                                 fn () => ((p ++ q) ** (r ++ s)))
                   val c' = c -- a -- b 
                  
                   val product = shiftLeft(a, n) ++ 
                                 shiftLeft(c', n div 2) ++ b
                 in
                   product
                 end
      end

  val mul = op**
end
