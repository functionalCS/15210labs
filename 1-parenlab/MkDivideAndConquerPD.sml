functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210

  (* My divide-and-conquer algorithm utilizes a function pd.  Given a
   * paren sequence, pd returns the following values in a tuple: number of
   * parenthesis left of the rightmost unmatched CPAREN, number of
   * parenthesis between the rightmost unmatched  CPAREN and the leftmost
   * unmatched OPAREN, number of parenthesis right of the leftmost unmatched
   * OPAREN, number of unmatched CPARENs, an int option which is SOME x
   * where x is the maximum parenthesis distance between the between the
   * rightmost unmatched  CPAREN and the leftmost unmatched OPAREN or
   * NONE if no matched substring has been found.  pd is called on paren
   * and the int option representing the maximum parenthesis distance is
   * returned.  This is because we know paren is closed so the entire
   * sequence will be represented in the middle segment when pd parens returns.
   * 
   * pd is a divide-and-conquer algorithm because it take a paren sequence
   * and cases on the treeview of that sequence.  If the treeview is EMPTY,
   * ELT OPAREN, or ELT CPAREN the appropriate values are returned.  Negative
   * middle segment lengths are returned if the sequence has no unmatched
   * CPARENS and/or no unmatched OPARENS to preserve the integrity of the
   * values when they are combined.  If the treeview is a NODE, pd is called
   * recursively on the right and left subsequences.  The values these calls
   * returned are then combined to return the correct values for the entire
   * sequence.  This combining cases in the number of unmatched OPARENS in
   * the right subsequence and the number of unmatched CPARENS in the left
   * subsequence.
   *)

  fun parenDist (parens : paren seq) : int option =
      let
        fun pd s =
            case showt s of
                EMPTY => (0,~2,0,0,0,NONE)
              | ELT OPAREN => (0,~1,0,0,1,NONE)
              | ELT CPAREN => (0,~1,0,1,0,NONE)
              | NODE (l, r) =>
                let
                  val ((a,b,c,d,e,f), (g,h,i,j,k,l)) =
                    par (fn () => pd l, fn () => pd r)
                in
                  case Int.compare(e, j) of
                      LESS => (a+b+c+g+2, h, i, j-e+d, k, l)
                    | GREATER => (a, b, c+g+h+i+2, d, e-j+k, f)
                    | _ => (a, b+c+g+h+2, i, d, k,
                            intMax(intMax(f,l),SOME (c+g+2)))
                end
        val (_,_,_,_,_,max) = pd parens
      in
        max
      end

end
