functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  fun first (T : 'a table) : (key * 'a) option =
      case Tree.expose T of
          SOME {key, value, left, ...} => (case Tree.expose left of 
                                                 NONE => SOME (key,value)
                                               | _ => first left)
        | NONE => NONE 

  fun last (T : 'a table) : (key * 'a) option =
      case Tree.expose T of
          SOME {key, value, right, ...} => (case Tree.expose right of 
                                                 NONE => SOME (key,value)
                                               | _ => last right)
        | NONE => NONE

  fun previous (T : 'a table) (k : key) : (key * 'a) option =
      case Tree.expose T of 
          SOME {left, key, value, right} =>
            (case Key.compare(k, key) of
                 LESS => previous left k
               | EQUAL => last left
               | _ => (case previous right k of
                           SOME (k',v) => SOME (k',v)
                         | _ => SOME (key,value)))
        | NONE => NONE

  fun next (T : 'a table) (k : key) : (key * 'a) option =
      case Tree.expose T of 
          SOME {left, key, value, right} =>
            (case Key.compare(k, key) of
                 GREATER => next right k
               | EQUAL => first right
               | _ => (case next left k of
                           SOME (k',v) => SOME (k',v)
                         | _ => SOME (key,value)))
        | NONE => NONE

  fun join (L : 'a table, R : 'a table) : 'a table =
      Tree.join(L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
      Tree.splitAt(T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
      let
        val previousLow = previous T low
        val nextHigh = next T high
        val T' = case previous T low of
                     SOME(k,_) => let val (_,_,right) = split(T, k)
                                  in right end
                   | _ => T
        val T'' = case next T' high of
                      SOME(k,_) => let val (left,_,_) = split(T', k)
                                   in left end
                    | _ => T'
      in  
        T''
      end

end
