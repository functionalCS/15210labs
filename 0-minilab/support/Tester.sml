structure TesterHelper =
struct
  structure BareSeq = BareArraySequence

  structure RefPartSeq = RefMkPartialArraySequence (structure BareSeq = BareSeq)
  structure StuPartSeq = MkPartialArraySequence (structure BareSeq = BareSeq)
  
  local
    open RefPartSeq

    (* Local helper functions *)

    fun collate cmp (s1, s2) =
        case (showl s1, showl s2) of
            (NIL, NIL) => EQUAL
          | (NIL, _) => LESS
          | (_, NIL) => GREATER
          | (CONS (x, xs), CONS (y, ys)) =>
              case cmp (x, y) of
                  EQUAL => collate cmp (xs, ys)
                | ord => ord

    fun equal cmp (S, T) = ((collate cmp) (S, T)) = EQUAL

    fun listToString f l =
        let
          val str = toString f (fromList l)
          val len = String.size str
          val left = Int.min(1, len)
          val right = Int.max(0, len-2)
        in
          "[" ^ (String.substring (str, left, right)) ^ "]"
        end

    fun pairToStr toSx toSy (x, y) =
        "(" ^ (toSx x) ^ ", " ^ (toSy y) ^ ")"

    fun pairCompare xCmp yCmp ((x1,y1), (x2,y2)) =
        case (xCmp (x1,x2), yCmp (y1,y2)) of
            (EQUAL, EQUAL) => EQUAL (* Only care about equality here *)
          | _ => GREATER  (* Arbitrarily pick GREATER. Doesn't matter *)

    fun pairEqual xCmp yCmp P = (pairCompare xCmp yCmp P) = EQUAL

    fun fuzzyRealCmp (a, b) =
        if (Real.== (a, Real.posInf) orelse Real.== (b, Real.posInf))
        then Real.compare (a, b)
        else Int.compare (Real.round (100.0 * a), Real.round (100.0 * b))
  in
    (* Instead of using elements, because we don't have access to
     * the standard ArraySequence in this lab, so we can't use the
     * Mk___Elt functors.
     *)

    (* string *)
    val strToStr = (fn e:string => e)
                            
    (* int seq *)
    val intSeqToStr = toString Int.toString
    val intSeqEqual = equal Int.compare

    (* string seq *)
    val strSeqToStr = toString strToStr
    val strSeqEqual = equal String.compare

    (* (int * int) seq *)
    val intIntSeqToStr = toString (pairToStr Int.toString Int.toString)
    val intIntSeqEqual = equal (pairCompare Int.compare Int.compare)

    (* (string seq * string seq) *)
    val strSeqStrSeqToStr = (pairToStr strSeqToStr strSeqToStr)

    (* (string seq * string) *)
    val strSeqStrToStr = (pairToStr strSeqToStr strToStr)
    val strSeqStrEqual = (pairEqual (collate String.compare) String.compare)

    (* int list *)
    val intListToStr = listToString Int.toString

    (* bool seq *)
    val boolSeqToStr = toString Bool.toString
    val boolSeqEqual = equal (fn (x:bool,y) => if x=y then EQUAL else GREATER)

    (* (int * int) *)
    val intIntToStr = (pairToStr Int.toString Int.toString)

    (* real seq *)
    val realSeqToStr = toString Real.toString
    val realSeqEqual = equal fuzzyRealCmp

    (* (int * real seq) seq *)
    val intRealSeqSeqToStr = toString (pairToStr Int.toString realSeqToStr)
    val intRealSeqSeqEqual = equal (pairCompare Int.compare (collate fuzzyRealCmp))
  end
end

structure PartSeqTester =
struct
  open StudentTestSuite
  open TesterHelper

  functor MkAutoPartSeq (structure PartSeq : PARTIALSEQUENCE) =
  struct
    open PartSeq
    fun mapIntDouble S = map (fn x => 2*x) S
    fun mapStrJoin S = map (fn x => x ^ "!") S
    fun enumInt S = enum S
    fun mapIdxIntMult S = mapIdx (fn (i,x) => i*x) S
    fun appendStr (S : string seq, T) = append (S, T)
    fun iterhIntJoin S = iterh (fn (s,x) => s^(Int.toString x)) "" S
    fun iterIntMult S = iter (fn (s,x) => s*x) 1 S
    fun toListInt S : int list = toList S
    fun toStringInt S = toString Int.toString S
    fun toStringBool S = toString Bool.toString S
  end

  structure RefAutoPartSeq = MkAutoPartSeq (structure PartSeq = RefPartSeq)
  structure StuAutoPartSeq = MkAutoPartSeq (structure PartSeq = StuPartSeq)
  
  local
    (* mapIntDouble *)
    val mapIntLogger = Logger.create (intSeqToStr, intSeqToStr)
    val mapIntDoubleChecker = Checker.fromRefsol (StuAutoPartSeq.mapIntDouble,
                                                  RefAutoPartSeq.mapIntDouble,
                                                  intSeqEqual)
    fun testMapIntDouble () = Tester.testGroup mapIntDoubleChecker
                                               mapIntLogger
                                               Tests.testsMapIntDouble

    (* enumInt *)
    val enumIntLogger = Logger.create (intSeqToStr, intIntSeqToStr)
    val enumIntChecker = Checker.fromRefsol (StuAutoPartSeq.enumInt,
                                             RefAutoPartSeq.enumInt,
                                             intIntSeqEqual)
    fun testEnumInt () = Tester.testGroup enumIntChecker
                                          enumIntLogger
                                          Tests.testsEnumInt

    (* mapIdxIntMult *)
    val mapIdxIntMultLogger = Logger.create (intSeqToStr, intSeqToStr)
    val mapIdxIntMultChecker = Checker.fromRefsol (StuAutoPartSeq.mapIdxIntMult,
                                                   RefAutoPartSeq.mapIdxIntMult,
                                                   intSeqEqual)
    fun testMapIdxIntMult () = Tester.testGroup mapIdxIntMultChecker
                                                mapIdxIntMultLogger
                                                Tests.testsMapIdxIntMult

    (* appendStr *)
    val appendStrLogger = Logger.create (strSeqStrSeqToStr, strSeqToStr)
    val appendStrChecker = Checker.fromRefsol (StuAutoPartSeq.appendStr,
                                               RefAutoPartSeq.appendStr,
                                               strSeqEqual)
    fun testAppendStr () = Tester.testGroup appendStrChecker
                                            appendStrLogger
                                            Tests.testsAppendStr

    (* iterhIntJoin *)
    val iterhIntJoinLogger = Logger.create (intSeqToStr, strSeqStrToStr)
    val iterhIntJoinChecker = Checker.fromRefsol (StuAutoPartSeq.iterhIntJoin,
                                                  RefAutoPartSeq.iterhIntJoin,
                                                  strSeqStrEqual)
    fun testIterhIntJoin () = Tester.testGroup iterhIntJoinChecker
                                               iterhIntJoinLogger
                                               Tests.testsIterhIntJoin

    (* iterIntMult *)
    val iterIntMultLogger = Logger.create (intSeqToStr, Int.toString)
    val iterIntMultChecker = Checker.fromRefsol (StuAutoPartSeq.iterIntMult,
                                                 RefAutoPartSeq.iterIntMult,
                                                 (op =))
    fun testIterIntMult () = Tester.testGroup iterIntMultChecker
                                              iterIntMultLogger
                                              Tests.testsIterIntMult

    (* toListInt *)
    val toListIntLogger = Logger.create (intSeqToStr, intListToStr)
    val toListIntChecker = Checker.fromRefsol (StuAutoPartSeq.toListInt,
                                               RefAutoPartSeq.toListInt,
                                               (op =))
    fun testToListInt () = Tester.testGroup toListIntChecker
                                            toListIntLogger
                                            Tests.testsToListInt

    (* toStringInt *)
    val toStringIntLogger = Logger.create (intSeqToStr, strToStr)
    val toStringIntChecker = Checker.fromRefsol (StuAutoPartSeq.toStringInt,
                                                 RefAutoPartSeq.toStringInt,
                                                 (op =))
    fun testToStringInt () = Tester.testGroup toStringIntChecker
                                              toStringIntLogger
                                              Tests.testsToStringInt
    
    (* toStringBool *)
    val toStringBoolLogger = Logger.create (boolSeqToStr, strToStr)
    val toStringBoolChecker = Checker.fromRefsol (StuAutoPartSeq.toStringBool,
                                                  RefAutoPartSeq.toStringBool,
                                                  (op =))
    fun testToStringBool () = Tester.testGroup toStringBoolChecker
                                               toStringBoolLogger
                                               Tests.testsToStringBool
  in
    fun testPartSeq () =
        let
          val _ = testMapIntDouble ()
          val _ = testEnumInt ()
          val _ = testMapIdxIntMult ()
          val _ = testAppendStr ()
          val _ = testIterhIntJoin ()
          val _ = testIterIntMult ()
          val _ = testToListInt ()
          val _ = testToStringInt ()
          val _ = testToStringBool ()
        in ()
        end
  end
end

structure SeqFunTester =
struct
  open StudentTestSuite
  open TesterHelper

  functor MkAutoSeqFun (structure SeqFun : SEQFUN) =
  struct
    val allHarmonics = SeqFun.allHarmonics
    fun groupedHarmonics (n, k) = SeqFun.groupedHarmonics n k
  end

  structure RefSeqFun = RefMkSeqFun (structure Seq = RefPartSeq)
  structure StuSeqFun = MkSeqFun (structure Seq = RefPartSeq)

  structure RefAutoSeqFun = MkAutoSeqFun (structure SeqFun = RefSeqFun)
  structure StuAutoSeqFun = MkAutoSeqFun (structure SeqFun = StuSeqFun)
  
  local
    (* allHarmonics *)
    val allHarmonicsLogger = Logger.create (Int.toString, realSeqToStr)
    val allHarmonicsChecker = Checker.fromRefsol (StuAutoSeqFun.allHarmonics,
                                                  RefAutoSeqFun.allHarmonics,
                                                  realSeqEqual)
    fun testAllHarmonics () = Tester.testGroup allHarmonicsChecker
                                               allHarmonicsLogger
                                               Tests.testsAllHarmonics

    (* groupedHarmonics *)
    val groupedHarmonicsLogger = Logger.create (intIntToStr, intRealSeqSeqToStr)
    val groupedHarmonicsChecker = Checker.fromRefsol (StuAutoSeqFun.groupedHarmonics,
                                                      RefAutoSeqFun.groupedHarmonics,
                                                      intRealSeqSeqEqual)
    fun testGroupedHarmonics () = Tester.testGroup groupedHarmonicsChecker
                                                   groupedHarmonicsLogger
                                                   Tests.testsGroupedHarmonics
  in
    fun testSeqFun () =
        let
          val _ = testAllHarmonics ()
          val _ = testGroupedHarmonics ()
        in ()
        end
  end
end

structure Tester =
struct
  fun testPartSeq () = PartSeqTester.testPartSeq ()
  fun testSeqFun () = SeqFunTester.testSeqFun ()
  fun printGroups n k =
      SeqFunTester.StuSeqFun.printGroups
        (SeqFunTester.StuSeqFun.groupedHarmonics n k)
end
