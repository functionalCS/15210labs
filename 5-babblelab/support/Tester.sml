structure Tester =
struct
  open StudentTestSuite

  structure Seq = ArraySequence
  structure Rand = MkMersenneTwister(structure Seq = ArraySequence)

  structure CharTok = MkCharacterToken(structure Seq = ArraySequence)
  structure StringTok = MkStringToken(structure Seq = ArraySequence)

  (* student solution *)
  structure StuCharBabble =
    MkBabblePackage(structure Tok = CharTok
                    structure Seq = Seq
                    structure Rand = Rand)

  structure StuStringBabble =
    MkBabblePackage(structure Tok = StringTok
                    structure Seq = Seq
                    structure Rand = Rand)

  (* reference solution *)
  structure RefCharBabble =
    MkRefBabblePackage(structure Tok = CharTok
                    structure Seq = Seq
                    structure Rand = Rand)

  structure RefStringBabble =
    MkRefBabblePackage(structure Tok = StringTok
                       structure Seq = Seq
                       structure Rand = Rand)

  (* testing *)
  structure Tests =
    MkTests(structure Seq = Seq
            structure StringTok = StringTok)

  fun uncurry2 f (x, y) = f x y

  fun tupToString tos1 tos2 (a, b) =
    "(" ^ (tos1 a) ^ "," ^ (tos2 b) ^ ")"

  fun optToString tos x =
    case x of
      NONE => "NONE"
    | SOME y => "SOME " ^ (tos y)

  fun cmpEq cmp (x, y) =
    cmp (x, y) = EQUAL

  fun testHist () =
    let
      fun elemEqual ((k1, c1), (k2, c2)) =
        (k1 = k2) andalso (c1 = c2)
      val checker =
        Checker.fromRefsol (StuCharBabble.Util.histogram Int.compare,
                            RefCharBabble.Util.histogram Int.compare,
                            Seq.equal elemEqual)
      val intsToString = tupToString Int.toString Int.toString
      val logger = Logger.create (Seq.toString Int.toString, Seq.toString intsToString)
    in
      Tester.testGroup checker logger Tests.histTests
    end

  fun testChoose () =
    let
      val checker =
        Checker.fromRefsol (uncurry2 StuCharBabble.Util.choose,
                            uncurry2 RefCharBabble.Util.choose,
                            cmpEq String.compare)
      fun strToString x = x
      val elemToString = tupToString strToString Int.toString
      val histToString = Seq.toString elemToString
      val logger = Logger.create (tupToString histToString Real.toString, strToString)
    in
      Tester.testGroup checker logger Tests.chooseTests
    end

  fun testKGrams () =
    let
      val (corpus, k, tests) = Tests.kGramTests
      val stuData = StuStringBabble.Data.makeData corpus k
      val refData = RefStringBabble.Data.makeData corpus k

      fun tokHistElemCmp ((t1, c1), (t2, c2)) =
        case StringTok.compare (t1, t2) of
          EQUAL => Int.compare (c1, c2)
        | other => other

      fun tokHistEq (h1, h2) =
        case (h1, h2) of
          (SOME h1', SOME h2') => Seq.collate tokHistElemCmp (h1', h2') = EQUAL
        | (NONE, NONE) => true
        | _ => false

      val checker =
        Checker.fromRefsol (StuStringBabble.Data.lookupHist stuData,
                            RefStringBabble.Data.lookupHist refData,
                            tokHistEq)

      val histToString = Seq.toString (tupToString StringTok.toString Int.toString)

      val logger = Logger.create (Seq.toString StringTok.toString, optToString histToString)
    in
      Tester.testGroup checker logger tests
    end

  fun babbleWith babbler tests =
    case tests of
      []                => ()
    | test :: moreTests =>
        let
          val (corpus, k, num, (low, high), randInt) = test
          val seed = Rand.fromInt randInt
          val _ = print (babbler corpus k num (low, high) seed ^ "\n")
        in
          babbleWith babbler moreTests
        end

  fun testCharBabble () =
    babbleWith StuCharBabble.babble Tests.charBabbleTests

  fun testStringBabble () =
    babbleWith StuStringBabble.babble Tests.stringBabbleTests
end
