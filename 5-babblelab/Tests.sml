functor MkTests
  (structure Seq : SEQUENCE
   structure StringTok : TOKEN) =
struct
  val % = Seq.fromList

  (* histTests : (int seq) list
   * 
   * Tester will call (histogram Int.compare S) for each S in histTests *)
  val histTests = [
    %[],
    %[1,2,3],
    %[1,6,3,1,2,2,9,10,0,1]
  ]

  (* chooseTests : (string hist * real) list
   *
   * Tester will call (choose hist r) for each (hist, r) in chooseTests *)
  val chooseTests = [
    (%[("this", 2), ("is", 1), ("a", 5), ("test", 2)], 0.0),
    (%[("this", 2), ("is", 1), ("a", 5), ("test", 2)], 0.19),
    (%[("this", 2), ("is", 1), ("a", 5), ("test", 2)], 0.2),
    (%[("this", 2), ("is", 1), ("a", 5), ("test", 2)], 0.9)
  ]

  val (read : string -> string) = TextIO.inputAll o TextIO.openIn

  (* kGramTests : string * int * (token seq) list 
   *
   * for each (corpus, k, tests) in kGramTests and each gram in tests, Tester
   * will call (lookupHist (makeData corpus k) gram) *)
  val kGramTests =
    (read "corpus.txt", 3, List.map StringTok.tokenize [
      "says about",
      "start walking",
      "the motion",
      "the direction",
      "this Eddington"
    ])

  (* stringBabbleTests : string * int * int * (int * int) * int 
   *
   * for each (corpus, k, num, (low, high), X) in stringBabbleTests,
   * Tester will call (babble corpus k num (low, high) (Rand.fromInt X)) and
   * print the results.
   * (See support/BABBLE_PACKAGE.sig) *)
  val stringBabbleTests = [
    (read "data/kennedy.txt", 3, 5, (15, 30), 42)
  ]

  (* charBabbleTests : string * int * int * (int * int) * int 
   *
   * for each (corpus, k, num, (low, high), X) in charBabbleTests,
   * Tester will call (babble corpus k num (low, high) (Rand.fromInt X)) and
   * print the results.
   * (See support/BABBLE_PACKAGE.sig) *)
  val charBabbleTests = [
    (read "data/kennedy.txt", 4, 5, (100, 150), 42)
  ]

end