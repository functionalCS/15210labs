functor MkBabble
  (structure Rand : RANDOM210
   structure Data : KGRAM_DATA
   structure Util : SEQ_UTIL
   sharing Data.Seq = Util.Seq
   sharing Data.Seq = Rand.Seq)
  : BABBLE =
struct
  structure Seq = Data.Seq
  structure Rand = Rand
  structure Data = Data
  open Seq

  type sentence = Data.token Seq.seq
  
  fun makeSentenceHelper data len seed kgram =
    let
      val (r, nextSeed) = Rand.randomReal seed (SOME (0.0,1.0))
      val hist = Data.lookupHist data kgram
    in
      case (hist, Int.compare(len,0)) of
          (SOME a, GREATER) =>
            let
              val next = Util.choose a r
            in
              next::(makeSentenceHelper data (len-1) nextSeed
                                        (append(drop(kgram,1),
                                                singleton(next))))
            end                  
        | _ => []
    end
     
  (* Implement the following functions *)
  fun makeSentence data len seed =
    let
      val k = Data.getK data
      val (r, nextSeed) = Rand.randomReal seed (SOME (0.0,1.0))
      val start = Data.chooseStarter data r
      val rest = makeSentenceHelper data (len-(k-1)) nextSeed start
      val sentence = fromList(toList(start)@rest)
    in
      sentence
    end

  fun makeParagraph data numSentences (low, high) seed =
    let
      val (lens, newSeeds) = Rand.randomIntSeq seed (SOME (low,high))
                                          numSentences
      val (seedInts, _) = Rand.randomIntSeq newSeeds NONE 
                                       numSentences
      val seeds = map Rand.fromInt seedInts
      val paragraph = map2 (fn (len, seed) => makeSentence data len seed)
                           lens seeds
    in
      paragraph
    end
      
end
