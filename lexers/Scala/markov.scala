import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

val MARKOV_MAP:mutable.Map[Seq[String], mutable.Map[String, Int]] = new mutable.HashMap()
val CHAIN_SIZE = 2

def adjustProbabilities(sentence:String):Unit = {
  val segments = sentence.split(" ").+:("").:+("").sliding(CHAIN_SIZE + 1).toList
  for(segment <- segments) {
    val key = segment.take(CHAIN_SIZE)
    val probs = MARKOV_MAP.getOrElse(key, scala.collection.mutable.Map())
    probs(segment.last) = probs.getOrElse(segment.last, 0) + 1
    MARKOV_MAP(key) = probs
  }
}

def normalize(line: String): String = {
  line.stripLineEnd
    .toLowerCase
    .filterNot("\\.-,\";:&" contains _)
}

val filePath = "/Users/phillip/Documents/Programming/markov-tweets/src/main/resources/shakespeare_corpus.txt"

Source.fromFile(filePath).getLines()
  .map(normalize(_))
  .map(s => s.trim)
  .foreach(s => adjustProbabilities(s))

val startWords = MARKOV_MAP.keys.filter(_.head == "").toList

val r = new Random()

def nextWord(seed:Seq[String]):String = {
  val possible = MARKOV_MAP.getOrElse(seed, List())
  r.shuffle(possible.flatMap(pair => List.fill(pair._2)(pair._1))).head
}

def nextSentence():String = {
  val seed = startWords(r.nextInt(startWords.size))
  val sentence:ArrayBuffer[String] = ArrayBuffer()
  sentence.appendAll(seed)
  while(sentence.last != "") {
    sentence.append(nextWord(sentence.view(sentence.size - CHAIN_SIZE, sentence.size)))
  }
  sentence.view(1, sentence.size - 1).mkString(" ").capitalize + ","
}

(0 until 14).map(_ => nextSentence()).mkString("\n")
