/**
  * 文の構成素
  */
sealed abstract class Constituent(val name: String)

// 句構造
sealed abstract class Phrase(name: String) extends Constituent(name)

// 品詞
sealed abstract class Pos(name: String) extends Constituent(name)

object Constituents {
  // 解析の開始地点を示す特別なシンボル
  case object Start extends Constituent("Start")

  case object NP extends Phrase("NP")
  case object VP extends Phrase("VP")
  case object PP extends Phrase("PP")
  case object S extends Phrase("S")

  case object Noun extends Pos("Noun")
  case object Verb extends Pos("Verb")
  case object Prep extends Pos("Prep")
  case object Det extends Pos("Det")
  case object Adj extends Pos("Adj")

  private val phraseList = Seq(NP, VP, PP, S)
  private val posList = Seq(Noun, Verb, Prep, Det, Adj)

  def getPhraseByName(name: String) = phraseList.find(_.name == name)
  def getPosByName(name: String) = posList.find(_.name == name)
  def getCnstByName(name: String) = (phraseList ++ posList).find(_.name == name)

}

// 単語
final case class Word(word: String) extends Constituent(word) {
  override def toString = "\""+ word + "\""
}

// 句構造規則
case class Rule private(lhs: Phrase, rhs: Seq[Constituent])
object Rule{
  import Constituents._
  val defaultRules: Seq[Rule] = Seq(
    Rule(NP, Seq(Det, Noun)),
    Rule(NP, Seq(NP, PP)),
    Rule(PP, Seq(Prep, NP)),
    Rule(S, Seq(NP, VP)),
    Rule(S, Seq(NP, Verb)),
    Rule(VP, Seq(Verb, NP)),
    Rule(VP, Seq(Verb, NP, PP))
  )
}

// 単語と品詞の対応表
object Words{
  import Constituents._
  val defaultPosMap: Map[String, Pos] = Map(
    "the" -> Det,
    "waiter" -> Noun,
    "meal" -> Noun,
    "day" -> Noun,
    "of" -> Prep,
    "brought" -> Verb,
    "slept" -> Verb
  )
}