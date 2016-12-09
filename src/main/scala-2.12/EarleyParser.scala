import Constituents._

import scala.annotation.tailrec

// TODO: テストを書く
/**
  * Created by jiftech on 16/10/30.
  */
case class Arc (lhs: Constituent, completed: Seq[Constituent], incomplete: Seq[Constituent], start: Int, end: Int) {
  def isCompleted = this.incomplete.isEmpty
  def isIncomplete = !isCompleted
  def nextConstituent = incomplete.head
  override def toString = "<" + lhs + " --> " + completed.mkString(" ") + " . " + incomplete.mkString(" ") + " [" + start + ", " + end + "]>"
}

object EarleyParser {
  import Constituents._
  type Chart = Set[Arc]
  /**
    * 文を解析し、目標とする構成素に一致するかを調べる
    *
    * @param statement 解析する文
    * @param rules 句構造規則のリスト
    * @param posMap 単語と品詞の対応を表すMap
    * @param goal 目標とする構成素
    * @return ｢目標とする構成素に一致するかどうか｣と、解析で生成されたChartの組
    */
  def parse(statement: String,
            rules: Seq[Rule] = Rule.defaultRules,
            posMap: Map[String, Pos] = Words.defaultPosMap,
            goal: Constituent = S): (Boolean, Chart) = {

    @tailrec
    def parseRoutine(wordList: List[String], curPos: Int, finalPos: Int, chart: Chart): Chart = {
      @tailrec
      def predict(curPos: Int, curChart: Chart): Chart = {
        val incompleteArcs = curChart.filter(_.isIncomplete)
        val newArcs =
          for {
            arc <- incompleteArcs
            rule <- rules if arc.nextConstituent == rule.lhs
          } yield Arc(rule.lhs, Nil, rule.rhs, curPos, curPos)

        val nextChart = curChart ++ newArcs

        // 今回のループでChartに更新がなければ終了
        if(curChart == nextChart) nextChart
        else predict(curPos, nextChart)
      }

      def scan(wordList: List[String], curPos: Int, nextPos: Int, chart: Chart): (List[String], Chart) = {
        val next::rest = wordList

        posMap.get(next) match {
          case Some(posOfNext) =>
            if(chart.exists(arc => !arc.isCompleted && arc.nextConstituent == posOfNext))
              (rest, chart + Arc(posOfNext, List(Word(next)), Nil, curPos, nextPos))
            else (rest, chart)

          case None => sys.error("単語リストにない単語が文中にあるため、解析に失敗しました")
        }
      }

      @tailrec
      def complete(curPos: Int, curChart: Chart): Chart = {
        val (completeArcs, incompleteArcs) = curChart.partition(_.isCompleted)
        val newArcs =
          for {
            compArc <- completeArcs if compArc.end == curPos
            incompArc <- incompleteArcs if (incompArc.nextConstituent == compArc.lhs && incompArc.end == compArc.start)
          } yield Arc(incompArc.lhs, incompArc.completed :+ incompArc.nextConstituent, incompArc.incomplete.tail, incompArc.start, curPos)

        val nextChart = curChart ++ newArcs

        // 今回のループでChartに更新がなければ終了
        if(curChart == nextChart) nextChart
        else complete(curPos, nextChart)
      }

      val predictedChart = predict(curPos, chart)
      val nextPos = curPos + 1
      val (restWords, scannedChart) = scan(wordList, curPos, nextPos, predictedChart)
      val completedChart = complete(nextPos, scannedChart)

      if(nextPos == finalPos) completedChart
      else parseRoutine(restWords, nextPos, finalPos, completedChart)
    }

    val wordList = statement.split(" ").toList
    val initChart = Set(Arc(Start, Nil, List(goal), 0, 0))
    val finalChart = parseRoutine(wordList, 0, wordList.length, initChart)
    (finalChart.contains(Arc(Start, List(goal), Nil, 0, wordList.length)), finalChart)
  }
}

object Main extends App {
  val statement = "the meal of the day"
  val rules = RuleLoader.loadRules("src/main/resources/rules.txt")
  val posMap = RuleLoader.loadPosMap("src/main/resources/posmap.txt")

  println("Statement to be parsed: \"" + statement + "\"")
  println("Rules:")
  rules.foreach(r => println("\t" + r))
  println("Pos Map:")
  posMap.foreach(w => println("\t" + w))

  val (result, finalChart) = EarleyParser.parse(statement, rules, posMap, NP)

  if(result) println("parse succeeded:")
  else println("parse failed:")
  finalChart foreach println
}
