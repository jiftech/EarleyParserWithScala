import scala.io.Source

/**
  * Created by jiftech on 16/11/01.
  */
object RuleLoader {
  import Constituents._
  /**
    * 句構造規則をファイルから読み込む
    * @param filename 読み込むファイル名
    * @return 規則のリスト
    */
  def loadRules(filename: String): Seq[Rule] = {
    val lines = Source.fromFile(filename).getLines().toSeq
    for((line, i) <- lines.zipWithIndex)
      yield {
        val lineNum = i + 1
        val split = line.split(" ").toList
        split match {
          case lhs :: "->" :: Nil => sys.error("句構造規則読み込みエラー: 右辺が記載されていない規則があります" + lineNumStr(lineNum))
          case lhs :: "->" :: rhss => constructRule(lhs, rhss, lineNum)
          case _ => sys.error("句構造規則読み込みエラー: 書式が正しくない規則があります" + lineNumStr(lineNum))
        }
      }
  }

  private def constructRule(strLhs: String, strRhss: Seq[String], lineNum: Int): Rule = {
    def getRhs(optionalCnstList: Seq[Option[Constituent]]) = getRhsBody(optionalCnstList, Some(Seq.empty[Constituent]))
    def getRhsBody(optionalCnstList: Seq[Option[Constituent]], rhs: Option[Seq[Constituent]]): Option[Seq[Constituent]] = {
      (optionalCnstList, rhs) match {
        case (Some(cnst) :: Nil, Some(rhsSeq)) => Some(rhsSeq :+ cnst)
        case (Some(cnst) :: rest, Some(rhsSeq)) => getRhsBody(rest, Some(rhsSeq :+ cnst))
        case (None :: _, _) => None
      }
    }
    val lhs = getPhraseByName(strLhs)
    val rhs = getRhs(strRhss.map(strRhs => getCnstByName(strRhs)))

    (lhs, rhs) match {
      case (Some(lhs), Some(rhs)) => Rule(lhs, rhs)
      case (None, None) => sys.error("句構造規則読み込みエラー: 左辺、右辺の両方に誤りがある規則があります" + lineNumStr(lineNum))
      case (None, _) => sys.error("句構造規則読み込みエラー: 左辺に誤りがある規則があります" + lineNumStr(lineNum))
      case (_, None) => sys.error("句構造規則読み込みエラー: 右辺に誤りがある規則があります" + lineNumStr(lineNum))
    }
  }

  /**
    * 単語と品詞の対応をファイルから読み込む
    * @param filename 読み込むファイル名
    * @return 単語と品詞の対応を表すMap
    */
  def loadPosMap(filename: String): Map[String, Pos] = {
    val lines = Source.fromFile(filename).getLines().toSeq
    val pairSeq = for((line, i) <- lines.zipWithIndex)
      yield {
        val lineNum = i + 1
        val split = line.split(" ").toList
        split match {
          case pos :: "->" :: Nil => sys.error("品詞一覧読み込みエラー: 右辺が記載されていません" + lineNumStr(lineNum))
          case pos :: "->" :: word if word.length == 1 => constructPosMap(pos, word.head, lineNum)
          case _ => sys.error("品詞一覧読み込みエラー: 書式が正しくありません" + lineNumStr(lineNum))
        }
      }
    pairSeq.toMap
  }

  private def constructPosMap(strPos: String, word: String, lineNum: Int): (String, Pos) = {
    val pos = getPosByName(strPos)

    pos match {
      case Some(pos) => word -> pos
      case None => sys.error("品詞一覧読み込みエラー: 左辺の品詞名に誤りがあります" + lineNumStr(lineNum))
    }
  }

  private def lineNumStr(lineNum: Int) = "(" + lineNum + "行目)"
}
