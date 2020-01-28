import io.circe._, io.circe.parser._, io.circe.generic.auto._
import cats.implicits._
import scala.io.Source

object PageViewsMonthly {

  def main(args: Array[String]): Unit = {

    def getMonthlyViews(project: String, year: Int, month: Int): Map[String, Int] = {
      def url: String = {
        val monthStr = (if (month < 10) "0" else "") + month
        s"https://wikimedia.org/api/rest_v1/metrics/pageviews/top/$project/all-access/$year/$monthStr/all-days"
      }

      val text = Source.fromURL(url).mkString
      val doc = parse(text).getOrElse(Json.Null)

      (doc \\ "articles")
        .flatMap(_.as[List[PageViews]].getOrElse(Nil))
        .map(PageViews.unapply(_).get)
        .toMap
    }

    def sumMapValues(maps: Seq[Map[String, Int]]) = maps.foldLeft(Map.empty[String, Int])(_ |+| _)

    val monthly = for (year <- 2015 to 2019;
                       month <- (1 to 12).filter( year > 2015 || _ > 6 ))
      yield
       getMonthlyViews("uk.wikipedia.org", year, month)

    val yearly = sumMapValues(monthly)

    val byBook = sumMapValues(
      yearly.toSeq.map { case (name, views) => Map(name.split("/").head -> views) }
    )

    val wikiTable = byBook.toSeq
      .filterNot(_._1.startsWith("Спеціальна:"))
      .sortBy(-_._2)
      .zipWithIndex.map {
      case ((name, views), i) => s"| ${i + 1} || [[$name]] || $views"
    }.mkString("{| class = \"wikitable\"\n|-\n", "\n|-\n", "\n|}")

    println(wikiTable)
  }
}
