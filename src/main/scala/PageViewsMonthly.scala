import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._

import scala.io.Source

object PageViewsMonthly {

  def main(args: Array[String]): Unit = {
    println(monthlyViews)
  }

  case class PV(article: String, views: Int)

  def monthlyViews = {

    def getMonthlyViews(project: String, year: Int, month: Int): Map[String, Int] = {
      def url: String = {
        val monthStr = (if (month < 10) "0" else "") + month
        s"https://wikimedia.org/api/rest_v1/metrics/pageviews/top/$project/all-access/$year/$monthStr/all-days"
      }

      val text = Source.fromURL(url).mkString
      val doc = parse(text).getOrElse(Json.Null)

      (doc \\ "articles")
        .flatMap(_.as[List[PV]].getOrElse(Nil))
        .map(PV.unapply(_).get)
        .toMap
    }

    def sumMapValues(maps: Seq[Map[String, Int]]) = maps.foldLeft(Map.empty[String, Int])(_ |+| _)

    val monthly = (1 to 12).map { month => getMonthlyViews("uk.wikibooks.org", 2016, month) }

    val yearly = sumMapValues(monthly)

    val byBook = sumMapValues(
      yearly.toSeq.map { case (name, views) => Map(name.split("/").head -> views) }
    )

    byBook.toSeq.sortBy(-_._2)
  }
}

