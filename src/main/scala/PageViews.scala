import java.net.URLEncoder
import java.util.concurrent.atomic.AtomicInteger

import io.circe._
import io.circe.optics.JsonPath._
import org.http4s.circe._
import org.http4s.client.blaze._

import scalaz.concurrent.Task
import org.log4s.getLogger

object PageViews {
  val project = "uk.wikibooks.org"
  val year = 2016

  val http = PooledHttp1Client()
  val logger = getLogger

  val (started, fetched) = (new AtomicInteger(0), new AtomicInteger(0))

  def logged[T](msg: T): T = {
    logger.info(s"Stat started: ${started.get}, fetched: ${fetched.get}, msg: " + msg)
    msg
  }

  def fetch[T](url: String)(f: Json => T): Task[T] = {
    started.incrementAndGet()
    http.expect[Json](logged(url)).map(f)
  }

  def encode(s: String) = URLEncoder.encode(s.replace(" ", "_"), "UTF-8")

  case class PagesWithContinue(pages: List[String], continue: Option[String])

  def allArticles: Task[Vector[String]] = {

    def fetchPages(continue: Option[String]): Task[PagesWithContinue] = {

      def url(continue: Option[String]) =
        "https://" + project + "/w/api.php?action=query&format=json" + "&list=allpages" + "&aplimit=max" +
          continue.fold("")("&apcontinue=" + encode(_))

      def parsePages(json: Json): List[String] =
        root
          .query
          .allpages.each
          .title
          .string.getAll(json)

      def parseContinue(json: Json): Option[String] =
        root
          .continue
          .apcontinue
          .string.getOption(json)

      fetch(url(continue)) { json =>
        PagesWithContinue(
          parsePages(json),
          parseContinue(json)
        )
      }
    }

    import scalaz.stream.Process
    import scalaz.stream.Process._
    def pageStream(continue: Option[String] = None): Process[Task, String] = {

      await(fetchPages(continue)) { pwc =>
        emitAll(pwc.pages) ++ pwc.continue.map(_ => pageStream(pwc.continue)).getOrElse(empty)
      }
    }

    pageStream().runLog
  }

  def pageViews(article: String): Task[(String, Int)] = {
    val statBaseUrl = "https://wikimedia.org/api/rest_v1/metrics/pageviews/"

    def rangeUrl(start: String, end: String) =
      statBaseUrl + s"per-article/$project/all-access/all-agents/${encode(article)}/daily/$start/$end"

    def url = rangeUrl(s"${year}0101", s"${year}1231")

    def views(json: Json): Int =
      root.items.each.views.int.getAll(json).sum

    fetch(url) { json =>
      fetched.incrementAndGet()
      logged(
        article -> views(json)
      )
    }.handle { case ex =>
      logger.error(ex)(article)
      article -> 0
    }
  }

  def main(args: Array[String]): Unit = {

    def getStat(pages: Seq[String]) = {

      val tasks = pages.map(pageViews)

      Task.gatherUnordered(tasks)
        .map(_.sortBy(-_._2))
        .unsafePerformSync
    }

    val result = allArticles.map(getStat).unsafePerformSync

    println(s"Fetched ${result.size} pairs")
    result.foreach(println)
  }
}