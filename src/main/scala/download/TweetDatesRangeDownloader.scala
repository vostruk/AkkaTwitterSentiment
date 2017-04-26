package download

import java.io.PrintWriter
import java.net.URLEncoder

import akka.pattern.ask

import scala.concurrent.duration._
import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import classify.{CategoryMessage, ClassifyDocumentMessage, DocumentCategoryMessage}
import oauth.signpost.commonshttp.CommonsHttpOAuthConsumer
import org.apache.commons.io.IOUtils
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DefaultHttpClient
import org.json4s.DefaultFormats
import play.api.libs.json.Json

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.Map
import scala.concurrent.Await
import scala.util.parsing.json.JSON

/**
  * Created by ostruk on 4/25/17.
  */

class TweetDatesRangeDownloader(ConsumerKey: String, ConsumerSecret: String, AccessToken: String, AccessSecret: String, requestParser: ActorRef) extends Actor {

  val consumer = new CommonsHttpOAuthConsumer(ConsumerKey, ConsumerSecret);

  def search(num: Int, query: String) {

    if (num < 1) {
      return
    }

    consumer.setTokenWithSecret(AccessToken, AccessSecret);
    val request = new HttpGet("https://api.twitter.com/1.1/search/tweets.json" + query);
    consumer.sign(request);

    val client = new DefaultHttpClient();
    val response = client.execute(request);

    println(response.getStatusLine().getStatusCode());

    val jsonRes = IOUtils.toString(response.getEntity().getContent())
    new PrintWriter("JsonResult_" + num + ".txt") {
      write(jsonRes); close
    }
    //myparse(jsonRes, "onlyTweets_"+num+".txt");

    requestParser ! jsonRes

    implicit val formats = DefaultFormats
    val parsedJson = Json.parse(jsonRes.toString)

    val value1 = parsedJson \ "search_metadata" \ "next_results"

    // println (value1.as[String])//.map(_.as[String]).lift(1))

    search(num - 1, value1.as[String])
  }

  def encodeFirstQuery(keywords: String, dateFrom: String, dateTo: String): String = {
    var sinceStr = ""
    var untilStr = ""

    if (dateFrom != "") sinceStr = " since:" + dateFrom
    if (dateTo != "") untilStr = " until:" + dateTo

    val str = keywords + sinceStr + untilStr;
    val s = URLEncoder.encode(str, "UTF-8");
    val query = "?q=" + s + "&count=100&lang=en";
    println(s)

    query
  }

  def receive = {
    case (q: String, f: String, t: String) => {
      search(1, encodeFirstQuery(q, f, t))
    }
  }
}

//=====================================

class RequestParserActor(NbM: ActorRef) extends Actor {

  var DateToStat: mutable.Map[(String, String), Int] = mutable.Map()

  class CC[T] {
    def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T])
  }

  object M extends CC[immutable.Map[String, Any]]
  object L extends CC[List[Any]]
  object S extends CC[String]
  object D extends CC[Double]
  object B extends CC[Boolean]

  def receive = {
    case jsonString: String => {
      val bString = jsonString.replaceAll("[\\t\\n\\r]+", " ");
      val result = for {
        Some(M(map)) <- List(JSON.parseFull(bString))
        L(statuses) = map("statuses")
        M(tweet) <- statuses
        S(text) = tweet("text")
        S(created) = tweet("created_at")
        D(id) = tweet("id")
      } yield {
        (text, created, id)
      }
      implicit val timeout = Timeout(40 seconds)
      //println(result.size)

      //CAN PRINT parsed Tweets to file
      val st = result.map { tuple => tuple.productIterator.mkString("\t") }
      new PrintWriter("textTweets.txt") {
        write(st mkString ("\n")); close
      }

      //!!!!!!!or can send for classification
      val TweetsCategoriesDates = result.map { tuple =>   (tuple._2, tuple._1, Await.result(NbM ? ClassifyDocumentMessage(tuple._1), timeout.duration).asInstanceOf[CategoryMessage])}

      println("received categorized elements")
      println(TweetsCategoriesDates.size)

    }

    case tcd: List[(String, String, String)] => {
      for ((_, c, d) <- tcd) {DateToStat((c, d)) = DateToStat((c, d)) + 1}
    }

    case "ShowStatistics" => {sender() ! DateToStat; println(DateToStat.values)   }
  }

}

