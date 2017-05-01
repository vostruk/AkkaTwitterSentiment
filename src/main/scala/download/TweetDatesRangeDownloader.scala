package download

import java.io.{File, FileOutputStream, PrintWriter}
import java.net.URLEncoder
import java.text.SimpleDateFormat

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

class TweetDatesRangeDownloader(ConsumerKey: String, ConsumerSecret: String, AccessToken: String, AccessSecret: String, naiveBayesActor: ActorRef) extends Actor {

  val consumer = new CommonsHttpOAuthConsumer(ConsumerKey, ConsumerSecret);
  val DateToStat = scala.collection.mutable.Map[String, Map[String, Int]]()

  def parseJsonString( jsonString: String) : List[(String, String, String)] = {
    class CC[T] {
      def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T])
    }

    object M extends CC[immutable.Map[String, Any]]
    object L extends CC[List[Any]]
    object S extends CC[String]
    object D extends CC[Double]
    object B extends CC[Boolean]
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
    implicit val timeout = Timeout(400 seconds)
    //println(result.size)

    //CAN PRINT parsed Tweets to file
    val st = result.map { tuple => tuple.productIterator.mkString("\t") }
    new PrintWriter("textTweets.txt") {
      write(st mkString ("\n")); close
    }

    //!!!!!!!or can send for classification
    val DatesTweetsCategories = result.map { tuple =>   (tuple._2, tuple._1, (Await.result(naiveBayesActor ? ClassifyDocumentMessage(tuple._1), timeout.duration).asInstanceOf[CategoryMessage]).category.get.toString())}

    println("received categorized elements")
    println(DatesTweetsCategories.size)
    return DatesTweetsCategories

  }


  def search(num: Int, query: String) {

    if (num < 1) {
      return
    }

    consumer.setTokenWithSecret(AccessToken, AccessSecret);
    val request = new HttpGet("https://api.twitter.com/1.1/search/tweets.json" + query);
    consumer.sign(request);

    val client = new DefaultHttpClient();
    val response = client.execute(request);

    val jsonRes = IOUtils.toString(response.getEntity().getContent())
//    new PrintWriter("JsonResult_" + num + ".txt") {
//      write(jsonRes); close
//    }
    //myparse(jsonRes, "onlyTweets_"+num+".txt");

    val dtc = parseJsonString(jsonRes)

    val st = dtc.map { tuple => tuple.productIterator.mkString("\t") }
    new PrintWriter(new FileOutputStream(new File("categorized.txt"),true)) {
      append(st mkString ("\n")); close
    }

    val TWITTER = "EEE MMM dd HH:mm:ss ZZZZZ yyyy";
    val sf = new SimpleDateFormat(TWITTER);

    //for ((d, t, c) <- dtc)
    dtc.foreach(tuple => {
      val d = tuple._1
      val t = tuple._2
      val c = tuple._3

      val date = sf.parse(d.toString())
      val dayMonth:String = date.getYear().toString() +"-"+ date.getMonth().toString()+"-"+ date.getDay().toString()
      println(dayMonth)

      if(c != null) {

        if(!DateToStat.contains(c))
         {DateToStat(c) = Map[String, Int](); println("Set new emoji to map!!###") }
         //if sentiment didn't exist in map will be created now

        if(!DateToStat(c).contains(dayMonth))
          DateToStat(c)(dayMonth) = 1;
        else DateToStat(c)( dayMonth) = DateToStat(c)( dayMonth) + 1
      }
      else println("Ccategory is null wtf??")

    })

    sender ! DateToStat

   // implicit val formats = DefaultFormats
   // val parsedJson = Json.parse(jsonRes.toString)

   // val value1 = parsedJson \ "search_metadata" \ "next_results"

    // println (value1.as[String])//.map(_.as[String]).lift(1))

    //search(num - 1, value1.as[String])
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


