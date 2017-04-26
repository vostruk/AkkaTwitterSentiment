package download

import org.apache.http.client.HttpClient
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.client.methods.HttpGet
import oauth.signpost.commonshttp.CommonsHttpOAuthConsumer
import org.apache.commons.io.IOUtils
import com.typesafe.config.ConfigFactory
import java.io.PrintWriter
import scala.util.parsing.json._
import play.api.libs.json._
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import org.json4s._

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

import classify.CategoryMessage
import classify.DocumentCategoryMessage
import classify.ClassifyDocumentMessage

import classify.DocumentPreprocessor
import classify.CategoriesRepositoryActor
import classify.LaplaceSmoothingCategoryModel
import classify.NaiveBayesModelActor
//=============================================

class RequestParserActor(NbM:ActorRef) extends Actor {
	
	class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }
	object M extends CC[Map[String, Any]]
	object L extends CC[List[Any]]
	object S extends CC[String]
	object D extends CC[Double]
	object B extends CC[Boolean]

    def receive = {
		case jsonString:String => {
			val bString =     jsonString.replaceAll("[\\t\\n\\r]+"," ");
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
			implicit val timeout = Timeout(5 seconds)
			println(result.size)

			//CAN PRINT parsed Tweets to file
			 	val st = result.map { tuple =>   tuple.productIterator.mkString("\t")}
				new PrintWriter("textTweets.txt") { write(st mkString("\n")); close }

			//!!!!!!!or can send for classification
			//val TweetsCategoriesDates = result.map { tuple =>   (tuple._2, tuple._1, Await.result(NbM ? ClassifyDocumentMessage(tuple._1), timeout.duration).asInstanceOf[CategoryMessage])}

			//val result2 = Await.result(categories, timeout.duration).asInstanceOf[List[CategoryMessage]]
    		println("received categorized elements")
			//println(TweetsCategoriesDates.size)
		}
		
		case DocumentCategoryMessage(tw, em)  => {
	    }
   }
	
}

class SearchRangeActor (ConsumerKey:String, ConsumerSecret:String, AccessToken:String, AccessSecret:String, requestParser: ActorRef ) extends Actor {

	val consumer = new CommonsHttpOAuthConsumer(ConsumerKey,ConsumerSecret);

 def search(num:Int, query :String) {

     if(num < 1){ return;}
	  
	 consumer.setTokenWithSecret(AccessToken, AccessSecret);
	 val request = new HttpGet("https://api.twitter.com/1.1/search/tweets.json"+query );
     consumer.sign(request);

     val client = new DefaultHttpClient();
     val response = client.execute(request);
 
     println(response.getStatusLine().getStatusCode());
     
	val jsonRes = IOUtils.toString(response.getEntity().getContent())
    new PrintWriter("JsonResult_"+num+".txt") { write(jsonRes); close }
	//myparse(jsonRes, "onlyTweets_"+num+".txt");
	
	requestParser ! jsonRes

    implicit val formats = DefaultFormats
    val parsedJson = Json.parse(jsonRes.toString)
    
    val value1 = (parsedJson \ "search_metadata" \ "next_results")//.as[String]
  
   // println (value1.as[String])//.map(_.as[String]).lift(1))

	 search(num-1, value1.as[String])
  }

def encodeFirstQuery(keywords:String,dateFrom:String, dateTo:String ):String = {
    var sinceStr = ""
	var untilStr = ""
	
	if(dateFrom!="") sinceStr=" since:"+dateFrom
	if(dateTo!="") untilStr=" until:"+dateTo

	val str=  keywords + sinceStr  + untilStr;
	val s = URLEncoder.encode(str, "UTF-8");
	val query = "?q="+s+"&count=100&lang=en";
	println(s) 
	return query;
}

  def receive = {
		case (q:String, f:String, t:String) => {  search(1, encodeFirstQuery(q , f, t))
 		}
	}

} //searchActor end


object JustDownload extends App{
 	
	  val  ConsumerKey  =  "9DZO2bQPgmXO4r2eML5yVE7tb";
	  val  ConsumerSecret  = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW";
	  val AccessToken = "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0";
	  val AccessSecret = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x";
 



 
//=============================================

val actorEmojisyDefinition = "😀" :: "😯" :: "☹️" :: "😠" :: Nil

val system = ActorSystem("DownloadSystem")

 val dp = new DocumentPreprocessor(2)
  val cr = system.actorOf(Props(new CategoriesRepositoryActor(dp, () => new LaplaceSmoothingCategoryModel(0.5, dp))))
  val NbMActor = system.actorOf(Props(new NaiveBayesModelActor(dp, cr)), name = "dpa1")
 val requestParserActor = system.actorOf(Props(new RequestParserActor(NbMActor)), name = "requestParserActor")
 val searchDownloadActor = system.actorOf(Props(new SearchRangeActor(ConsumerKey, ConsumerSecret, AccessToken, AccessSecret, requestParserActor)), name = "DownloadActor")

 print("Query: ");
 //"#brexit"
 val q =  scala.io.StdIn.readLine()
 
 //"2017-03-31"
 print("From (ex. 2017-04-20): ");
 val f = scala.io.StdIn.readLine() //"2017-04-20"//

 //"2017-04-01"
 print("To (ex. 2017-04-24): ");
 val t = scala.io.StdIn.readLine() //"2017-04-24"//
 
 searchDownloadActor ! (q, f, t)
 

}
