package download

import com.danielasfregola.twitter4s.TwitterRestClient
import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.entities.enums.ResultType
import com.typesafe.config.ConfigFactory
import java.io.PrintWriter
import scala.util.{Success, Failure}
import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import com.danielasfregola.twitter4s.entities.{HashTag, AccessToken, ConsumerToken}

import com.danielasfregola.twitter4s.entities.enums.Language

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.{ ask, pipe }

    class TweetProcessor extends Actor {
     
         
      def receive = {
        case result : Future[Seq[Tweet]]  => {

          result.onComplete({
    	    case Success(tweets) => {
      
	       println(s"Downloaded ${tweets.size} tweets")
	  
		 val hashtags: Seq[String ] = tweets.map { tweet =>
		       "surprised\t" + tweet.text
			
		    }

		//val hashtagTexts: Seq[String] = hashtags.flatten.map(_.text.toLowerCase)
		new PrintWriter("myJSONBig.txt") { write(hashtags mkString("\n")); close }
	//	toFileAsJson(filename, tweets)
		
		//here we can send tweets to the next actor or in our case we continue the loop
		context.parent ! "nextInput"
		
	    }
	    case Failure(exception) => {
	     
		println(exception)
	    }
	  })

	
	}
        
	case "done" =>
          println("done")
      }
     
    }



class TweetDownloader (consumerToken: ConsumerToken, accessToken: AccessToken) extends Actor {
  
  val client = TwitterRestClient(consumerToken, accessToken)

  def searchTweets(i: Int, query: String, max_id: Option[Long] = None): Future[Seq[Tweet]] = {
    def extractNextMaxId(params: Option[String]): Option[Long] = {
      //example: "?max_id=658200158442790911&q=%23scala&include_entities=1&result_type=mixed"
      params.getOrElse("").split("&").find(_.contains("max_id")).map(_.split("=")(1).toLong)
    }
    
    client.searchTweet(query, count = 100, result_type = ResultType.Mixed, max_id = max_id, language = Some(Language.English)).flatMap { ratedData =>
        val result = ratedData.data
        val nextMaxId = extractNextMaxId(result.search_metadata.next_results)
        val tweets = result.statuses
	
	//println(result.search_metadata.query)
	//println(result.search_metadata.count)
	println(nextMaxId)
	println(tweets.size)
	

	if (tweets.nonEmpty && i>0) searchTweets(i-1, query, nextMaxId).map(_ ++ tweets)
        else Future(tweets.sortBy(_.created_at))
      } recover { case _ => Seq.empty }
  }




  def receive = {
    case (num: Int, tag: String, t:ActorRef) => {
 	 println("processing tweets (download)")
	 val result = searchTweets(num, tag)//.map { tweets =>
	 //  sender ! tweets }
	 t ! result
	}

    case "hello" => println("buenos dias!")
    case _       => println("huh?")
  }
}





class Looper extends Actor {
     
  val consumerToken = ConsumerToken(key = "9DZO2bQPgmXO4r2eML5yVE7tb", secret = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW")
  val accessToken = AccessToken(key = "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0", secret = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x")

  //you can create as many downloaders as you need, using different credentials
  val TDownloader = context.actorOf(Props(new TweetDownloader(consumerToken, accessToken)), name = "DownloadActor")
  val TProcessor = context.actorOf(Props[TweetProcessor], name = "ProActor")
 
      def receive = {

	case "nextInput" => {
	   print("Query: ");
 	   val q = scala.io.StdIn.readLine()
	   


	  if(q!=""){
	   print("How many tweets to download: ");
 	   val num = (scala.io.StdIn.readLine().toInt/100).toInt;

           TDownloader  ! (num, q, TProcessor)
	  }else self ! "done"
	}        
	case "done" =>
          println("done")
	context.system.terminate()
      }
     
    }


object SearchAndSaveTweets extends App{

  val system = ActorSystem("DownloadSystem")
  val looperActor = system.actorOf(Props[Looper], name = "looperActor")
  
  looperActor ! "nextInput"

}
