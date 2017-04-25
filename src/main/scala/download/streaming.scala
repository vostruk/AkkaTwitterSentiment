package download

import com.danielasfregola.twitter4s.TwitterStreamingClient
import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.entities.streaming.common.WarningMessage
import com.danielasfregola.twitter4s.entities.streaming.StreamingMessage
import com.danielasfregola.twitter4s.entities.{HashTag, AccessToken, ConsumerToken}
import com.danielasfregola.twitter4s.entities.enums.Language
import classify.DocumentCategoryMessage

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props


object streaming extends App{

  val consumerToken = ConsumerToken(key = "9DZO2bQPgmXO4r2eML5yVE7tb", secret = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW")
  val accessToken = AccessToken(key = "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0", secret = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x")

   val system = ActorSystem("DownloadSystem")
  val rr = system.actorOf(Props[rec], name = "rr")
  val streamActor = system.actorOf(Props(new OnlineTweetStreamer(consumerToken, accessToken, "😀", rr )), name = "streamActor")

  streamActor ! ("start", 5)

}

class rec extends Actor {

    var l = 0;

    def receive = {
        case DocumentCategoryMessage(tw, em)  => {
		l = l+1;
		println(l);
	    }
         }
	
}


class OnlineTweetStreamer (consumerToken: ConsumerToken, accessToken: AccessToken, emotion:String, receiver:ActorRef) extends Actor {
  
    val client = TwitterStreamingClient(consumerToken, accessToken)
    var countReceived = 0
    
    def sendTweetText: PartialFunction[StreamingMessage, Unit] = {
      case tweet:Tweet => { if(countReceived>0)  receiver ! DocumentCategoryMessage(tweet.text, emotion); else context.stop(self); countReceived = countReceived -1;}
      case warning: WarningMessage  => println("koniec bo blad")
   }

    def receive = {
        case ("start", num:Int)  => {
		countReceived = num
		client.filterStatuses(tracks=List(emotion), languages = List(Language.English))(sendTweetText)
         }
	
	case "done" =>
          println("done")
      }

}
