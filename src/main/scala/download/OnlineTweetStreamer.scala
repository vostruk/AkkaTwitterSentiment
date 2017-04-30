package download

import com.danielasfregola.twitter4s.TwitterStreamingClient
import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.entities.streaming.common.WarningMessage
import com.danielasfregola.twitter4s.entities.streaming.StreamingMessage
import com.danielasfregola.twitter4s.entities.{AccessToken, ConsumerToken}
import com.danielasfregola.twitter4s.entities.enums.Language
import classify.DocumentCategoryMessage
import akka.actor.ActorRef
import akka.actor.Actor

import collection.mutable.Map
import scala.collection.mutable
import scala.collection.immutable


class OnlineTweetStreamer(consumerToken: ConsumerToken, accessToken: AccessToken, receiver: ActorRef) extends Actor {

  val client = TwitterStreamingClient(consumerToken, accessToken)
  var countReceived = 0
  var emotions: List[String] = "😠" :: Nil
  var emoMap : immutable.Map[String, String] = null

  def filterEmoji(t : String): String = {
    var emoCheck : mutable.Map[String, Int] = mutable.Map(emotions map { s => (s, 0)} : _*)
    //Since our actor retrieves every emotion from the list, we need to classify them

    emotions.foreach(elem => if(t.contains(elem)) emoCheck(elem)=emoCheck(elem)+1)
    var rv = "Many"
    emoCheck.values.count(_ >0) match {
      case 1 => for((e, c) <- emoCheck) {if(c==1) rv = e }
      case 0 => rv = "None"
      case _ => rv = "Many"
    }
     rv
  }

  def sendTweetText: PartialFunction[StreamingMessage, Unit] = {
    case tweet: Tweet => {
      val emoji = filterEmoji(tweet.text)
      if (countReceived > 0) {
       if(emoji!="None" && emoji != "Many")
        receiver ! DocumentCategoryMessage(tweet.text, emoMap(emoji))
      }else context.stop(self);
      countReceived = countReceived - 1
    }
    case warning: WarningMessage => println("koniec bo blad " + warning.toString)
  }

  def receive = {
    case ("start", num: Int) => {
      countReceived = num
      client.filterStatuses(tracks = emotions, languages = List(Language.English))(sendTweetText)
    }

    case emoList : immutable.Map[String, String] => {
      emotions = emoList.keys.toList
      emoMap = emoList
      println("New emoji list set for streamer")
    }
  }

}
