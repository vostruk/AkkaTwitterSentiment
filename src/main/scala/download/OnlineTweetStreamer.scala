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

import scala.collection.mutable
import scala.collection.immutable

case class StartStreamingMessage(emoList : List[String], GEmoMap : immutable.Map[String, immutable.Set[String]])
case class AskAboutTweetsProcessedMessage()

class OnlineTweetStreamer(consumerToken: ConsumerToken, accessToken: AccessToken, receiver: ActorRef) extends Actor {

  var GlobalEmojiMap = immutable.Map("happiness" -> immutable.Set("😀"),  "surprise" -> immutable.Set("😯"), "sadness"  -> immutable.Set("☹️"),  "anger" ->  immutable.Set("😠"), "disgust" -> immutable.Set("\uD83D\uDE12") ,  "fear"  -> immutable.Set("\uD83D\uDE31"))

  var AgentEmotionsList : List[String] = "happiness" :: "surprise" :: "sadness" :: "anger" :: "disgust" :: "fear" :: Nil
  val client = TwitterStreamingClient(consumerToken, accessToken)
  var countReceived = 0
  var emojiListToStream: List[String] = Nil
  var actorOnHold = true

  def filterEmoji(t : String): String = {
    val globalemojiList : List[String] = GlobalEmojiMap.values.flatten.toList
    val emoCheck : mutable.Map[String, Int] = mutable.Map(globalemojiList map { s => (s, 0)} : _*)
    globalemojiList.foreach(elem => if(t.contains(elem)) emoCheck(elem) = emoCheck(elem)+1)

    var rv = "Many"
    val foundEmoji:List[String] = emoCheck.filter(t => t._2>0).keys.toList

    var reverseHelper =  mutable.Map[String, String]()
    GlobalEmojiMap.foreach(el => el._2.foreach(listel => reverseHelper(listel) = el._1))
    var foundEmotions = mutable.Map(GlobalEmojiMap.keys.toList map { s => (s, 0)} : _*)

    foundEmoji.foreach(elem => { foundEmotions(reverseHelper(elem)) = foundEmotions(reverseHelper(elem)) + 1 })

    foundEmotions.values.count(_>0) match {
      case 1 => for((e, c) <- foundEmotions) {if(c>0) rv = e }
      case 0 => rv = "None"
      case _ => rv = "Many"
    }
     rv
  }

  def sendTweetText: PartialFunction[StreamingMessage, Unit] = {
    case tweet: Tweet => {
      if (!actorOnHold) {
        val emoji = filterEmoji(tweet.text)
        if (emoji != "None" && emoji != "Many") {
          receiver ! DocumentCategoryMessage(tweet.text, emoji)
          countReceived = countReceived + 1
        }
      }
    }
    case warning: WarningMessage => println("koniec bo blad " + warning.toString)
  }

  def receive = {
    case StartStreamingMessage(emoList : List[String], emomap) => {
      AgentEmotionsList = emoList
      GlobalEmojiMap = emomap
      emoList.foreach(
        emo => {
          if(GlobalEmojiMap.contains(emo)) {
            emojiListToStream = emojiListToStream ::: GlobalEmojiMap(emo).toList
            AgentEmotionsList :+ emo
          }
        }
      )

      actorOnHold = false
      client.filterStatuses(tracks = emojiListToStream, languages = List(Language.English))(sendTweetText)

    }
    case AskAboutTweetsProcessedMessage() => sender ! countReceived

  }

}
