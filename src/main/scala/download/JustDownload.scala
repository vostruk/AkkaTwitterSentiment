package download


import akka.actor.ActorSystem
import akka.actor.Props

import concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps
import classify.DocumentPreprocessor
import classify.CategoriesRepositoryActor
import classify.LaplaceSmoothingCategoryModel
import classify.NaiveBayesModelActor
import classify.NaiveBayesModel
import com.danielasfregola.twitter4s.entities.{AccessToken, ConsumerToken}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

//=============================================


object JustDownload extends App {

  val CKey = "9DZO2bQPgmXO4r2eML5yVE7tb";
  val CSecret = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW";
  val AToken = "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0";
  val ASecret = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x";

  val consumerToken = ConsumerToken( CKey, CSecret)
  val accessToken = AccessToken(AToken, ASecret)

  //=============================================

  val actorEmojiDefinition = "😀" :: "😯" :: "☹️" :: "😠" :: Nil

  val system = ActorSystem("DownloadSystem")

  val dp = new DocumentPreprocessor(2)
  val cr = system.actorOf(Props(new CategoriesRepositoryActor(() => new LaplaceSmoothingCategoryModel(0.5, dp))))
  val nbm1 = new NaiveBayesModel(dp, cr)
  val NbMActor = system.actorOf(Props(new NaiveBayesModelActor(nbm1)), name = "dpa1")

  val streamActor = system.actorOf(Props(new OnlineTweetStreamer(consumerToken, accessToken, NbMActor)), name = "streamActor")

  streamActor ! actorEmojiDefinition
  streamActor ! ("start", 20)


  val requestParserActor = system.actorOf(Props(new RequestParserActor(NbMActor)), name = "requestParserActor")
  val TweetDatesRangeDownloaderActor = system.actorOf(Props(new TweetDatesRangeDownloader(CKey, CSecret, AToken, ASecret, requestParserActor)), name = "DownloadActor")

  //print("Query: ");
  val q = "#brexit"// scala.io.StdIn.readLine()

  //"2017-03-31"
  //print("From (ex. 2017-04-20): ");
  val f = "2017-04-20"//scala.io.StdIn.readLine() //

  //"2017-04-01"
  //print("To (ex. 2017-04-24): ");
  val t = "2017-04-24"//scala.io.StdIn.readLine() //

  //tez odrazu wysyla message do parsera a parser do kategorizera (NBM)
  val timeoutFuture = Future( Await.result( Future(TweetDatesRangeDownloaderActor ! (q, f, t) ), 5.seconds))

  timeoutFuture.onComplete {
    case Success(t)  => print("we're good!!"); requestParserActor ! "ShowStatistics"
    case Failure(e) => println("@@!!timeout " + e.getMessage)
  }



}
