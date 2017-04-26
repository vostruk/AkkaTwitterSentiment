import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent._
import ExecutionContext.Implicits.global
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout


case object EvaluateModel


class TestingActor(testDataFileName: String, naiveBayesModel: ActorRef) extends Actor {
  implicit val duration: Timeout = 100 seconds;

  val (testLabels, testDocuments) = {
    val source = scala.io.Source.fromFile(testDataFileName)
    val inputText = try source.mkString.toLowerCase finally source.close()
    val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2))
    lines.map((line) => (line(0), line(1))).toList.unzip
  }

  override def receive = {
    case EvaluateModel =>
      val decisionsFuture = testDocuments.map((doc) => naiveBayesModel ? ClassifyDocumentMessage(doc)).map(_.mapTo[CategoryMessage])

      val decisions = Await.result(Future.sequence(decisionsFuture.map(_.map(_.category))), 100 seconds)
      val correctDecisions: Double = decisions.map(_.getOrElse("")).zip(testLabels).count((x) => x._1 == x._2)
      val accuracy = correctDecisions / decisions.size
      sender ! accuracy
  }
}
