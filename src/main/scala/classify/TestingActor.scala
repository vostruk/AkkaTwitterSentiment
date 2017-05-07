package classify

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


case object EvaluateModel
case object StartEvaluatingModel
case object StopEvaluatingModel
case object GetAccuracy


class TestingActor(testDataFileName: String, naiveBayesModelRouterActor: ActorRef) extends Actor {
  implicit val duration: Timeout = 100 seconds;
  var naiveBayesModelOption: Option[ActorRef] = None
  var evaluationRunning = false
  var evaluationRoundInProgress = false
  var accuracy = 0
  var correctDecisions = 0
  var receivedDecisions = 0

  val (testLabels, testDocuments) = {
    val source = scala.io.Source.fromFile(testDataFileName)
    val inputText = try source.mkString.toLowerCase finally source.close()
    val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2))
    lines.map((line) => (line(0), line(1))).toList.unzip
  }

  override def receive = {
    case StartEvaluatingModel =>
      evaluationRunning = true
      self ! EvaluateModel
    case StopEvaluatingModel =>
      evaluationRunning = false
    case EvaluateModel =>
      if (!evaluationRoundInProgress) {
        correctDecisions = 0
        receivedDecisions = 0
        evaluationRoundInProgress = true
        testDocuments.zip(testLabels).foreach((x) => naiveBayesModelRouterActor ! ClassifyDocumentMessage(x._1, self, x._2))
      }
      if (evaluationRunning) {
        Thread.sleep(1000)
        self ! EvaluateModel
      }
    case CategoryMessage(Some(category), trueCategory) if category == trueCategory.asInstanceOf[String] =>
      receivedDecisions += 1
      correctDecisions += 1
      if (receivedDecisions == testDocuments.size) {
        accuracy = correctDecisions / testDocuments.size
        evaluationRoundInProgress = false
      }
    case CategoryMessage(_, _) =>
      receivedDecisions += 1
      if (receivedDecisions == testDocuments.size) {
        accuracy = correctDecisions / testDocuments.size
        evaluationRoundInProgress = false
      }
    case GetAccuracy =>
      sender ! accuracy
  }
}
