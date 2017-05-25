package classify

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


case object EvaluateModel
case object StartEvaluatingModel
case object StopEvaluatingModel
case object GetAccuracy
case object GetQuality
case class SetTestDataFile(testDataFile: java.io.File)
case class ClassificationQuality(precision: mutable.Map[String, Double], recall: mutable.Map[String, Double])

class TestingActor(naiveBayesModelRouterActor: ActorRef) extends Actor {
  implicit val duration: Timeout = 100 seconds;
  var naiveBayesModelOption: Option[ActorRef] = None
  var evaluationRunning = false
  var evaluationRoundInProgress = false
  var accuracy: Double = 0
  var truePositives: mutable.Map[String, Double] = mutable.Map().withDefaultValue(0)
  var positiveResults: mutable.Map[String, Double] = mutable.Map().withDefaultValue(0)
  var positiveConditions: mutable.Map[String, Double] = mutable.Map().withDefaultValue(0)
  var qualityHolder = ClassificationQuality(mutable.Map().withDefaultValue(0), mutable.Map().withDefaultValue(0))
  var correctDecisions: Double = 0
  var receivedDecisions = 0
  var testLabels: List[String] = List[String]()
  var testDocuments: List[String] = List[String]()


  def refreshQuality(): Unit = {
    accuracy = correctDecisions / testDocuments.size
    val precision = mutable.Map[String, Double]()
    val recall = mutable.Map[String, Double]()
    for (category <- positiveResults.keys) {
      precision(category) = truePositives(category) / positiveResults(category)
    }
    for (category <- positiveConditions.keys) {
      recall(category) = truePositives(category) / positiveConditions(category)
    }
    qualityHolder = ClassificationQuality(precision, recall)
  }

  override def receive = {
    case SetTestDataFile(testDataFile) =>
      val source = scala.io.Source.fromFile(testDataFile.getAbsolutePath)
      val inputText = try source.mkString.toLowerCase finally source.close()
      val lines = inputText.toLowerCase.split("\n").map(_.split("\t", 2)).toList
      testLabels = lines.map(_(0))
      testDocuments = lines.map(_(1))

    case StartEvaluatingModel if testDocuments.nonEmpty =>
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
        context.system.scheduler.scheduleOnce(10 milliseconds, self, EvaluateModel)
      }
    case CategoryMessage(Some(category), trueCategory) if category == trueCategory.asInstanceOf[String] =>
      val trueCategoryString = trueCategory.asInstanceOf[String]
      truePositives(trueCategoryString) += 1
      positiveConditions(trueCategoryString) += 1
      positiveResults(trueCategoryString) += 1
      receivedDecisions += 1
      correctDecisions += 1
      if (receivedDecisions == testDocuments.size) {
        refreshQuality()
        evaluationRoundInProgress = false
      }
    case CategoryMessage(Some(category), trueCategory) =>
      positiveConditions(trueCategory.asInstanceOf[String]) += 1
      positiveResults(category) += 1
      receivedDecisions += 1
      if (receivedDecisions == testDocuments.size) {
        refreshQuality()
        evaluationRoundInProgress = false
      }
    case GetAccuracy =>
      sender ! accuracy.toString
    case GetQuality =>
      sender ! qualityHolder
  }
}
