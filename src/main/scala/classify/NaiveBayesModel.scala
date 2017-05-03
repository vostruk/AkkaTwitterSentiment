package classify

import scala.collection.mutable
import scala.math.log
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._


case class DocumentCategoryMessage(document: String, category: String)
case class ClassifyDocumentMessage(document: String)
case class CategoryMessage(category: Option[String])


class NaiveBayesModelActor(catgoriesRepositoryActor: ActorRef) extends Actor {

  val categoryActors: mutable.Map[String, ActorRef] = mutable.Map[String, ActorRef]()
  val documentPreprocessor = new DocumentPreprocessor(2)

  override def receive = {
    case NewCategory(category, categoryActor) => categoryActors(category) = categoryActor
    case DocumentCategoryMessage(document, category) => addDocument(document, category)
    case ClearTrainedModel => categoryActors.clear()
    case ClassifyDocumentMessage(document) =>
      if (categoryActors.isEmpty) {
        sender ! CategoryMessage(None)
      }
      else {
        val category = classifyDocument(document)
        sender ! CategoryMessage(Some(category))
      }
  }

  def addDocument(document: String, category: String): Unit = {
    implicit val duration: Timeout = 20 seconds;
    if (!categoryActors.contains(category)) {
      val categoryAgentFuture = catgoriesRepositoryActor ? GetCategoryActor(category)
      categoryActors(category) = Await.result(categoryAgentFuture, 10 seconds).asInstanceOf[ActorRef]
    }
    categoryActors(category) ! AddDocumentMessage(document)
  }

  def classifyDocument(document: String): String = {
    implicit val duration: Timeout = 20 seconds;
    val allNGrams = documentPreprocessor.getTermsFromDocument(document)
    val documentsCount = categoryActors.mapValues((actor) => Await.result(actor ? GetDocumentsCount, 10 seconds).asInstanceOf[Int])
    val allDocumentsCount: Double = documentsCount.values.sum
    val ngramLikelihoods: List[(String, Double)] = for {
      (category, categoryActor) <- categoryActors.toList
      ngram <- allNGrams
      ngramProbability = Await.result(categoryActor ? GetNGramProbablityMessage(ngram), 10 seconds).asInstanceOf[Double]
    } yield (category, log(ngramProbability))
    val scoredCategories = ngramLikelihoods.groupBy(_._1).mapValues(_.map(_._2).sum).map((keyVal) => (keyVal._1, keyVal._2 + log(documentsCount(keyVal._1) / allDocumentsCount)))
    scoredCategories.maxBy(_._2)._1
  }
}
